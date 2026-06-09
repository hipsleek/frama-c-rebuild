(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype


module Tbl =
  Cil_state_builder.Kinstr_hashtbl
    (Eva.Assigns)
    (struct
      let name = "Callwise dependencies"
      let size = 17
      let dependencies = [ Eva.Analysis.self ]
    end)

let self = Tbl.self

let merge_call_froms table callsite froms =
  try
    let current = Kinstr.Hashtbl.find table callsite in
    let new_froms = Eva.Assigns.join froms current in
    Kinstr.Hashtbl.replace table callsite new_froms
  with Not_found ->
    Kinstr.Hashtbl.add table callsite froms

(** State for the analysis of one function call *)
type from_state = {
  current_function: Kernel_function.t (** Function being analyzed *);
  table_for_calls: Eva.Assigns.t Kinstr.Hashtbl.t
(** State of the From plugin for each statement containing a function call
    in the body of [current_function]. Updated incrementally each time
    Value analyses such a statement *);
}

(** The state of the callwise From analysis. Only the top of this callstack
    is accessed. New calls are pushed on the stack when Value starts the
    analysis of a function, and popped when the analysis finishes. This
    stack is manually synchronized with Value's callstack. *)
let call_froms_stack : from_state list ref = ref []

let reset () =
  call_froms_stack := []

let record_callwise_dependencies_in_db call_site froms =
  try
    let previous = Tbl.find call_site in
    Tbl.replace call_site (Eva.Assigns.join previous froms)
  with Not_found -> Tbl.add call_site froms

let call_for_individual_froms _callstack current_function _state call_type =
  if From_parameters.ForceCallDeps.get () then begin
    match call_type with
    | `Body ->
      let table_for_calls = Kinstr.Hashtbl.create 7 in
      call_froms_stack :=
        { current_function; table_for_calls } :: !call_froms_stack
    | `Reuse | `Builtin | `Spec -> ()
  end

let pop_local_table kf =
  match !call_froms_stack with
  | { current_function } :: tail ->
    if kf != current_function then
      From_parameters.fatal "calldeps %a != %a@."
        Kernel_function.pretty current_function Kernel_function.pretty kf;
    call_froms_stack := tail
  | _ -> From_parameters.fatal "calldeps: internal stack is empty"

let end_record callstack froms =
  let callsite = Eva.Callstack.top_callsite callstack in
  record_callwise_dependencies_in_db callsite froms;
  match callsite, !call_froms_stack with
  | Kstmt _, { table_for_calls } :: _ ->
    merge_call_froms table_for_calls callsite froms
  | Kglobal, [] ->  (* the entry point *)
    Tbl.mark_as_computed ()
  | _ ->
    From_parameters.fatal
      "calldeps: internal stack is inconsistent with Eva callstack"


module MemExec =
  State_builder.Hashtbl
    (Datatype.Int.Hashtbl)
    (Eva.Assigns)
    (struct
      let size = 17
      let dependencies = [Tbl.self]
      let name = "From.Callwise.MemExec"
    end)

let compute_call_from_value_states current_function states =
  let module To_Use = struct
    let get_from_call _f callsite =
      let { table_for_calls } = List.hd !call_froms_stack in
      try Kinstr.Hashtbl.find table_for_calls (Cil_types.Kstmt callsite)
      with Not_found -> raise From_compute.Call_did_not_take_place

    let stmt_request stmt =
      Eva.Results.in_cvalue_state
        (try Stmt.Hashtbl.find states stmt
         with Not_found -> Cvalue.Model.bottom)

    let keep_base kf base =
      let fundec = Kernel_function.get_definition kf in
      not (Base.is_formal_or_local base fundec)

    let cleanup_and_save _kf froms = froms
  end
  in
  let module Callwise_Froms = From_compute.Make(To_Use) in
  Callwise_Froms.compute_and_return current_function


let record_for_individual_froms callstack kf pre_state value_res =
  if From_parameters.ForceCallDeps.get () then begin
    let froms =
      match value_res with
      | `Body (Eva.Cvalue_callbacks.{before_stmts}, memexec_counter) ->
        let froms =
          if Eva.Analysis.save_results kf
          then compute_call_from_value_states kf (Lazy.force before_stmts)
          else Eva.Assigns.top
        in
        if From_parameters.VerifyAssigns.get () then
          Eva.Logic_inout.verify_assigns kf ~pre:pre_state froms;
        MemExec.replace memexec_counter froms;
        pop_local_table kf;
        froms
      | `Reuse counter -> MemExec.find counter
      | `Builtin (_states, Some (result,_)) -> result
      | `Builtin (_states, None)
      | `Spec _states ->
        let bhv = Eva.Logic_inout.valid_behaviors kf pre_state in
        let assigns = Ast_info.merge_assigns bhv in
        From_compute.compute_using_prototype_for_state
          pre_state kf assigns
    in
    end_record callstack froms
  end


(* Register our callbacks inside the value analysis *)
let () =
  Eva.Cvalue_callbacks.register_at_start_hook reset;
  Eva.Cvalue_callbacks.register_call_hook call_for_individual_froms;
  Eva.Cvalue_callbacks.register_call_results_hook record_for_individual_froms

let iter = Tbl.iter
let find = Tbl.find

let compute_all_calldeps () =
  if not (Tbl.is_computed ()) then begin
    if Eva.Analysis.is_computed () then
      Project.clear
        ~selection:(State_selection.with_dependencies Eva.Analysis.self)
        ();
    Eva.Analysis.compute ()
  end
