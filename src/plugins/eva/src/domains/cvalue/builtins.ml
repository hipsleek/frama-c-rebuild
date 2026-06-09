(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

exception Invalid_nb_of_args of int
exception Outside_builtin_possibilities

type builtin_type = unit -> Eva_ast.typ * Eva_ast.typ list
type cacheable = Eval.cacheable = Cacheable | NoCache | NoCacheCallers

type full_result = {
  c_values: (Cvalue.V.t option * Cvalue.Model.t) list;
  c_clobbered: Base.SetLattice.t;
  c_assigns: (Assigns.t * Memory_zone.t) option;
  cacheable: cacheable;
}

type call_result =
  | States of Cvalue.Model.t list
  | Result of Cvalue.V.t list
  | Full of full_result

type builtin = Cvalue.Model.t -> (Eva_ast.exp * Cvalue.V.t) list -> call_result

(* Table of all registered builtins; filled by [register_builtin] calls.  *)
let table = Hashtbl.create 17

(* Table binding each kernel function to their builtin for a given analysis.
   Filled at the beginning of each analysis by [prepare_builtins]. *)
let builtins_table = Hashtbl.create 17

module Info = struct
  let name = "Eva.Builtins.BuiltinsOverride"
  let dependencies = [ Self.state ]
end

(** Set of functions overridden by a builtin. *)
module BuiltinsOverride = State_builder.Set_ref (Kernel_function.Set) (Info)

let register_builtin name ?replace ?typ f =
  Parameters.register_builtin name;
  let builtin = (name, f, typ) in
  Hashtbl.replace table name builtin;
  match replace with
  | None -> ()
  | Some fname -> Hashtbl.replace table fname builtin

let unregister_builtin name =
  Parameters.unregister_builtin name;
  Hashtbl.remove table name

let is_builtin name =
  try
    let bname, _, _ = Hashtbl.find table name in
    name = bname
  with Not_found -> false

let builtin_names_and_replacements () =
  let stand_alone, replacements =
    Hashtbl.fold
      (fun name (builtin_name, _, _) (acc1, acc2) ->
         if name = builtin_name
         then name :: acc1, acc2
         else acc1, (name, builtin_name) :: acc2)
      table ([], [])
  in
  List.sort String.compare stand_alone,
  List.sort (fun (name1, _) (name2, _) -> String.compare name1 name2) replacements

let print_builtin_list fmt =
  let stand_alone, replacements = builtin_names_and_replacements () in
  let pp_replacement fmt (name, rep_by) =
    if rep_by = "Frama_C_" ^ name
    then Format.fprintf fmt "%s" name
    else Format.fprintf fmt "%s (replaced by: %s)" name rep_by
  in
  let pp_list = Pretty_utils.pp_list ~sep:",@ " in
  Format.fprintf fmt
    "@;@[<v 3>** Automatic replacements:@;\
     (unless otherwise specified, \
     function <f> is replaced by builtin Frama_C_<f>)@;@;%a@]@;@;"
    (pp_list pp_replacement) replacements;
  Format.fprintf fmt
    "@[<v 3>** Full list of builtins (configurable via -eva-builtin):@;@;%a@]"
    (pp_list Format.pp_print_string) stand_alone

let print_builtins_and_exit () =
  let header fmt = Format.fprintf fmt "List of Eva builtins:" in
  Self.printf ~header "@[<v>%t@]" print_builtin_list;
  raise Cmdline.Exit

let () =
  Cmdline.run_after_configuring_stage
    (fun () -> if Parameters.BuiltinsList.get () then print_builtins_and_exit ())

(* -------------------------------------------------------------------------- *)
(* --- Prepare builtins for an analysis                                   --- *)
(* -------------------------------------------------------------------------- *)

let warn_incompatible_type ~source name kf =
  let kf_typ = Kernel_function.get_type kf in
  Self.warning ~wkey:Self.wkey_builtins_override ~source ~once:true
    "Builtin %s will not be used for function %a of incompatible type %a."
    name Kernel_function.pretty kf Printer.pp_typ kf_typ

let warn_no_specification ~source kf =
  Self.warning ~wkey:Self.wkey_builtins_missing_spec ~source ~once:true
    "The builtin for function %a will not be used, as its frama-c libc \
     specification is not available."
    Kernel_function.pretty kf

let warn_no_default_behavior ~source kf =
  Self.warning ~wkey:Self.wkey_builtins_missing_spec ~source ~once:true
    "The builtin for function %a will not be used, as its specification \
     has no default behavior."
    Kernel_function.pretty kf

let warn_no_assigns ~source kf =
  Self.warning ~wkey:Self.wkey_builtins_missing_spec ~source ~once:true
    "The builtin for function %a will not be used, as its specification has \
     no assigns clause."
    Kernel_function.pretty kf

let warn_user_specification ~source kf =
  Self.warning ~wkey:Self.wkey_builtins_missing_spec ~source ~once:true
    "No Frama-C libc specification found for function %a, for which a \
     builtin is used; its soundness relies on the specification provided \
     by the user."
    Kernel_function.pretty kf

let warn_builtin_override ~source kf bname =
  let fname = Kernel_function.get_name kf in
  Self.warning ~wkey:Self.wkey_builtins_override ~source ~once:true
    "Definition of function %s is overridden by %s"
    fname (if fname = bname then "its builtin" else "builtin " ^ bname)

let is_frama_c_builtin kf =
  let vi = Kernel_function.get_vi kf in
  Ast_info.start_with_frama_c vi.vname || Cil_builtins.has_fc_builtin_attr vi

type spec = Spec of Cil_types.spec | NoSpec | NoAssigns | NoDefaultBehavior

(* Returns the specification of a builtin, required to evaluate preconditions
   and to transfer the states of other domains. *)
let find_builtin_specification kf =
  (* Functions for which a builtin is used should have a specification, except
     Frama_C_* builtins such as Frama_C_assert, for which we generate an empty
     specification with assigns clauses. *)
  if is_frama_c_builtin kf
  then Populate_spec.populate_funspec kf [`Assigns];
  let spec = Annotations.funspec kf in
  match Cil.find_default_behavior spec with
  | None -> if spec.spec_behavior = [] then NoSpec else NoDefaultBehavior
  | Some bhv -> if bhv.b_assigns = WritesAny then NoAssigns else Spec spec

(* Returns [true] if the function [kf] is incompatible with the expected type
   for a given builtin, which therefore cannot be applied. *)
let inconsistent_builtin_typ kf = function
  | None -> false (* No expected type provided with the builtin, no check. *)
  | Some typ ->
    let expected_result, expected_args = typ () in
    match (Kernel_function.get_type kf).tnode with
    | TFun (result, args, _) ->
      (* If a builtin expects a void pointer, then accept all pointer types. *)
      let need_cast typ expected =
        Cil.need_cast typ expected
        && not Ast_types.(is_void_ptr expected && is_ptr typ)
      in
      let args = Cil.argsToList args in
      need_cast result expected_result
      || List.length args <> List.length expected_args
      || List.exists2 (fun (_, t, _) u -> need_cast t u) args expected_args
    | _ -> assert false

let prepare_builtin kf (name, builtin, expected_typ) =
  let source = fst (Kernel_function.get_location kf) in
  if inconsistent_builtin_typ kf expected_typ
  then warn_incompatible_type ~source name kf
  else
    match find_builtin_specification kf with
    | NoSpec -> warn_no_specification ~source kf
    | NoDefaultBehavior -> warn_no_default_behavior ~source kf
    | NoAssigns -> warn_no_assigns ~source kf
    | Spec spec ->
      BuiltinsOverride.add kf;
      Hashtbl.replace builtins_table kf (name, builtin, spec)

let prepare_builtins () =
  BuiltinsOverride.clear ();
  Hashtbl.clear builtins_table;
  let autobuiltins = Parameters.BuiltinsAuto.get () in
  (* Links kernel functions to the registered builtins. *)
  Hashtbl.iter
    (fun name (bname, f, typ) ->
       if autobuiltins || name = bname
       then
         try
           let kf = Globals.Functions.find_by_name name in
           prepare_builtin kf (name, f, typ)
         with Not_found -> ())
    table;
  (* Overrides builtins attribution according to the -eva-builtin option. *)
  Parameters.BuiltinsOverrides.iter
    (fun (kf, name) -> prepare_builtin kf (Hashtbl.find table name));
  BuiltinsOverride.mark_as_computed ()

(* Emits warning if builtin [name] overrides function definition [kf], or if
   the Frama-C specification of [kf] is missing. *)
let check_builtin kf (name, _, _) =
  let source = fst (Kernel_function.get_location kf) in
  if not (Kernel_function.is_in_libc kf || is_frama_c_builtin kf)
  then warn_user_specification ~source kf;
  let is_internal = Filepath.is_relative ~base:System_config.Share.libc in
  if Kernel_function.is_definition kf && not (is_internal (Filepos.path source))
  then warn_builtin_override ~source kf name

let find_builtin_override kf =
  let builtin = Hashtbl.find_opt builtins_table kf in
  Option.iter (check_builtin kf) builtin;
  builtin

let is_builtin_overridden kf =
  if not (BuiltinsOverride.is_computed ())
  then prepare_builtins ();
  BuiltinsOverride.mem kf

(* -------------------------------------------------------------------------- *)
(* --- Applying a builtin                                                 --- *)
(* -------------------------------------------------------------------------- *)

let clobbered_set_from_ret state ret =
  let aux b _ acc =
    match Cvalue.Model.find_base_or_default b state with
    | `Top -> Base.SetLattice.top
    | `Bottom -> acc
    | `Value m ->
      if Locals_scoping.offsetmap_contains_local m then
        Base.SetLattice.(join (inject_singleton b) acc)
      else acc
  in
  try Cvalue.V.fold_topset_ok aux ret Base.SetLattice.bottom
  with Abstract_interp.Error_Top -> Base.SetLattice.top

type call = (Precise_locs.precise_location, Cvalue.V.t) Eval.call
type result = Cvalue.Model.t * Locals_scoping.clobbered_set

open Eval

let compute_arguments arguments rest =
  let compute assigned =
    match Eval.value_assigned assigned with
    | `Bottom -> Cvalue.V.bottom
    | `Value v -> v
  in
  let list = List.map (fun arg -> arg.concrete, compute arg.avalue) arguments in
  let rest = List.map (fun (exp, v) -> exp, compute v) rest in
  list @ rest

let process_result call state call_result =
  let clob = Locals_scoping.bottom () in
  let bind_result state return =
    match return, call.return with
    | Some value, Some vi_ret ->
      let b_ret = Base.of_varinfo vi_ret in
      let offsm = Eval_op.offsetmap_of_v ~typ:vi_ret.vtype value in
      let prefix = "Builtin " ^ Kernel_function.get_name call.kf in
      let lval_ret = Eva_ast.Build.var vi_ret in
      Cvalue_transfer.warn_imprecise_offsm_write ~prefix lval_ret offsm;
      Cvalue.Model.add_base b_ret offsm state, clob
    | _, _ -> state, clob (* TODO: error? *)
  in
  match call_result with
  | States states -> List.rev_map (fun s -> s, clob) states
  | Result values -> List.rev_map (fun v -> bind_result state (Some v)) values
  | Full result ->
    Locals_scoping.remember_bases_with_locals clob result.c_clobbered;
    let process_one_return acc (return, state) =
      if Cvalue.Model.is_reachable state
      then bind_result state return :: acc
      else acc
    in
    List.fold_left process_one_return [] result.c_values

let apply_builtin (builtin:builtin) call ~pre ~post =
  let arguments = compute_arguments call.arguments call.rest in
  try
    let call_result = builtin pre arguments in
    let states = process_result call post call_result in
    let froms, cacheable =
      match call_result with
      | Full result -> result.c_assigns, result.cacheable
      | States _ | Result _ -> None, Cacheable
    in
    let result = `Builtin (List.map fst states, froms) in
    Cvalue_callbacks.apply_call_results_hooks call.callstack call.kf pre result;
    states, cacheable
  with
  | Invalid_nb_of_args n ->
    Self.abort ~current:true
      "Invalid number of arguments for builtin %a: %d expected, %d found"
      Kernel_function.pretty call.kf n (List.length arguments)
  | Outside_builtin_possibilities ->
    Self.fatal ~current:true
      "Call to builtin %a failed" Kernel_function.pretty call.kf
