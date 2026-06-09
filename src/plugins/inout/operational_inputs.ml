(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* Computation of over-approximated operational inputs:
   An accurate computation of these inputs needs the computation of
   under-approximated outputs.
*)

type t = Inout_type.t = {
  over_inputs: Memory_zone.t;
  over_inputs_if_termination: Memory_zone.t;
  over_logic_inputs: Memory_zone.t;
  (* [over_logic_inputs] is used internally by Eva to make memexec consider also
     the logic inputs of the function. Computed in [transfer_annotations]. *)
  under_outputs_if_termination: Memory_zone.t;
  over_outputs: Memory_zone.t;
  over_outputs_if_termination: Memory_zone.t;
}

let top = {
  over_inputs = Memory_zone.top;
  over_inputs_if_termination = Memory_zone.top;
  over_logic_inputs = Memory_zone.top;
  under_outputs_if_termination = Memory_zone.bottom;
  over_outputs = Memory_zone.top;
  over_outputs_if_termination = Memory_zone.top;
}

(* [_if_termination] fields of the type above, which are the one propagated by
   the dataflow analysis of this module. It is meaningless to store the other
   ones, as they come from branches that are by construction not propagated
   until the end by the dataflow. *)
type compute_t = {
  over_inputs_d : Memory_zone.t ;
  under_outputs_d : Memory_zone.t;
  over_outputs_d: Memory_zone.t;
}

(* Initial value for the computation *)
let empty = {
  over_inputs_d = Memory_zone.bottom;
  under_outputs_d = Memory_zone.bottom;
  over_outputs_d = Memory_zone.bottom;
}

let bottom = {
  over_inputs_d = Memory_zone.bottom;
  under_outputs_d = Memory_zone.top;
  over_outputs_d = Memory_zone.bottom;
}

let equal ct1 ct2 =
  Memory_zone.equal ct1.over_inputs_d ct2.over_inputs_d &&
  Memory_zone.equal ct1.under_outputs_d ct2.under_outputs_d &&
  Memory_zone.equal ct1.over_outputs_d ct2.over_outputs_d

let join c1 c2 = {
  over_inputs_d = Memory_zone.join c1.over_inputs_d c2.over_inputs_d;
  under_outputs_d = Memory_zone.meet c1.under_outputs_d c2.under_outputs_d;
  over_outputs_d = Memory_zone.join c1.over_outputs_d c2.over_outputs_d;
}

let is_included c1 c2 =
  Memory_zone.is_included c1.over_inputs_d c2.over_inputs_d &&
  Memory_zone.is_included c2.under_outputs_d c1.under_outputs_d &&
  Memory_zone.is_included c1.over_outputs_d c2.over_outputs_d

let join_and_is_included smaller larger =
  let join = join smaller larger in
  join, equal join larger
;;

let externalize_zone ~with_formals kf =
  Memory_zone.filter_base
    (Eva.Logic_inout.accept_base ~formals:with_formals ~locals:false kf)

(* This code evaluates an assigns, computing in particular a sound approximation
   of sure outputs. For an assigns [locs_out \from locs_from], the process
   is the following:
   - evaluate locs_out to locations; discard those that are not exact, as
     we cannot guarantee that they are always assigned
   - evaluate locs_from, as a zone (no need for locations)
   - compute the difference between the out and the froms, ie remove the
     zones that are such that [z \from z] holds

   (Note: large parts of this code are inspired/redundant with
   [assigns_to_zone_foobar_state] in Value/register.ml)
*)
let eval_assigns kf state assigns =
  let treat_one_zone acc (out, froms as asgn) = (* treat a single assign *)
    (* Return a list of independent output zones, plus a zone indicating
       that the zone has been overwritten in a sure way *)
    let clean_deps =
      Memory_zone.filter_base
        (function
          | Base.Var (v, _) | Base.Allocated (v, _, _) ->
            not (Kernel_function.is_formal v kf)
          | Base.CLogic_Var _ | Base.Null -> true)
    in
    let out_term = out.it_content in
    let outputs_under, outputs_over, deps =
      if Logic_const.(is_result out_term || is_exit_status out_term)
      then (Memory_zone.bottom, Memory_zone.bottom, Memory_zone.bottom)
      else
        let output =
          Eva.Logic_inout.tlval_to_zones Assigns state Write out_term
        in
        match output with
        | Some output -> output.under, output.over, clean_deps output.deps
        | None ->
          Inout_parameters.warning ~current:true ~once:true
            "failed to interpret assigns clause '%a'" Printer.pp_term out_term;
          (Memory_zone.bottom, Memory_zone.top, Memory_zone.top)
    in
    (* Compute all inputs as a zone *)
    let inputs =
      match froms with
      | FromAny -> Memory_zone.top
      | From l ->
        let aux acc { it_content = from } =
          let inputs = Eva.Logic_inout.tlval_to_zones Assigns state Read from in
          match inputs with
          | Some inputs ->
            let acc = Memory_zone.join (clean_deps inputs.deps) acc in
            Memory_zone.join inputs.over acc
          | _ ->
            Inout_parameters.warning ~current:true ~once:true
              "failed to interpret inputs in assigns clause '%a'"
              Printer.pp_from asgn;
            Memory_zone.top
        in
        List.fold_left aux deps l
    in
    (* Fuse all outputs. An output is sure if it was certainly
       overwritten (i.e. is in the left part of an assign clause,
       and if it is not amongst its from.) *)
    (* Note: here we remove an overapproximation from an
       underapproximation to get an underapproximation, which is not
       the usual direction. It works here because diff on non-top zones is
       an exact operation. *)
    let sure_out =
      Memory_zone.(if equal top inputs then bottom else diff outputs_under inputs)
    in
    {
      under_outputs_d = Memory_zone.link acc.under_outputs_d sure_out;
      over_inputs_d = Memory_zone.join acc.over_inputs_d inputs;
      over_outputs_d = Memory_zone.join acc.over_outputs_d outputs_over;
    }
  in
  match assigns with
  | WritesAny ->
    Inout_parameters.warning "@[no assigns clauses for@ function %a.@]@ \
                              Results will be imprecise."
      Kernel_function.pretty kf;
    top
  | Writes l  ->
    let init = { bottom with under_outputs_d = Memory_zone.bottom } in
    let r = List.fold_left treat_one_zone init l in {
      over_inputs = r.over_inputs_d;
      over_logic_inputs = r.over_inputs_d;
      over_inputs_if_termination = r.over_inputs_d;
      under_outputs_if_termination = r.under_outputs_d;
      over_outputs = r.over_outputs_d;
      over_outputs_if_termination = r.over_outputs_d;
    }

let compute_using_spec state kf =
  let behaviors = Eva.Logic_inout.valid_behaviors kf state in
  let assigns = Ast_info.merge_assigns behaviors in
  eval_assigns kf state assigns

let compute_using_prototype ?stmt kf =
  let state = Cumulative_analysis.specialize_state_on_call ?stmt kf in
  compute_using_spec state kf

(* Results of this module, consolidated by functions. Formals and locals
   are stored *)
module Internals =
  Kernel_function.Make_Table(Inout_type)
    (struct
      let name = "Inout.Operational_inputs.Internals"
      let dependencies = [ Eva.Analysis.self ]
      let size = 17
    end)

module Callsite =
  Datatype.Pair_with_collections (Kernel_function) (Cil_datatype.Kinstr)
module CallsiteHash = Callsite.Hashtbl

(* Results of an an entire call, represented by a pair (stmt, kernel_function).
*)
module CallwiseResults =
  State_builder.Hashtbl
    (Callsite.Hashtbl)
    (Inout_type)
    (struct
      let size = 17
      let dependencies = [Internals.self]
      let name = "Inout.Operational_inputs.CallwiseResults"
    end)

module Computer(Fenv:Dataflows.FUNCTION_ENV)(X:sig
    val _version: string (* Debug: Callwise or functionwise *)
    val _kf: kernel_function (* Debug: Function being analyzed *)
    val kf_pre_state: Cvalue.Model.t (* Memory pre-state of the function. *)
    val stmt_state: stmt -> Cvalue.Model.t (* Memory state at the given stmt *)
    val stmt_request: stmt -> Eva.Results.request (* Request at the given stmt *)
    val at_call: stmt -> kernel_function -> Inout_type.t (* Results of the
                                                            analysis for the given call. Must not contain locals or formals *)
  end) = struct

  (* We want to compute the in/out for all terminating and
     non-terminating points of the function. This is not immediate
     with a dataflow, as all (1) infinite loops, (2) branches that call a non
     terminating function, or (3) branches that fail, will not appear in the
     final state. Hence, we two use auxiliary variables into which we add
     all partial results. *)
  let non_terminating_inputs = ref Memory_zone.bottom
  let non_terminating_outputs = ref Memory_zone.bottom
  let non_terminating_logic_inputs = ref Memory_zone.bottom

  let store_non_terminating_inputs inputs =
    non_terminating_inputs := Memory_zone.join !non_terminating_inputs inputs;
  ;;

  let store_non_terminating_logic_inputs logic_inputs =
    non_terminating_logic_inputs :=
      Memory_zone.join !non_terminating_logic_inputs logic_inputs

  let store_non_terminating_outputs outputs =
    non_terminating_outputs := Memory_zone.join !non_terminating_outputs outputs;
  ;;

  (* Store the 'non-termination' information of a function subcall into
     the current call. [under_outputs] are the current call sure outputs. *)
  let store_non_terminating_subcall under_outputs subcall =
    store_non_terminating_inputs (Memory_zone.diff subcall.over_inputs under_outputs);
    store_non_terminating_outputs subcall.over_outputs;
  ;;

  let catenate c1 c2 =
    let inputs = Memory_zone.diff c2.over_inputs_d c1.under_outputs_d in
    store_non_terminating_inputs inputs;
    { over_inputs_d = Memory_zone.join c1.over_inputs_d inputs;
      under_outputs_d = Memory_zone.link c1.under_outputs_d c2.under_outputs_d;
      over_outputs_d = Memory_zone.join  c1.over_outputs_d c2.over_outputs_d;
    }

  type t = compute_t

  let pretty fmt x =
    Format.fprintf fmt
      "@[Over-approximated operational inputs: %a@]@\n\
       @[Under-approximated operational outputs: %a@]"
      Memory_zone.pretty x.over_inputs_d
      Memory_zone.pretty x.under_outputs_d

  let bottom = bottom
  let join_and_is_included = join_and_is_included
  let join = join
  let is_included = is_included

  (* Transfer function on expression. *)
  let transfer_exp s exp data =
    let request = X.stmt_request s in
    let inputs = Eva.Results.expr_deps exp request in
    let new_inputs = Memory_zone.diff inputs data.under_outputs_d in
    store_non_terminating_inputs new_inputs;
    {data with over_inputs_d = Memory_zone.join data.over_inputs_d new_inputs}

  (* Initialized const variables should be included as outputs of the function,
     so [for_writing] must be false for local initializations. It should be
     true for all other instructions. *)
  let add_out ~for_writing request lv deps data =
    let lv_address = Eva.Results.eval_address ~for_writing lv request in
    let new_outs = Eva.Results.as_zone lv_address in
    let exact = Eva.Results.is_singleton lv_address in
    store_non_terminating_outputs new_outs;
    let lv_deps = Eva.Results.address_deps lv request in
    let deps = Memory_zone.join lv_deps deps in
    let new_inputs = Memory_zone.diff deps data.under_outputs_d in
    store_non_terminating_inputs new_inputs;
    let new_sure_outs =
      if exact then
        (* There is only one modified zone. So, this is an exact output.
           Add it into the under-approximated outputs. *)
        Memory_zone.link data.under_outputs_d new_outs
      else data.under_outputs_d
    in
    { under_outputs_d = new_sure_outs;
      over_inputs_d = Memory_zone.join data.over_inputs_d new_inputs;
      over_outputs_d = Memory_zone.join data.over_outputs_d new_outs }

  let transfer_call ~for_writing s dest f args _loc data =
    let request = X.stmt_request s in
    (* Join the inputs of [args] and of the function expression. *)
    let f_inputs = Eva.Results.lval_deps (f,NoOffset) request in
    let eval_deps acc e = Memory_zone.join acc (Eva.Results.expr_deps e request) in
    let f_args_inputs = List.fold_left eval_deps f_inputs args in
    let data =
      catenate
        data
        { over_inputs_d = f_args_inputs ;
          under_outputs_d = Memory_zone.bottom;
          over_outputs_d = Memory_zone.bottom; }
    in
    let called = Eva.Results.(eval_callee f request |> default []) in
    let for_functions =
      List.fold_left
        (fun acc kf ->
           let res = X.at_call s kf in
           store_non_terminating_subcall data.over_outputs_d res;
           let for_function = {
             over_inputs_d = res.over_inputs_if_termination;
             under_outputs_d = res.under_outputs_if_termination;
             over_outputs_d = res.over_outputs_if_termination;
           } in
           join for_function acc)
        bottom
        called
    in
    let result = catenate data for_functions in
    let result =
      (* Treatment for the possible assignment of the call result *)
      (match dest with
       | None -> result
       | Some lv -> add_out ~for_writing request lv Memory_zone.bottom result)
    in result

  (* Propagate all zones in predicates for the given statement, only in the case
     of assertions and loop-invariants. For the time being, we do not treat
     terminating and non-terminating points of the function differently. *)
  let transfer_annotations stmt =
    Annotations.iter_code_annot
      (fun _ ca ->
         match ca.annot_content with
         | AAssert (_, p)
         | AInvariant (_, true, p) ->
           begin
             let pre = X.kf_pre_state
             and here = X.stmt_state stmt in
             let deps =
               Eva.Logic_inout.predicate_deps ~pre ~here p.tp_statement
             in
             match deps with
             | None ->
               (* To be sound, we should perform a join with the top zone here.
                  We do nothing instead because the latter behavior would
                  directly disable memexec. *)
               ()
             | Some p_zone -> store_non_terminating_logic_inputs p_zone
           end
         | _ -> ())
      stmt

  (* Transfer function on instructions. *)
  let transfer_instr stmt (i: instr) (data: t) =
    match i with
    | Set (lv, exp, _) ->
      let request = X.stmt_request stmt in
      let e_inputs = Eva.Results.expr_deps exp request in
      add_out ~for_writing:true request lv e_inputs data
    | Local_init (v, AssignInit i, _) ->
      let request = X.stmt_request stmt in
      let rec aux lv i acc =
        match i with
        | SingleInit e ->
          let e_inputs = Eva.Results.expr_deps e request in
          add_out ~for_writing:false request lv e_inputs acc
        | CompoundInit(ct, initl) ->
          (* Avoid folding implicit zero-initializer of large arrays. *)
          let implicit = Cumulative_analysis.fold_implicit_initializer ct in
          let doinit o i _ data = aux (Cil.addOffsetLval o lv) i data in
          let data = Cil.foldLeftCompound ~implicit ~doinit ~ct ~initl ~acc in
          if implicit then data else
            (* If the implicit zero-initializers hade been skipped, add the
               zone of the array as outputs. It is exactly the written zone for
               arrays of scalar elements. Nothing is read by zero-initializers,
               so the inputs are empty. *)
            add_out ~for_writing:false request lv Memory_zone.bottom acc
      in
      aux (Cil.var v) i data
    | Call (lvaloption,funclv,argl,loc) ->
      transfer_call ~for_writing:true stmt lvaloption funclv argl loc data
    | Local_init(v, ConsInit(f, args, kind), loc) ->
      let transfer = transfer_call ~for_writing:false stmt in
      Cil.treat_constructor_as_func transfer v f args kind loc data
    | Asm _ | Code_annot _ | Skip _ -> data
  ;;

  (* transfer_guard: gets the state obtained after evaluating the
     condition, and split the state according to the truth value of
     the condition. In this case, we just make sure that dead
     edges get bottom, instead of the input state. *)
  let transfer_guard stmt e t =
    let request = X.stmt_request stmt in
    let v_e = Eva.Results.(eval_exp e request |> as_cvalue) in
    let t1 = Ast_types.unroll (Cil.typeOf e) in
    let do_then, do_else =
      if Ast_types.is_integral t1 || Ast_types.is_ptr t1
      then Cvalue.V.contains_non_zero v_e,
           Cvalue.V.contains_zero v_e
      else true, true (* TODO: a float condition is true iff != 0.0 *)
    in
    (if do_then then t else bottom),
    (if do_else then t else bottom)
  ;;

  let return_data = ref bottom;;

  let transfer_stmt s data =
    let map_on_all_succs new_data = List.map (fun x -> (x,new_data)) s.succs in
    match s.skind with
    | Instr i -> map_on_all_succs (transfer_instr s i data)

    | If(exp,_,_,_) ->
      let data = transfer_exp s exp data in
      Dataflows.transfer_if_from_guard transfer_guard s data
    | Switch(exp,_,_,_) ->
      let data = transfer_exp s exp data in
      Dataflows.transfer_switch_from_guard transfer_guard s data

    | Return(Some exp,_) -> return_data := transfer_exp s exp data;
      assert (s.succs == []); []
    | Return(None,_) -> return_data := data;
      assert (s.succs == []); []
    | Throw _ | TryCatch _ ->
      Inout_parameters.fatal "Exception node in the AST"
    | UnspecifiedSequence _ | Loop _ | Block _
    | Goto _ | Break _ | Continue _
    | TryExcept _ | TryFinally _
      -> map_on_all_succs data
  ;;

  let transfer_stmt s data =
    if Cvalue.Model.is_reachable (X.stmt_state s)
    then begin
      transfer_annotations s;
      transfer_stmt s data
    end
    else []
  ;;

  let init = [(Kernel_function.find_first_stmt Fenv.kf), empty];;

  let end_dataflow () =
    let res_if_termination = !return_data in {
      over_inputs_if_termination = res_if_termination.over_inputs_d;
      under_outputs_if_termination = res_if_termination.under_outputs_d ;
      over_outputs_if_termination = res_if_termination.over_outputs_d;
      over_inputs =
        Memory_zone.join !non_terminating_inputs res_if_termination.over_inputs_d;
      over_logic_inputs = !non_terminating_logic_inputs;
      over_outputs =
        Memory_zone.join !non_terminating_outputs res_if_termination.over_outputs_d;
    }

end


let externalize ~with_formals kf v =
  let filter = externalize_zone ~with_formals kf in
  Inout_type.map filter v

let compute_externals_using_prototype ?stmt kf =
  let internals = compute_using_prototype ?stmt kf in
  externalize ~with_formals:false kf internals

let ref_get_internal = ref (fun _kf : t -> assert false)

let get_internal_aux ?stmt kf =
  match stmt with
  | None -> !ref_get_internal kf
  | Some stmt ->
    try CallwiseResults.find (kf, Kstmt stmt)
    with Not_found ->
      if Eva.Analysis.use_spec_instead_of_definition kf then
        compute_using_prototype ~stmt kf
      else !ref_get_internal kf

let ref_get_external = ref (fun _kf : t -> assert false)

let get_external_aux ?stmt kf =
  match stmt with
  | None -> !ref_get_external kf
  | Some stmt ->
    try
      let internals = CallwiseResults.find (kf, Kstmt stmt) in
      externalize ~with_formals:false kf internals
    with Not_found ->
      if Eva.Analysis.use_spec_instead_of_definition kf then
        let r = compute_externals_using_prototype ~stmt kf in
        CallwiseResults.add (kf, Kstmt stmt) r;
        r
      else !ref_get_external kf

let extract_inout_from_froms kf assigns =
  let Eva.Assigns.{ return = deps_return; memory = deps_table } = assigns in
  let in_return = Eva.Deps.to_zone deps_return in
  let in_, out_ =
    match deps_table with
    | Top -> Memory_zone.top, Memory_zone.top
    | Bottom -> Memory_zone.bottom, Memory_zone.bottom
    | Map m ->
      let aux_from out in_ (acc_in,acc_out as acc) =
        (* Skip zones fully unassigned, they are not really port of the
           dependencies, but just present in the offsetmap to avoid "holes" *)
        match (in_ : Eva.Assigns.DepsOrUnassigned.t) with
        | Unassigned -> acc
        | AssignedFrom in_ | MaybeAssignedFrom in_ ->
          Memory_zone.join acc_in (Eva.Deps.to_zone in_),
          Memory_zone.join acc_out out
      in
      Eva.Assigns.Memory.fold aux_from m (Memory_zone.bottom, Memory_zone.bottom)
  in
  let in_ = Memory_zone.join in_return in_ in
  let externalize = externalize_zone ~with_formals:false kf in
  externalize in_, externalize out_


[@@@ warning "-60"]
module Callwise = struct

  module Record_Inout_Callbacks = Hook.Build (struct type t = Inout_type.t end)

  let merge_call_in_local_table call local_table v =
    let prev =
      try CallsiteHash.find local_table call
      with Not_found -> Inout_type.bottom
    in
    let joined = Inout_type.join v prev in
    CallsiteHash.replace local_table call joined

  let merge_call_in_global_tables (kf, _ as call) v =
    (* Global callwise table *)
    let prev =
      try CallwiseResults.find call
      with Not_found -> Inout_type.bottom
    in
    CallwiseResults.replace call (Inout_type.join v prev);
    (* Global, kf-indexed, table *)
    let prev =
      try Internals.find kf
      with Not_found -> Inout_type.bottom
    in
    Internals.replace kf (Inout_type.join v prev);
  ;;


  let call_inout_stack = ref []

  let reset () =
    call_inout_stack := []

  let call_for_callwise_inout _callstack kf _state = function
    | `Body ->
      let table_current_function = CallsiteHash.create 7 in
      call_inout_stack := (kf, table_current_function) :: !call_inout_stack
    | `Reuse | `Spec | `Builtin -> ()

  let pop_local_table kf =
    match !call_inout_stack with
    | (kf', table) :: tail ->
      if not (Kernel_function.equal kf kf') then
        Inout_parameters.fatal "callwise inout: %a != %a@."
          Kernel_function.pretty kf Kernel_function.pretty kf';
      CallsiteHash.iter merge_call_in_global_tables table;
      call_inout_stack := tail;
    | [] -> Inout_parameters.fatal "callwise: internal stack is empty"

  let end_record callstack kf inout =
    Record_Inout_Callbacks.apply inout;
    let callsite = Eva.Callstack.top_callsite callstack in
    match callsite, !call_inout_stack with
    | Kstmt _, (_caller, table) :: _ ->
      merge_call_in_local_table (kf, callsite) table inout;
    | Kglobal, [] ->  (* the entry point *)
      merge_call_in_global_tables (kf, callsite) inout;
      CallwiseResults.mark_as_computed ()
    | _ ->
      Inout_parameters.fatal
        "callwise: internal stack is inconsistent with Eva callstack"


  module MemExec =
    State_builder.Hashtbl
      (Datatype.Int.Hashtbl)
      (Inout_type)
      (struct
        let size = 17
        let dependencies = [Internals.self]
        let name = "Operational_inputs.MemExec"
      end)


  let compute_call_from_value_states kf call_stack states =
    let module Fenv = (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV) in
    let module Computer = Computer(Fenv)(
      struct
        let _version = "callwise"
        let _kf = kf

        (* Returns the [kf] pre-state with respect to the single [call_stack]. *)
        let kf_pre_state =
          Eva.Results.(at_start_of kf |> in_callstack call_stack |>
                       get_cvalue_model)

        let stmt_state stmt =
          try Cil_datatype.Stmt.Hashtbl.find states stmt
          with Not_found -> Cvalue.Model.bottom

        let stmt_request stmt = Eva.Results.in_cvalue_state (stmt_state stmt)

        let at_call stmt kf =
          let _cur_kf, table = List.hd !call_inout_stack in
          try
            let with_internals = CallsiteHash.find table (kf, Kstmt stmt) in
            let filter =
              match kf.fundec with
              | Definition (fundec, _) ->
                (fun b -> not (Base.is_formal_or_local b fundec))
              | _ ->
                let vi_kf = Kernel_function.get_vi kf in
                (fun b -> not (Base.is_formal_of_prototype b vi_kf))
            in
            Inout_type.map (Memory_zone.filter_base filter) with_internals
          with Not_found -> Inout_type.bottom
      end) in
    let module [@warning "-60"] Compute =
      Dataflows.Simple_forward (Fenv) (Computer)
    in
    Computer.end_dataflow ()

  let record_for_callwise_inout callstack kf pre_state value_res =
    let inout =
      match value_res with
      | `Body (Eva.Cvalue_callbacks.{before_stmts}, memexec_counter) ->
        let inout =
          if Eva.Analysis.save_results kf
          then
            let cvalue_states = Lazy.force before_stmts in
            compute_call_from_value_states kf callstack cvalue_states
          else top
        in
        MemExec.replace memexec_counter inout;
        pop_local_table kf;
        inout
      | `Reuse counter -> MemExec.find counter
      | `Spec _states
      | `Builtin (_states, None) -> compute_using_spec pre_state kf
      | `Builtin (_states, Some (froms,sure_out)) ->
        let in_, out_ = extract_inout_from_froms kf froms in
        {
          over_inputs_if_termination = in_;
          over_inputs = in_;
          over_logic_inputs = Memory_zone.bottom;
          over_outputs_if_termination = out_ ;
          over_outputs = out_;
          under_outputs_if_termination = sure_out;
        }
    in
    end_record callstack kf inout


  (* Register our callbacks inside the value analysis *)

  let () =
    Eva.Cvalue_callbacks.register_at_start_hook reset;
    Eva.Cvalue_callbacks.register_call_results_hook record_for_callwise_inout;
    Eva.Cvalue_callbacks.register_call_hook call_for_callwise_inout

  let _register_call_hook =
    Dynamic.register
      ~comment:"Registers a function to be applied on the inputs/outputs \
                computed for each function call."
      ~plugin:Inout_parameters.name
      "register_call_hook"
      Datatype.(func (func Inout_type.ty unit) unit)
      Record_Inout_Callbacks.extend_once

end


(* Functionwise version of the computations. *)
module FunctionWise = struct

  (* Stack of function being processed *)
  let call_stack : kernel_function Stack.t = Stack.create ()

  let compute_internal_using_cfg kf =
    try
      let module Fenv =
        (val Dataflows.function_env kf: Dataflows.FUNCTION_ENV)
      in
      let module Computer = Computer(Fenv)(struct
          let _version = "functionwise"
          let _kf = kf
          let kf_pre_state = Eva.Results.(at_start_of kf |> get_cvalue_model)
          let stmt_state s = Eva.Results.(before s |> get_cvalue_model)
          let stmt_request s = Eva.Results.before s
          let at_call stmt kf = get_external_aux ~stmt kf
        end) in
      Stack.iter (fun g -> if kf == g then raise Exit) call_stack;
      Stack.push kf call_stack;

      let module [@warning "-60"] Compute =
        Dataflows.Simple_forward (Fenv) (Computer)
      in
      let result = Computer.end_dataflow () in
      ignore (Stack.pop call_stack);
      result

    with Exit -> Inout_type.bottom (*TODO*) (*{
                                              Inout_type.over_inputs_if_termination = empty.over_inputs_d ;
                                              under_outputs_if_termination = empty.under_outputs_d;
                                              over_inputs = empty.over_inputs_d;
                                              over_outputs = empty.over_outputs_d;
                                              over_outputs_if_termination = empty.over_outputs_d;
                                              }*)

  let compute_internal_using_cfg kf =
    if not (Eva.Analysis.save_results kf) then
      top
    else begin
      Inout_parameters.feedback ~level:2 "computing for function %a%s"
        Kernel_function.pretty kf
        (let s = ref "" in
         Stack.iter
           (fun kf -> s := !s^" <-"^
                           (Format.asprintf "%a" Kernel_function.pretty kf))
           call_stack;
         !s);
      let r = compute_internal_using_cfg kf in
      Inout_parameters.feedback ~level:2 "done for function %a"
        Kernel_function.pretty kf;
      r
    end
end


let get_internal =
  Internals.memo
    (fun kf ->
       Eva.Analysis.compute ();
       try Internals.find kf (* The results may have been computed by the call
                                to Eva.Analysis.compute *)
       with
       | Not_found ->
         if Eva.Analysis.use_spec_instead_of_definition kf then
           compute_using_prototype kf
         else
           FunctionWise.compute_internal_using_cfg kf
    )

let raw_externals ~with_formals kf =
  let filter = externalize ~with_formals kf in
  filter (get_internal kf)

module Externals =
  Kernel_function.Make_Table(Inout_type)
    (struct
      let name = "External inouts full"
      let dependencies = [ Internals.self ]
      let size = 17
    end)
let get_external = Externals.memo (raw_externals ~with_formals:false)
let compute kf = ignore (get_external kf)



module Externals_With_Formals =
  Kernel_function.Make_Table(Inout_type)
    (struct
      let name = "Inout.Operational_inputs.Externals_With_Formals"
      let dependencies = [ Internals.self ]
      let size = 17
    end)
let get_external_with_formals =
  Externals_With_Formals.memo (raw_externals ~with_formals:true)


let pretty_operational_inputs_internal fmt kf =
  Format.fprintf fmt "@[InOut (internal) for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Inout_type.pretty_operational_inputs (get_internal kf)

let pretty_operational_inputs_external fmt kf =
  Format.fprintf fmt "@[InOut for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Inout_type.pretty_operational_inputs (get_external kf)

let pretty_operational_inputs_external_with_formals fmt kf =
  Format.fprintf fmt "@[InOut (with formals) for function %a:@\n%a@]@\n"
    Kernel_function.pretty kf
    Inout_type.pretty_operational_inputs (get_external_with_formals kf)

let pretty fmt x =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "@[<v 2>Operational inputs:@ @[<hov>%a@]@]@ "
    Memory_zone.pretty (x.Inout_type.over_inputs);
  Format.fprintf fmt "@[<v 2>Operational inputs on termination:@ @[<hov>%a@]@]@ "
    Memory_zone.pretty (x.Inout_type.over_inputs_if_termination);
  Format.fprintf fmt "@[<v 2>Sure outputs:@ @[<hov>%a@]@]"
    Memory_zone.pretty (x.Inout_type.under_outputs_if_termination);
  Format.fprintf fmt "@]"

let get_internal_precise = get_internal_aux


let () =
  ref_get_internal := get_internal;
  ref_get_external := get_external
