(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eval

(* Reference filled in by the callwise-inout callback *)
module InOutCallback =
  State_builder.Option_ref (Inout_type)
    (struct
      let dependencies = [Self.state]
      let name = "Transfer_stmt.InOutCallback"
    end)

let register_callback () =
  Eva_dynamic.Inout.register_call_hook InOutCallback.set

let () = Cmdline.run_after_configuring_stage register_callback

let current_kf_inout = InOutCallback.get_option

(* Should we warn about indeterminate copies in the function [kf] ? *)
let warn_indeterminate kf =
  let params = Parameters.WarnCopyIndeterminate.get () in
  Kernel_function.Set.mem kf params

(* An assignment from a right scalar lvalue is interpreted as a copy when
   indeterminate copies are allowed. Otherwise, such assignments are interpreted
   through the evaluation of the right lvalue, possibly leading to alarms about
   non initialization and dangling pointers. *)
let do_copy_at ~pos =
  match Position.kf pos with
  | None -> false
  | Some kf -> not (warn_indeterminate kf)

(* Warn for call arguments that contain uninitialized/escaping except on
   [Frama_C_show_each] directives or if the user disables these alarms
   on functions whose body is analyzed. *)
let is_determinate kf =
  let name = Kernel_function.get_name kf in
  (warn_indeterminate kf || Function_calls.use_spec_instead_of_definition kf)
  && not (Ast_info.start_with_frama_c_builtin name)

let subdivide_stmt = Eva_utils.get_subdivision

let subdivide_pos ~pos = match Position.stmt pos with
  | None -> Parameters.SubdivideNonLinear.get ()
  | Some stmt -> subdivide_stmt stmt

(* Used to disambiguate files for Frama_C_dump_each_file directives. *)
module DumpFileCounters =
  State_builder.Hashtbl (Datatype.String.Hashtbl) (Datatype.Int)
    (struct
      let size = 3
      let dependencies = [ Self.state ]
      let name = "Transfer_stmt.DumpFileCounters"
    end)

module VarHashtbl = Cil_datatype.Varinfo.Hashtbl

let substitution_visitor table =
  let rewrite_varinfo ~visitor:_ vi =
    match VarHashtbl.find_opt table vi with
    | None -> vi
    | Some vi' -> vi'
  in
  { Eva_ast.Rewrite.default with rewrite_varinfo }

module type Engine_Subset = sig
  include Engine_abstractions_sig.S

  module Transfer_inout : Engine_sig.Transfer_inout
    with type location = Loc.location
     and type value = Val.t
     and type valuation = Eval.Valuation.t

  module Interferences : Engine_sig.Interferences with type state = Dom.t

  module Compute : Engine_sig.Compute with type state = Dom.t
                                       and type value = Val.t
                                       and type loc = Loc.location
end

module Make (Engine: Engine_Subset) = struct

  module Value = Engine.Val
  module Location = Engine.Loc
  module Domain = Engine.Dom
  module Eval = Engine.Eval
  module EvaAstDeps = Eva_ast.MakeDepsOf (Location)
  module Interferences = Engine.Interferences

  type state = Domain.t

  (* When using a product of domains, a product of states may have no
     concretization (if the domains have inferred incompatible properties)
     without being bottom (if the inter-reduction between domains are
     insufficient to prove the incompatibility). In such a state, an evaluation
     can lead to bottom without any alarm (the evaluation reveals the
     incompatibility).  We report these cases to the user, as they could also
     reveal a bug in some Eva's abstractions. Note that they should not happen
     when only one domain is enabled. *)

  let notify_unreachability fmt =
    if Domain.log_category = Domain_product.product_category
    then
      Self.feedback ~level:4 ~current:true ~once:true
        "The evaluation of %(%a%)@ led to bottom without alarms:@ at this point \
         the product of states has no possible concretization.@."
        fmt
    else
      Self.warning ~current:true
        "The evaluation of %(%a%)@ led to bottom without alarms:@ at this point \
         the abstract state has no possible concretization,@ which is probably \
         a bug."
        fmt

  let report_unreachability _state (result, alarms) fmt =
    if result = `Bottom && Alarmset.is_empty alarms
    then notify_unreachability fmt
    else Format.ifprintf Format.std_formatter fmt

  (* The three functions below call evaluation functions and notify the user
     if they lead to bottom without alarms. *)

  let evaluate_and_check ?valuation ~subdivnb state expr =
    let res = Eval.evaluate ?valuation ~subdivnb state expr in
    report_unreachability state res "the expression %a" Eva_ast.pp_exp expr;
    res

  let lvaluate_and_check ?valuation ~subdivnb ~for_writing state lval =
    let res = Eval.lvaluate ?valuation ~subdivnb ~for_writing state lval in
    report_unreachability state res "the lvalue %a" Eva_ast.pp_lval lval;
    res

  let copy_lvalue_and_check ?valuation ~subdivnb state lval =
    let res = Eval.copy_lvalue ?valuation ~subdivnb state lval in
    report_unreachability state res "the copy of %a" Eva_ast.pp_lval lval;
    res

  (* ------------------------------------------------------------------------ *)
  (*                               Assignments                                *)
  (* ------------------------------------------------------------------------ *)

  (* Default assignment: evaluates the right expression. *)
  let assign_by_eval ~subdivnb state valuation expr =
    evaluate_and_check ~valuation ~subdivnb state expr
    >>=: fun (valuation, value) ->
    Assign value, valuation

  (* Assignment by copying the value of a right lvalue. *)
  let assign_by_copy ~subdivnb state valuation lval lloc =
    copy_lvalue_and_check ~valuation ~subdivnb state lval
    >>=: fun (valuation, value) ->
    Copy ({lval; lloc}, value), valuation

  (* For an initialization, use for_writing:false for the evaluation of
     the left location, as the written variable could be const.  This is only
     useful for local initializations through function calls, as other
     initializations are handled by initialization.ml. *)
  let for_writing ~pos = match Position.stmt pos with
    | None -> false
    | Some stmt -> match stmt.skind with
      | Instr (Local_init _) -> false
      | _ -> true

  (* Find a lvalue hidden under identity casts. This function correctly detects
     bitfields (thanks to [need_cast]) and will never expose the underlying
     field. *)
  let rec find_lval (expr : exp) = match expr.node with
    | Lval lv -> Some lv
    | CastE (typ, e) ->
      if Eval_typ.need_cast typ e.typ then None else find_lval e
    | _ -> None

  (* Emits an alarm if the left and right locations of a struct or union copy
     overlap. *)
  let check_overlap typ (lval, loc) (right_lval, right_loc) =
    if Ast_types.is_struct_or_union typ
    then
      let truth = Location.assume_no_overlap ~partial:true loc right_loc in
      let alarm () =
        let cil_lval = Eva_ast.to_cil_lval lval in
        let cil_right_lval = Eva_ast.to_cil_lval right_lval in
        Alarms.Overlap (cil_lval, cil_right_lval)
      in
      Eval.interpret_truth ~alarm (loc, right_loc) truth
    else `Value (loc, right_loc), Alarmset.none

  (* Checks the compatibility between the left and right locations of a copy. *)
  let are_compatible loc right_loc =
    let size1 = Location.size loc
    and size2 = Location.size right_loc in
    Z_or_top.equal size1 size2 && not (Z_or_top.is_top size1)

  (* Assignment. *)
  let assign_lv_or_ret ~pos ~is_ret state lval expr =
    let for_writing = for_writing ~pos in
    let subdivnb = subdivide_pos ~pos in
    let eval, alarms = lvaluate_and_check ~for_writing ~subdivnb state lval in
    Alarmset.emit ~pos alarms;
    match eval with
    | `Bottom ->
      Self.warning ~pos ~stacktrace:true ~once:true
        "@[all target addresses were invalid. This path is \
         assumed to be dead.@]";
      `Bottom
    | `Value (valuation, lloc) ->
      (* Tries to interpret the assignment as a copy for the returned value
         of a function call, on struct and union types, and when
         -eva-warn-copy-indeterminate is disabled. *)
      let lval_copy =
        if is_ret || Ast_types.is_struct_or_union lval.typ || do_copy_at ~pos
        then find_lval expr
        else None
      in
      let eval, alarms = match lval_copy with
        | None ->
          assert (not is_ret);
          assign_by_eval ~subdivnb state valuation expr
        | Some right_lval ->
          let for_writing = false in
          (* In case of a copy, checks that the left and right locations are
             compatible and that they do not overlap. *)
          lvaluate_and_check ~for_writing ~subdivnb ~valuation state right_lval
          >>= fun (valuation, rloc) ->
          check_overlap lval.typ (lval, lloc) (right_lval, rloc)
          >>= fun (lloc, rloc) ->
          if are_compatible lloc rloc
          then assign_by_copy ~subdivnb state valuation right_lval rloc
          else assign_by_eval ~subdivnb state valuation expr
      in
      if is_ret then assert (Alarmset.is_empty alarms);
      Alarmset.emit ~pos alarms;
      let* assigned, valuation = eval in
      let access =
        Engine.Transfer_inout.register_assign_lval pos valuation lval expr
      in
      let domain_valuation = Eval.to_domain_valuation valuation in
      let lvalue = { lval; lloc } in
      let+ state =
        Domain.assign ~pos lvalue expr assigned domain_valuation state
      in
      Interferences.inject_after_change ~pos access state

  let assign = assign_lv_or_ret ~is_ret:false
  let assign_ret = assign_lv_or_ret ~is_ret:true

  (* ------------------------------------------------------------------------ *)
  (*                               Assumption                                 *)
  (* ------------------------------------------------------------------------ *)

  (* Assumption. *)
  let assume ~pos state expr positive =
    let eval, alarms = Eval.reduce state expr positive in
    (* TODO: check not comparable. *)
    Alarmset.emit ~pos alarms;
    let* valuation = eval in
    let access = Engine.Transfer_inout.register_read_exp pos valuation expr in
    let+ state =
      Domain.assume ~pos expr positive (Eval.to_domain_valuation valuation) state
    in
    Interferences.inject_after_change ~pos access state


  (* ------------------------------------------------------------------------ *)
  (*                             Function Calls                               *)
  (* ------------------------------------------------------------------------ *)

  (* Returns the result of a call. *)
  let process_call ~pos call recursion valuation access state =
    let domain_valuation = Eval.to_domain_valuation valuation in
    (* Process the call according to the domain decision. *)
    match Domain.start_call ~pos call recursion domain_valuation state with
    | `Value state ->
      let pos = Position.of_local pos in
      let state = Interferences.inject_after_change ~pos access state in
      Domain.Store.register_state call.callstack (Start call.kf) state;
      Engine.Compute.compute_call call recursion state
    | `Bottom ->
      { states = []; cacheable = Cacheable; kind = `Bottom }

  (* ------------------- Retro propagation on formals ----------------------- *)

  (* [is_safe_argument valuation expr] is true iff the expression [expr] could
     not have been written during the last call.
     If the Location module includes precise_locs, and if the inout plugins
     is run callwise, then the function uses the precise_locs of the [valuation]
     and the results of inout. An argument is safe if its dependencies (the
     locations on which its value depends) do not intersect with the zones
     written by the called function.
     If precise_locs or the callwise inout is not available, a syntactic
     criterion is used. See {!Backward_formals.safe_argument}. *)
  let is_safe_argument valuation expr =
    match InOutCallback.get_option () with
    | None -> Backward_formals.safe_argument expr
    | Some inout ->
      let find_loc lval = Eval.Valuation.find_loc_def valuation lval in
      let expr_zone = EvaAstDeps.zone_of_exp find_loc expr in
      let written_zone = inout.Inout_type.over_outputs_if_termination in
      not (Memory_zone.intersects expr_zone written_zone)

  (* Removes from the list of arguments of a call the arguments whose concrete
     or formal argument could have been written during the call, as well as
     arguments of non arithmetic or non pointer type. *)
  let filter_safe_arguments valuation call =
    let written_formals = Backward_formals.written_formals call.kf in
    let is_safe argument =
      not (Cil_datatype.Varinfo.Set.mem argument.formal written_formals)
      && Ast_types.is_scalar argument.formal.vtype
      && is_safe_argument valuation argument.concrete
    in
    List.filter is_safe call.arguments

  (* At the end of a call, this function gathers the arguments whose value can
     be reduced at the call site. These are the arguments such that:
     – the formal has not been written during the call, but its value has been
       reduced;
     – no variable of the concrete argument has been written during the call
       (thus the concrete argument is still equal to the formal).
     [state] is the state at the return statement of the called function;
     it is used to evaluate the formals; their values are then compared to the
     ones at the beginning of the call.
     The function returns an association list between the argument that can be
     reduced, and their new (more precise) value.  *)
  let gather_reduced_arguments call valuation state =
    let safe_arguments = filter_safe_arguments valuation call in
    let empty = Eval.Valuation.empty in
    let reduce_one_argument acc argument =
      let* acc = acc in
      let pre_value = match argument.avalue with
        | Assign pre_value -> `Value pre_value
        | Copy (_lv, pre_value) -> pre_value.v
      in
      let lval = Eva_ast.Build.var argument.formal in
      (* We use copy_lvalue instead of evaluate to get the escaping flag:
         if a formal is escaping at the end of the called function, it may
         have been freed, which is not detected as a write. We prevent the
         backward propagation in that case.
         If the call has copied the argument, it may be uninitialized. Thus,
         we also avoid the backward propagation if the formal is uninitialized
         here. This should not happen in the Assign case above. *)
      let* _valuation, post_value =
        fst (Eval.copy_lvalue ~valuation:empty ~subdivnb:0 state lval) in
      if
        Bottom.is_included Value.is_included pre_value post_value.v
        || post_value.escaping || not post_value.initialized
      then `Value acc
      else post_value.v >>-: fun post_value -> (argument, post_value) :: acc
    in
    List.fold_left reduce_one_argument (`Value []) safe_arguments

  (* [reductions] is an association list between expression and value.
     This function reduces the [state] by assuming [expr = value] for each pair
     (expr, value) of [reductions]. *)
  let reduce_arguments reductions state =
    let valuation = `Value Eval.Valuation.empty in
    let reduce_one_argument valuation (argument, post_value) =
      let* valuation = valuation in
      Eval.assume ~valuation state argument.concrete post_value
    in
    let* valuation = List.fold_left reduce_one_argument valuation reductions in
    Domain.update (Eval.to_domain_valuation valuation) state

  (* -------------------- Treat the results of a call ----------------------- *)

  (* Treat the assignment of the return value in the caller: if the function
     has a non-void type, perform the assignment if there is a lvalue at
     the callsite, and in all cases, remove the pseudo-variable from scope. *)
  let treat_return ~pos ~kf_callee lv return state =
    match lv, return with
    | None, None -> `Value state
    | None, Some vi_ret -> `Value (Domain.leave_scope kf_callee [vi_ret] state)
    | Some _, None -> assert false
    | Some lval, Some vi_ret ->
      let exp_ret_caller = Eva_ast.Build.var_exp vi_ret in
      let+ state = assign_ret ~pos state lval exp_ret_caller in
      Domain.leave_scope kf_callee [vi_ret] state

  (* ---------------------- Make a one function call ------------------------ *)

  (* The variables leaving scope at the end of a call to [kf]:
     the formals, and the locals of the body of kf, if any. *)
  let leaving_vars kf =
    let locals =
      try
        let fundec = Kernel_function.get_definition kf in
        fundec.sbody.blocals
      with Kernel_function.No_Definition -> []
    in
    Kernel_function.get_formals kf @ locals

  (* Do the call to one function. *)
  let do_one_call ~pos valuation lv call recursion access state =
    let kf_callee = call.kf in
    let pre = state in
    (* Process the call according to the domain decision. *)
    let call_result = process_call ~pos call recursion valuation access state in
    let leaving_vars = leaving_vars kf_callee in
    (* Treat each resulting state one by one. *)
    let process_resulting_state state =
      (* Gathers the possible reductions on the value of the concrete arguments
         at the call site, according to the value of the formals at the post
         state of the called function. *)
      let* reductions = gather_reduced_arguments call valuation state in
      (* The formals (and the locals) of the called function leave scope. *)
      let post = Domain.leave_scope kf_callee leaving_vars state in
      let recursion = Option.map Recursion.revert recursion in
      (* Computes the state after the call, from the post state at the end of
         the called function, and the pre state at the call site. *)
      let* state = Domain.finalize_call ~pos call recursion ~pre ~post in
      (* Backward propagates the [reductions] on the concrete arguments. *)
      let* state = reduce_arguments reductions state in
      treat_return ~pos:(Position.of_local pos) ~kf_callee lv call.return state
    in
    (* Partitioning key remains unchanged. *)
    let process (key, state) =
      let+ state' = process_resulting_state state in
      key, state'
    in
    let states = Bottom.list_filter_map process call_result.states in
    { call_result with states }


  (* ------------------- Evaluation of the arguments ------------------------ *)

  (* [evaluate_argument ~determinate valuation state expr]
     evaluates the call argument [expr] in the state [state] and the valuation
     [valuation]. Returns the value assigned, and the updated valuation.
     TODO: share more code with [assign]. *)
  let evaluate_actual ~subdivnb ~determinate valuation state (expr : exp) =
    match expr.node with
    | Lval lv ->
      lvaluate_and_check ~for_writing:false ~subdivnb ~valuation state lv
      >>= fun (valuation, loc) ->
      if Z_or_top.is_top (Location.size loc)
      then
        Self.abort ~current:true
          "Function argument %a has unknown size. Aborting"
          Eva_ast.pp_exp expr;
      if determinate && Ast_types.is_scalar lv.typ
      then assign_by_eval ~subdivnb state valuation expr
      else assign_by_copy ~subdivnb state valuation lv loc
    | _ -> assign_by_eval ~subdivnb state valuation expr

  (* Evaluates the list of the actual arguments of a call. Returns the list
     of each argument expression associated to its assigned value, and the
     valuation resulting of the evaluations. *)
  let compute_actuals ~subdivnb ~determinate valuation state arguments =
    let process expr acc =
      acc >>= fun (args, valuation) ->
      evaluate_actual ~subdivnb ~determinate valuation state expr >>=:
      fun (assigned, valuation) ->
      (expr, assigned) :: args, valuation
    in
    List.fold_right process arguments (`Value ([], valuation), Alarmset.none)

  (* ------------------------- Make an Eval.call ---------------------------- *)

  (* Create an Eval.call *)
  let create_call ~pos kf args =
    let return = Library_functions.get_retres_vi kf in
    let callstack = Callstack.push kf (fst pos) (snd pos) in
    let arguments, rest =
      let formals = Kernel_function.get_formals kf in
      let rec format_arguments acc args formals = match args, formals with
        | _, [] -> acc, args
        | [], _ -> assert false
        | (concrete, avalue) :: args, formal :: formals ->
          let argument = { formal ; concrete; avalue } in
          format_arguments (argument :: acc)  args formals
      in
      let arguments, rest = format_arguments [] args formals in
      let arguments = List.rev arguments in
      arguments, rest
    in
    {kf; callstack; arguments; rest; return; }

  let replace_value visitor substitution = function
    | Assign value -> Assign (Value.replace_base substitution value)
    | Copy (loc, flagged) ->
      let v = flagged.v >>-: Value.replace_base substitution in
      let flagged = { flagged with v } in
      let lloc = Location.replace_base substitution loc.lloc in
      let lval = Eva_ast.Rewrite.visit_lval visitor loc.lval in
      let loc = { lval; lloc } in
      Copy (loc, flagged)

  let replace_recursive_call recursion call =
    let tbl = VarHashtbl.create 9 in
    List.iter (fun (v1, v2) -> VarHashtbl.add tbl v1 v2) recursion.substitution;
    let visitor = substitution_visitor tbl in
    let base_substitution = recursion.base_substitution in
    let replace_arg argument =
      let concrete = Eva_ast.Rewrite.visit_exp visitor argument.concrete in
      let avalue = replace_value visitor base_substitution argument.avalue in
      { argument with concrete; avalue }
    in
    let arguments = List.map replace_arg call.arguments in
    { call with arguments; }

  let make_call ~pos ~subdivnb kf arguments valuation state =
    (* Evaluate the arguments of the call. *)
    let determinate = is_determinate kf in
    compute_actuals ~subdivnb ~determinate valuation state arguments
    >>=: fun (args, valuation) ->
    let call = create_call ~pos kf args in
    let recursion = Recursion.make call in
    let replace a = replace_recursive_call a call in
    let call = Option.fold ~some:replace ~none:call recursion in
    call, recursion, valuation

  (* ----------------- show_each and dump_each directives ------------------- *)

  (* The product of domains formats the printing of each leaf domains, by
     checking their log_category and adding their name before the dump. If the
     domain is not a product, this needs to be done here. *)
  let print_state =
    if Domain.log_category = Domain_product.product_category
    then Domain.pretty
    else if Self.is_debug_key_enabled Domain.log_category
    then
      fun fmt state ->
        Format.fprintf fmt "# %s:@ @[<hv>%a@]@ " Domain.name Domain.pretty state
    else fun _ _ -> ()

  (* Frama_C_dump_each functions. *)
  let dump_state ~pos name state =
    Self.result ~dkey:Self.dkey_show ~pos ~stacktrace:true
      "%s:@\n@[<v>%a@]==END OF DUMP=="
      name print_state state

  (* Idem as for [print_state]. *)
  let show_expr =
    if Domain.log_category = Domain_product.product_category
    then Domain.show_expr
    else if Self.is_debug_key_enabled Domain.log_category
    then
      fun valuation state fmt exp ->
        Format.fprintf fmt "# %s: @[<hov>%a@]"
          Domain.name (Domain.show_expr valuation state) exp
    else fun _ _ _ _ -> ()

  (* Frama_C_domain_show_each functions. *)
  let domain_show_each ~pos ~subdivnb name arguments state =
    let pretty fmt expr =
      let pp fmt  =
        match fst (Eval.evaluate ~subdivnb state expr) with
        | `Bottom ->
          Unicode.pp_bottom fmt
        | `Value (valuation, _v) ->
          show_expr (Eval.to_domain_valuation valuation) state fmt expr
      in
      Format.fprintf fmt "%a : @[<h>%t@]" Eva_ast.pp_exp expr pp
    in
    let pp = Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@ " ~suf:"@]" pretty in
    Self.result ~dkey:Self.dkey_show ~pos ~stacktrace:true
      "@[<v>%s:@ %a@]"
      name pp arguments

  (* For non scalar expressions, prints the offsetmap of the cvalue domain. *)
  let show_offsm =
    match Engine.Dom.get_cvalue, Location.get Main_locations.PLoc.key with
    | None, _ | _, None ->
      fun fmt _ _ _ -> Unicode.pp_top fmt
    | Some get_cvalue, Some get_ploc ->
      fun fmt subdivnb lval state ->
        try
          let offsm =
            let* (_, loc) =
              fst (Eval.lvaluate ~for_writing:false ~subdivnb state lval) in
            Eval_op.offsetmap_of_loc (get_ploc loc) (get_cvalue state)
          in
          (Bottom.pretty (Eval_op.pretty_offsetmap lval.typ)) fmt offsm
        with Abstract_interp.Error_Top ->
          Unicode.pp_top fmt

  (* For scalar expressions, prints the cvalue component of their values. *)
  let show_value =
    match Value.get Main_values.CVal.key with
    | None -> fun fmt _ _ _ -> Unicode.pp_top fmt
    | Some get_cval ->
      fun fmt subdivnb expr state ->
        let value =
          fst (Eval.evaluate ~subdivnb state expr) >>-: snd >>-: get_cval
        in
        (Bottom.pretty Cvalue.V.pretty) fmt value

  let pretty_arguments ~subdivnb state arguments =
    let is_scalar lval = Ast_types.is_scalar lval.Eva_ast.typ in
    let pretty fmt (expr : Eva_ast.exp) =
      match expr.node with
      | StartOf { node = Var v, NoOffset } when Ast_info.is_string_literal v ->
        let s = Globals.Vars.get_string_literal v in
        Format.fprintf fmt "{{ %a }}" Printer.pp_str_literal s
      | Lval lval | StartOf lval when not (is_scalar lval) ->
        show_offsm fmt subdivnb lval state
      | _ -> show_value fmt subdivnb expr state
    in
    Pretty_utils.pp_list ~pre:"@[<hv>" ~sep:",@ " ~suf:"@]" pretty arguments

  (* Frama_C_show_each functions. *)
  let show_each ~pos ~subdivnb name arguments state =
    Self.result ~dkey:Self.dkey_show ~pos ~stacktrace:true
      "@[<hv>%s:@ %a@]"
      name (pretty_arguments ~subdivnb state) arguments

  (* Frama_C_dump_each_file functions. *)
  let dump_state_file_exc ~pos ~subdivnb name arguments state =
    let size = String.length name in
    let name =
      if size > 23
      (*  Frama_C_dump_each_file_ + 'something' *)
      then String.sub name 23 (size - 23)
      else failwith "no filename specified"
    in
    let n = try DumpFileCounters.find name with Not_found -> 0 in
    DumpFileCounters.add name (n+1);
    let file = Format.sprintf "%s_%d" name n |> Filepath.of_string in
    let open Filesystem.Operators in
    let$ fmt = Filesystem.with_formatter_exn file in
    let loc = Current_loc.get () in
    Self.feedback ~dkey:Self.dkey_show ~pos ~stacktrace:true
      "Dumping state in file '%a'" Filepath.pretty file;
    Format.fprintf fmt "DUMPING STATE at %a@."
      Fileloc.pretty_long loc;
    let pretty_args = pretty_arguments ~subdivnb state in
    if arguments <> []
    then Format.fprintf fmt "Args: %a@." pretty_args arguments;
    Format.fprintf fmt "@[<v>%a@]@?" print_state state

  let dump_state_file ~pos ~subdivnb name arguments state =
    try dump_state_file_exc ~pos ~subdivnb name arguments state
    with e ->
      Self.warning ~pos ~once:true
        "Error during, or invalid call to Frama_C_dump_each_file (%s). Ignoring"
        (Printexc.to_string e)

  (** Applies the show_each or dump_each directives. *)
  let apply_special_directives ~pos ~subdivnb kf arguments state =
    let pos = Position.of_local pos in
    let name = Kernel_function.get_name kf in
    if Ast_info.start_with_frama_c name
    then
      if Ast_info.is_show_each_builtin name
      then (show_each ~pos ~subdivnb name arguments state; true)
      else if Ast_info.is_domain_show_each_builtin name
      then (domain_show_each ~pos ~subdivnb name arguments state; true)
      else if Ast_info.is_dump_file_builtin name
      then (dump_state_file ~pos ~subdivnb name arguments state; true)
      else if Ast_info.is_dump_each_builtin name
      then (dump_state ~pos name state; true)
      else false
    else false

  (* Legacy callbacks for the cvalue domain, usually called by
     {Cvalue_transfer.start_call}. *)
  let apply_cvalue_callback ~pos kf state =
    (* Generates assigns clauses for other plugins, as they may use
       the directive specification. *)
    Populate_spec.populate_funspec kf [`Assigns];
    let stmt, callstack = pos in
    let stack_with_call = Callstack.push kf stmt callstack in
    let cvalue_state = Engine.Dom.get_cvalue_or_top state in
    Cvalue_callbacks.apply_call_hooks stack_with_call kf cvalue_state `Builtin;
    Cvalue_callbacks.apply_call_results_hooks stack_with_call kf cvalue_state
      (`Builtin ([cvalue_state], None))

  (* --------------------- Process the call statement ---------------------- *)

  (* Aborts the analysis when a function pointer is completely imprecise. *)
  let top_function_pointer func =
    if not (Parameters.Domains.mem "cvalue") then
      Self.abort ~current:true
        "Calls through function pointers are not supported without the cvalue \
         domain."
    else
      Self.fatal ~current:true
        "Function pointer %a evaluates to anything." Eva_ast.pp_lhost func

  let join_call_results res1 res2 =
    let states = res2.Engine_sig.states @ res1.Engine_sig.states
    and cacheable =
      if res1.cacheable = NoCacheCallers || res2.cacheable = NoCacheCallers
      then NoCacheCallers
      else Cacheable
    and kind = match res1.kind, res2.kind with
      | `Body, _ | _, `Body -> `Body
      | `Spec, _ | _, `Spec -> `Spec
      | `Builtin, _ | _, `Builtin -> `Builtin
      | `Internal, _ | _, `Internal -> `Internal
      | `Bottom, `Bottom -> `Bottom
    in
    Engine_sig.{ states; cacheable; kind }

  let call ~pos lval_option func args state =
    let stmt = fst pos in
    let subdivnb = subdivide_stmt stmt in
    (* Resolve [func] into the called kernel functions. *)
    let functions, alarms =
      Eval.eval_function ~subdivnb func ~args state
    in
    Alarmset.emit ~pos:(Position.of_local pos) alarms;
    let bottom =
      Engine_sig.{ states = []; cacheable = Cacheable; kind = `Bottom }
    in
    let process_one_function kf valuation =
      (* Create the call. *)
      let eval, alarms = make_call ~pos ~subdivnb kf args valuation state in
      let access =
        (* Register call arguments to Inout_access *)
        let+ call, _, valuation = eval in
        let position = Position.of_local pos in
        Engine.Transfer_inout.register_call_args position valuation call
      in
      let access = Bottom.value ~bottom:Inout_access.Access.bottom access in
      (* The special Frama_C_ functions to print states are handled here. *)
      if apply_special_directives ~pos ~subdivnb kf args state
      then
        let () = apply_cvalue_callback ~pos kf state in
        let states = [(Partition.Key.empty, state)] in
        Engine_sig.{ states; cacheable = Cacheable; kind = `Internal }
      else begin
        Alarmset.emit ~pos:(Position.of_local pos) alarms;
        match eval with
        | `Bottom -> bottom
        | `Value (call, recursion, valuation) ->
          let do_one_call () =
            do_one_call ~pos valuation lval_option call recursion access state
          in
          let finally = InOutCallback.clear in
          let call_result = Fun.protect ~finally do_one_call in
          let cacheable =
            if call_result.cacheable = NoCacheCallers then NoCacheCallers
            else Cacheable
          in
          Engine_sig.{ call_result with cacheable }
      end
    in
    match functions with
    | `Bottom -> bottom
    | `Top -> top_function_pointer func
    | `Value functions ->
      (* Check "calls" annotations, and reduce called functions accordingly. *)
      let functions = Transfer_logic.check_calls_annotations stmt functions in
      (* Process each possible function apart, and append the result list. *)
      let process acc (kf, valuation) =
        process_one_function kf valuation
        |> join_call_results acc
      in
      List.fold_left process bottom functions


  (* ------------------------------------------------------------------------ *)
  (*                            Return statements                             *)
  (* ------------------------------------------------------------------------ *)

  let return ~pos return_exp state =
    let kf = Position.Local.kf pos in
    match return_exp with
    | None -> `Value state
    | Some return_exp ->
      let result_vi = Option.get (Library_functions.get_retres_vi kf) in
      let return_lval = Eva_ast.Build.var result_vi in
      let kind = Abstract_domain.Result kf in
      let state = Domain.enter_scope kind [result_vi] state in
      let pos = Position.of_local pos in
      assign ~pos state return_lval return_exp


  (* ------------------------------------------------------------------------ *)
  (*                            Unspecified Sequence                          *)
  (* ------------------------------------------------------------------------ *)

  exception EBottom of Alarmset.t

  let process_truth ~alarm =
    let build_alarm status = Alarmset.singleton ~status (alarm ()) in
    function
    | `Unreachable           -> raise (EBottom Alarmset.none)
    | `False                 -> raise (EBottom (build_alarm Alarmset.False))
    | `Unknown _             -> build_alarm Alarmset.Unknown
    | `True | `TrueReduced _ -> Alarmset.none

  let check_non_overlapping state lvs1 lvs2 =
    let lvaluate ~valuation lval =
      fst (Eval.lvaluate ~valuation ~for_writing:false ~subdivnb:0 state lval)
    in
    let eval_loc (acc, valuation) lval =
      match lvaluate ~valuation lval with
      | `Bottom -> acc, valuation
      | `Value (valuation, loc) -> (lval, loc) :: acc, valuation
    in
    let eval_list valuation lvs =
      List.fold_left eval_loc ([], valuation) lvs
    in
    let list1, valuation = eval_list Eval.Valuation.empty lvs1 in
    let list2, _ = eval_list valuation lvs2 in
    let check acc (lval1, loc1) (lval2, loc2) =
      let truth = Location.assume_no_overlap ~partial:false loc1 loc2 in
      let alarm () =
        let cil_lval1 = Eva_ast.to_cil_lval lval1
        and cil_lval2 = Eva_ast.to_cil_lval lval2 in
        Alarms.Not_separated (cil_lval1, cil_lval2)
      in
      let alarm = process_truth ~alarm truth in
      Alarmset.combine alarm acc
    in
    List.product_fold check Alarmset.none list1 list2

  (* Not currently taking advantage of calls information. But see
     plugin Undefined Order by VP. *)
  let check_unspecified_sequence ~pos state seq =
    let check_stmt_pair acc statement1 statement2 =
      let stmt1, _, writes1, _, _ = statement1 in
      let stmt2, modified2, writes2, reads2, _ = statement2 in
      if stmt1 == stmt2 then acc else
        (* Values that cannot be read, as they are modified in the statement
           (but not by the whole sequence itself) *)
        let unauthorized_reads =
          List.filter
            (fun x -> List.for_all
                (fun y -> not (Eva_ast.Lval.equal x y)) modified2)
            writes1
        in
        let alarms1 = check_non_overlapping state unauthorized_reads reads2 in
        let alarms =
          if stmt1.sid >= stmt2.sid then alarms1 else
            let alarms2 = check_non_overlapping state writes1 writes2 in
            Alarmset.combine alarms1 alarms2
        in
        Alarmset.combine alarms acc
    in
    try
      let alarms = List.product_fold check_stmt_pair Alarmset.none seq seq in
      Alarmset.emit ~pos alarms;
      `Value ()
    with EBottom alarms -> Alarmset.emit ~pos alarms; `Bottom

  (* ------------------------------------------------------------------------ *)
  (*                               Scopes                                     *)
  (* ------------------------------------------------------------------------ *)

  (* Makes the local variables [variables] enter the scope in [state].
     Also initializes volatile variable to top.

     Note:

     All variables local to a block are introduced in domain states when
     entering the block. Variables explicitly initialized at declaration time
     (for which vi.vdefined is true) enter the scope too early, as they should
     be introduced on the fly when encountering their [Local_init] instruction.
     However, goto statements can skip their declaration/initialization, so it
     is safer to always introduce all local variables (without initialize them)
     when entering a block. *)
  let enter_scope kf variables state =
    let kind = Abstract_domain.Local kf in
    let state = Domain.enter_scope kind variables state in
    let is_volatile varinfo =
      Ast_types.has_qualifier "volatile" varinfo.vtype
    in
    let vars = List.filter is_volatile variables in
    let initialized = false in
    let init_value = Abstract_domain.Top in
    let initialize_volatile state varinfo =
      let lval = Eva_ast.Build.var varinfo in
      let location = Location.eval_varinfo varinfo in
      Domain.initialize_variable lval location ~initialized init_value state
    in
    List.fold_left initialize_volatile state vars

  let leave_scope kf variables state =
    Domain.leave_scope kf variables state
end
