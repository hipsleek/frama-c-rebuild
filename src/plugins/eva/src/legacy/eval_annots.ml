(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eval_terms
open Lattice_bounds

let has_requires spec =
  let behav_has_requires b = b.b_requires <> [] in
  List.exists behav_has_requires spec.spec_behavior

let code_annotation_text ca =
  match ca.annot_content with
  | AAssert (_, {tp_kind}) -> Cil_printer.name_of_assert tp_kind
  | AInvariant _ ->  "loop invariant"
  | AVariant _ | AAssigns _ | AAllocation _ | AStmtSpec _
  | AExtended _  ->
    assert false (* currently not treated by Value *)

(* location of the given code annotation. If unknown, use the location of the
   statement instead. *)
let code_annotation_loc ca stmt =
  match Cil_datatype.Code_annotation.loc ca with
  | Some loc when not (Fileloc.(equal loc unknown)) -> loc
  | _ -> Cil_datatype.Stmt.loc stmt


let iter_normal_ensures f kf =
  let f bhv _emitter (kind, ensures) =
    if kind = Normal
    then f (Property.ip_of_ensures kf Kglobal bhv (kind, ensures))
  in
  Annotations.iter_behaviors
    (fun bhv -> Annotations.iter_ensures (f bhv) kf bhv.b_name) kf

let mark_unreachable () =
  let mark ppt =
    if not (Property_status.automatically_computed ppt) then begin
      Self.debug "Marking property %a as dead"
        Description.pp_property ppt;
      let emit =
        Property_status.emit ~distinct:false Eva_utils.emitter ~hyps:[]
      in
      let reach_p = Property.ip_reachable_ppt ppt in
      emit ppt Property_status.True;
      emit reach_p Property_status.False_and_reachable
    end
  in
  (* Mark standard code annotations *)
  let mark_code_annot stmt _emit ca =
    if not (Results.is_reachable stmt) then begin
      let kf = Kernel_function.find_englobing_kf stmt in
      let ppts = Property.ip_of_code_annot kf stmt ca in
      List.iter mark ppts;
    end
  in
  (* Mark preconditions of dead calls *)
  let mark_preconditions = object
    inherit Visitor.frama_c_inplace

    method! vstmt_aux stmt =
      if not (Results.is_reachable stmt) then begin
        let mark_status kf =
          (* Do not mark preconditions as dead if they are not analyzed in
             non-dead code. Otherwise, the consolidation does strange things. *)
          if not (Eva_utils.skip_specifications kf) ||
             Builtins.is_builtin_overridden kf
          then begin
            (* Setup all precondition statuses for [kf]: maybe it has
               never been called anywhere. *)
            Statuses_by_call.setup_all_preconditions_proxies kf;
            (* Now mark the statuses at this particular statement as dead*)
            let preconds =
              Statuses_by_call.all_call_preconditions_at
                ~warn_missing:false kf stmt
            in
            List.iter (fun (_, p) -> mark p) preconds
          end
        in
        match stmt.skind with
        | Instr (Call (_, e, _, _)) ->
          Option.iter mark_status (Kernel_function.get_called e)
        | Instr(Local_init(_, ConsInit(f,_,_),_)) ->
          mark_status (Globals.Functions.get f)
        | _ -> ()
      end;
      Cil.DoChildren

    method! vinst _ = Cil.SkipChildren
    method! vexpr _ = Cil.SkipChildren
    method! vlval _ = Cil.SkipChildren
    method! vtype _ = Cil.SkipChildren
    method! vspec _ = Cil.SkipChildren
    method! vcode_annot _ = Cil.SkipChildren
  end
  in
  (* Mark postconditions of analyzed functions with unreachable return stmt. *)
  let mark_postconditions kf =
    match Function_calls.analysis_status kf with
    | Analyzed _ when not (Eva_utils.skip_specifications kf) ->
      let return_stmt = Kernel_function.find_return kf in
      if not (Results.is_reachable return_stmt) then
        iter_normal_ensures mark kf
    | _ -> ()
  in
  Annotations.iter_all_code_annot mark_code_annot;
  Visitor.visitFramacFileFunctions mark_preconditions (Ast.get ());
  Globals.Functions.iter mark_postconditions

let c_labels kf cs =
  let module LabelMap = Cil_datatype.Logic_label.Map in
  if Function_calls.use_spec_instead_of_definition kf then
    LabelMap.empty
  else
    let fdec = Kernel_function.get_definition kf in
    let aux acc stmt =
      if stmt.labels != [] then
        let request = Results.(before stmt |> in_callstack cs) in
        match Results.get_cvalue_model_result request with
        | Error _ -> acc
        | Ok state -> LabelMap.add (StmtLabel (ref stmt)) state acc
      else acc
    in
    List.fold_left aux LabelMap.empty fdec.sallstmts

(* Evaluates [p] at [stmt], using per callstack states for maximum precision. *)
(* TODO: we can probably factor some code with the GUI *)
let eval_by_callstack kf stmt p =
  (* This is actually irrelevant for alarms: they never use \old *)
  let pre = Results.(at_start_of kf |> get_cvalue_model_result) in
  match pre with
  | Error (Bottom | Top | DisabledDomain) -> Unknown
  | Ok pre ->
    let requests = Results.(before stmt |> by_callstack) in
    let aux_callstack acc_status (callstack, request) =
      let state = Results.get_cvalue_model request in
      let c_labels = c_labels kf callstack in
      let env = Eval_terms.env_annot ~c_labels ~pre ~here:state () in
      let status = Eval_terms.eval_predicate env p in
      let join = Eval_terms.join_predicate_status in
      match Bottom.join join acc_status (`Value status) with
      | `Value Unknown -> raise Exit (* shortcut *)
      | _ as r -> r
    in
    try
      match List.fold_left aux_callstack `Bottom requests with
      | `Bottom -> Eval_terms.Unknown (* probably never reached *)
      | `Value status -> status
    with Exit -> Eval_terms.Unknown

(* Detection of terms \at(_, L) where L is a C label *)
class contains_c_at = object
  inherit Visitor.frama_c_inplace

  method! vterm t = match t.term_node with
    | Tat (_, StmtLabel _) -> raise Exit
    | _ -> Cil.DoChildren
end

let contains_c_at ca =
  let vis = new contains_c_at in
  try
    ignore (Visitor.visitFramacCodeAnnotation vis ca);
    false
  with Exit -> true

(* Re-evaluate all alarms, and see if we can put a 'green' or 'red' status,
   which would be more precise than those we have emitted during the current
   analysis. *)
let mark_green_and_red () =
  let do_code_annot stmt _e ca  =
    let is_alarm = Alarms.find ca <> None in
    (* We reevaluate only alarms, in the hope that we can emit an 'invalid'
       status, or user assertions that mention a C label. The latter are
       currently skipped during evaluation. *)
    if (is_alarm || contains_c_at ca) && Results.is_reachable stmt then
      match ca.annot_content with
      | AAssert (_, p) | AInvariant (_, true, p) ->
        let p = p.tp_statement in
        let loc = code_annotation_loc ca stmt in
        let open Current_loc.Operators in
        let<> UpdatedCurrentLoc = loc in
        let kf = Kernel_function.find_englobing_kf stmt in
        let ip = Property.ip_of_code_annot_single kf stmt ca in
        (* This status is exact: we are _not_ refining the statuses previously
           emitted, but writing a synthetic more precise status. *)
        let distinct = false in
        let emit status =
          let status, text_status = match status with
            | `True -> Property_status.True, "valid"
            | `False -> Property_status.False_if_reachable, "invalid"
          in
          Property_status.emit ~distinct Eva_utils.emitter ~hyps:[] ip status;
          let source = fst loc in
          let text_ca = code_annotation_text ca in
          Self.result ~level:3 ~once:true ~source "%s%a got final status %s."
            text_ca Description.pp_named p text_status;
        in
        begin
          match eval_by_callstack kf stmt p with
          | Eval_terms.False -> emit `False
          | Eval_terms.True ->
            (* Should not happen for an alarm that has been emitted during this
               analysis. However, this is possible for an 'old' alarm. *)
            emit `True
          | Eval_terms.Unknown -> ()
        end
      | AInvariant (_, false, _) | AStmtSpec _ | AVariant _ | AAssigns _
      | AAllocation _ | AExtended _ -> ()
  in
  Annotations.iter_all_code_annot do_code_annot

(* Special evaluation for the alarms on the very first statement of the
   main function. We put 'Invalid' statuses on them using this function. *)
let mark_invalid_initializers () =
  let kf = fst (Globals.entry_point ()) in
  let first_stmt = Kernel_function.find_first_stmt kf in
  let do_code_annot _e ca  =
    match Alarms.find ca with (* We only check alarms *)
    | None -> ()
    | Some _ ->
      match ca.annot_content with
      | AAssert (_, p) ->
        let p = p.tp_statement in
        let ip = Property.ip_of_code_annot_single kf first_stmt ca in
        (* Evaluate in a fully empty state. Only predicates that do not
           depend on the memory will result in 'False' *)
        let bot = Cvalue.Model.bottom in
        let env = Eval_terms.env_annot ~pre:bot ~here:bot () in
        begin match Eval_terms.eval_predicate env p with
          | True | Unknown -> ()
          | False ->
            let status = Property_status.False_and_reachable in
            let distinct = false (* see comment in mark_green_and_red above *) in
            Red_statuses.add_red_property Kglobal ip;
            Property_status.emit ~distinct Eva_utils.emitter ~hyps:[] ip status;
        end
      | _ -> ()
  in
  Annotations.iter_code_annot do_code_annot first_stmt
