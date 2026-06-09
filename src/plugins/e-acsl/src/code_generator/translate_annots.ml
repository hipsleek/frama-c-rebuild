(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

(* ************************************************************************** *)
(* Functions that translate a given ACSL annotation into the corresponding C
   statements (if any) for runtime assertion checking. *)
(* ************************************************************************** *)

let pre_funspec kf env funspec =
  let kinstr = Kglobal in
  let unsupported f x = ignore (Env.handle_error (fun env -> f x; env) env) in
  let convert_unsupported_clauses env =
    unsupported
      (fun spec ->
         let ppt = Property.ip_decreases_of_spec kf kinstr spec in
         if Translate_utils.must_translate_opt ppt
         then Env.not_yet env "decreases clause")
      funspec;
    (* TODO: spec.spec_terminates is not part of the E-ACSL subset *)
    unsupported
      (fun spec ->
         let ppt = Property.ip_terminates_of_spec kf kinstr spec in
         if Translate_utils.must_translate_opt ppt
         then Env.not_yet env "terminates clause")
      funspec;
    env
  in
  let loc = Kernel_function.get_location kf in
  let env = convert_unsupported_clauses env in
  let contract = Contract.create ~loc funspec in
  Env.with_params ~rte:true ~kinstr ~env
    (fun env -> Contract.translate_preconditions kf env contract)

let post_funspec kf env =
  Env.with_params ~rte:true ~kinstr:Kglobal ~env
    (fun env -> Contract.translate_postconditions kf env)

let pre_code_annotation kf stmt env annot =
  let kinstr = Kstmt stmt in
  let convert env = match annot.annot_content with
    | AAssert(l, p) ->
      if Translate_utils.must_translate
          (Property.ip_of_code_annot_single kf stmt annot) then
        let env = Env.set_annotation_kind env Assertion in
        if l <> [] then
          Env.not_yet env "@[assertion applied only on some behaviors@]";
        Env.with_params ~rte:true ~kinstr ~env
          (fun env -> Translate_predicates.do_it kf env p)
      else
        env
    | AStmtSpec(l, spec) ->
      if l <> [] then
        Env.not_yet env "@[statement contract applied only on some behaviors@]";
      let loc = Stmt.loc stmt in
      let contract = Contract.create ~loc spec in
      Env.with_params ~rte:true ~kinstr ~env
        (fun env -> Contract.translate_preconditions kf env contract)
    | AInvariant(l, loop_invariant, p) ->
      let open Current_loc.Operators in
      let<> UpdatedCurrentLoc = p.tp_statement.pred_loc in
      if Translate_utils.must_translate
          (Property.ip_of_code_annot_single kf stmt annot) then
        let env = Env.set_annotation_kind env Invariant in
        if l <> [] then
          Env.not_yet env "@[invariant applied only on some behaviors@]";
        let env =
          Env.with_params ~rte:true ~kinstr ~env
            (fun env -> Translate_predicates.do_it kf env p)
        in
        if loop_invariant then
          Env.add_loop_invariant env p
        else env
      else
        env
    | AVariant (t, measure) ->
      let open Current_loc.Operators in
      let<> UpdatedCurrentLoc = t.term_loc in
      if Translate_utils.must_translate
          (Property.ip_of_code_annot_single kf stmt annot)
      then Env.set_loop_variant env ?measure t
      else env
    | AAssigns _ ->
      (* TODO: it is not a precondition --> should not be handled here,
         to be fixed when implementing e-acsl#29 *)
      let ppts = Property.ip_of_code_annot kf stmt annot in
      List.iter
        (fun ppt ->
           if Translate_utils.must_translate ppt
           then Env.not_yet env "assigns")
        ppts;
      env
    | AAllocation _ ->
      let ppts = Property.ip_of_code_annot kf stmt annot in
      List.iter
        (fun ppt ->
           if Translate_utils.must_translate ppt
           then Env.not_yet env "allocation")
        ppts;
      env
    | AExtended _ -> env (* never translate extensions. *)
  in
  Env.handle_error convert env

let post_code_annotation kf stmt env annot =
  let kinstr = Kstmt stmt in
  let convert env = match annot.annot_content with
    | AStmtSpec(_, _) ->
      Env.with_params ~rte:true ~kinstr ~env
        (fun env -> Contract.translate_postconditions kf env)
    | AAssert _
    | AInvariant _
    | AVariant _
    | AAssigns _
    | AAllocation _
    | AExtended _ -> env
  in
  Env.handle_error convert env
