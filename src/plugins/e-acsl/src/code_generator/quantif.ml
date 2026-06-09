(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Analyses_types

(** Forward reference for [Translate_predicates.to_exp]. *)
let predicate_to_exp_ref
  : (adata:Assert.t ->
     kernel_function ->
     Env.t ->
     predicate ->
     exp * Assert.t * Env.t) ref
  =
  ref (fun ~adata:_ _kf _env _p ->
      Extlib.mk_labeled_fun "predicate_to_exp_ref")


(* It could happen that the bounds provided for a quantified [lv] are empty
   in the sense that [min <= lv < max] but [min > max + 1]. In such cases, \true
   (or \false depending on the quantification) should be generated instead of
   nested loops.
   [has_empty_quantif_with_false_negative] partially detects such cases:
   Case 1: an empty quantification was detected for sure, return true.
   Case 2: we don't know, return false. *)
let rec has_empty_quantif_with_false_negative ~logic_env = function
  | [] ->
    (* case 2 *)
    false
  | (t1, _, t2) :: guards ->
    let bounds =
      let open Option.Operators in
      let* ival1 = Interval.get_ival ~logic_env t1 in
      let* ival2 = Interval.get_ival ~logic_env t2 in
      let* lower_bound = fst @@ Ival.min_and_max ival1 in
      let* upper_bound = snd @@ Ival.min_and_max ival2 in
      Some (lower_bound, upper_bound)
    in
    match bounds with
    | Some (lower_bound, upper_bound) ->
      let res  = lower_bound >= upper_bound in
      res (* case 1 *) ||
      has_empty_quantif_with_false_negative guards ~logic_env
    | None ->
      has_empty_quantif_with_false_negative guards ~logic_env

let () =
  Labels.has_empty_quantif_ref :=
    let logic_env = Analyses_datatype.Logic_env.empty in
    has_empty_quantif_with_false_negative ~logic_env
(* Since we do not support logic functions with labels, we do not need to
   pass an actual logic environment *)

module Label_ids =
  State_builder.Counter(struct let name = "E_ACSL.Label_ids" end)

let convert kf env loc ~is_forall quantif =
  (* guarded quantification over integers (or a subtype of integer) *)
  let bound_vars, goal = Bound_variables.get_preprocessed_quantifier quantif in
  let logic_env = Env.Logic_env.get env in
  let empty = has_empty_quantif_with_false_negative ~logic_env bound_vars in
  match empty, is_forall with
  | true, true ->
    Cil.one ~loc, env
  | true, false ->
    Cil.zero ~loc, env
  | false, _ ->
    begin
      (* create different "initial value", "found value" and guard expression
         for the predicate depending on the type of quantification *)
      let init_val, found_val, mk_guard =
        let z = Cil.zero ~loc in
        let o = Cil.one ~loc in
        if is_forall then o, z, fun x -> x
        else z, o, fun e -> Smart_exp.lnot ~loc:e.eloc e
      in
      (* transform [bound_vars] into [lscope_var list],
         and update logic scope and logic environment in the process *)
      let lvs_guards, env = List.fold_left
          (fun (lvs_guards, env) (t1, lv, t2) ->
             let lvs = Lvs_quantif (t1, Rle, lv, Rlt, t2) in
             let env = Env.Logic_scope.extend env lvs in
             let logic_env = Env.Logic_env.get env in
             let i = Interval.joins ~logic_env [t2; t1] in
             let env = Env.Logic_env.add env lv i in
             lvs :: lvs_guards, env)
          ([], env)
          bound_vars
      in
      let var_res, res, env =
        (* variable storing the result of the quantifier *)
        let name = if is_forall then "forall" else "exists" in
        Env.new_var
          ~loc
          ~name
          env
          kf
          None
          Cil_const.intType
          (fun v _ ->
             let lv = Cil.var v in
             [ Smart_stmt.assigns ~loc ~result:lv init_val ])
      in
      let end_loop_ref = ref Cil_datatype.Stmt.dummy in
      (* innermost block *)
      let mk_innermost_block env =
        (* innermost loop body: store the result in [res] and go out according
           to evaluation of the goal *)
        let predicate_to_exp = !predicate_to_exp_ref ~adata:Assert.no_data in
        let test, _, env = predicate_to_exp kf (Env.push env) goal in
        let then_blk = Cil.mkBlock [ Cil.mkEmptyStmt ~loc () ] in
        let else_blk =
          (* use a 'goto', not a simple 'break' in order to handle 'forall' with
             multiple binders (leading to imbricated loops) *)
          Cil.mkBlock
            [ Smart_stmt.assigns ~loc ~result:(Cil.var var_res) found_val;
              Cil.mkStmt ~valid_sid:true (Goto(end_loop_ref, loc)) ]
        in
        let blk, env =
          Env.pop_and_get ~kf
            env
            (Smart_stmt.if_stmt ~loc ~cond:(mk_guard test) then_blk ~else_blk)
            ~global_clear:false
            Env.After
        in
        let blk = Cil.flatten_transient_sub_blocks blk in
        [ Smart_stmt.block_stmt blk ], env
      in
      let stmts, env =
        Loops.mk_nested_loops ~loc mk_innermost_block kf env lvs_guards
      in
      let env =
        Env.add_stmt env (Smart_stmt.block_stmt (Cil.mkBlock stmts))
      in
      (* where to jump to go out of the loop *)
      let end_loop = Cil.mkEmptyStmt ~loc () in
      let label_name = "e_acsl_end_loop" ^ string_of_int (Label_ids.next ()) in
      let label = Label(label_name, loc, false) in
      end_loop.labels <- label :: end_loop.labels;
      end_loop_ref := end_loop;
      let env = Env.add_stmt env end_loop in
      let env =
        List.fold_left
          (fun env _ -> Env.Logic_env.pop env)
          env
          lvs_guards
      in
      res, env
    end

let quantif_to_exp kf env p =
  let loc = p.pred_loc in
  match p.pred_content with
  | Pforall _ ->
    convert kf env loc ~is_forall:true p
  | Pexists _ ->
    convert kf env loc ~is_forall:false p
  | _ -> assert false
