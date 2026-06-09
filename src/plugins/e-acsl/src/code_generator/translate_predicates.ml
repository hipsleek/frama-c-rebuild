(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Generate C implementations of E-ACSL predicates. *)

open Cil_types
open Cil_datatype
open Analyses_types
let dkey = Options.Dkey.translation

module IL = struct
  include Interlang
end
module M = Interlang_gen.M
open M.Operators

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

module Translate_rtes = struct
  let translate_rte_annots_ref :
    ((Format.formatter -> code_annotation -> unit) ->
     code_annotation ->
     kernel_function ->
     Env.t ->
     code_annotation list ->
     Env.t) ref =
    ref (fun _pp _elt _kf _env _l ->
        Extlib.mk_labeled_fun "translate_rte_annots_ref")

  let translate_rte_annots pp elt kf env l = !translate_rte_annots_ref pp elt kf env l

  let translate_rte_exp_ref :
    (?filter:(code_annotation -> bool) ->
     kernel_function ->
     Env.t ->
     exp ->
     Env.t) ref =
    ref (fun ?filter:_ _kf _env _e ->
        Extlib.mk_labeled_fun "translate_rte_exp_ref")

  let translate_rte_exp ?filter kf env e = !translate_rte_exp_ref ?filter kf env e
end

(* ************************************************************************** *)
(* Transforming predicates into C expressions (if any) *)
(* ************************************************************************** *)

let relation_to_binop = function
  | Rlt -> Lt
  | Rgt -> Gt
  | Rle -> Le
  | Rge -> Ge
  | Req -> Eq
  | Rneq -> Ne

let predicate_content_to_exp_il p =
  let p = Logic_normalizer.get_pred p in
  match p.pred_content with
  | Ptrue -> M.return @@ IL.Exp.mk_true ()
  | Pfalse -> M.return @@ IL.Exp.mk_false ()
  | Prel(rel, t1, t2) ->
    let* logic_env = M.read_logic_env in
    let t1 = Logic_normalizer.get_term t1 in
    let t2 = Logic_normalizer.get_term t2 in
    let ity =
      Typing.join
        (Typing.get_effective_ty ~logic_env t1)
        (Typing.get_effective_ty ~logic_env t2)
    in
    let rel = Interlang_gen.of_relation rel in
    let* op1 = Translate_terms.to_exp_il t1 in
    let* op2 = Translate_terms.to_exp_il t2 in
    M.return @@ Interlang.Exp.binop rel ity op1 op2
  | _ -> M.not_covered Printer.pp_predicate p

(* Convert an ACSL predicate into a corresponding C expression (if any) in the
   given environment. Also extend this environment which includes the generating
   constructs.
   If [inplace] is true, then the root predicate is immediately translated
   regardless of its label. Otherwise [Translate_ats] is used to retrieve the
   translation. *)
let rec predicate_content_to_exp_old ?(inplace=false) ?name ~loc ~adata ~env ~kf p =
  let p = Logic_normalizer.get_pred p in
  let logic_env = Env.Logic_env.get env in
  let open Current_loc.Operators in
  let<> UpdatedCurrentLoc = loc in
  let of_bool = function true -> Cil.one ~loc | false -> Cil.zero ~loc in
  match p.pred_content with
  | Pfalse -> Cil.zero ~loc, adata, env
  | Ptrue -> Cil.one ~loc, adata, env
  | Papp (li, labels, args) when Misc.labels_are_all_here labels ->
    let e, adata, env = Logic_functions.app_to_exp ~adata ~loc kf env li args in
    let adata = Assert.register_pred ~loc env p e adata in
    if Logic_normalizer.predicate_is_unsound_if_false li then
      let cond = Smart_exp.lnot ~loc e in
      let then_blk = Cil.mkBlock [Smart_stmt.set_unsound_verdict ~loc] in
      let env = Env.add_stmt env @@ Smart_stmt.if_stmt ~loc ~cond then_blk in
      e, adata, env
    else
      e, adata, env
  | Papp (_, _,_) -> Env.not_yet env "predicates with labels"
  | Pdangling _ -> Env.not_yet env "\\dangling"
  | Pvalid_function _ -> Env.not_yet env "\\valid_function"
  | Prel(rel, t1, t2) ->
    let t1 = Logic_normalizer.get_term t1 in
    let t2 = Logic_normalizer.get_term t2 in
    let ity =
      Typing.join
        (Typing.get_effective_ty ~logic_env t1)
        (Typing.get_effective_ty ~logic_env t2)
    in
    let e1, adata, env = Translate_terms.to_exp ~adata kf env t1 in
    let e2, adata, env = Translate_terms.to_exp ~adata kf env t2 in
    let e, env = Translate_utils.comparison_to_exp
        ~loc
        kf
        env
        ity
        (relation_to_binop rel)
        e1
        e2
        None
    in
    e, adata, env
  | Pand(p1, p2) ->
    (* p1 && p2 <==> if p1 then p2 else false *)
    Extlib.flatten @@ Env.with_params_and_result ~rte:true ~env (fun env ->
        let e1, adata, env1 = to_exp ~adata kf env p1 in
        let e2, adata, env2 =
          to_exp ~adata kf (Env.push env1) p2 in
        let res2 = e2, env2 in
        let env3 = Env.push env2 in
        let name = match name with None -> "and" | Some n -> n in
        Extlib.nest
          adata
          (Translate_utils.conditional_to_exp
             ~name
             ~loc
             kf
             None
             e1
             res2
             (Cil.zero ~loc, env3))
      )
  | Por(p1, p2) ->
    (* p1 || p2 <==> if p1 then true else p2 *)
    Extlib.flatten @@ Env.with_params_and_result ~rte:true ~env (fun env ->
        let e1, adata, env1 = to_exp ~adata kf env p1 in
        let env' = Env.push env1 in
        let e2, adata, env2 =
          to_exp ~adata kf (Env.push env') p2
        in
        let res2 = e2, env2 in
        let name = match name with None -> "or" | Some n -> n in
        Extlib.nest
          adata
          (Translate_utils.conditional_to_exp
             ~name
             ~loc
             kf
             None
             e1
             (Cil.one ~loc, env')
             res2)
      )
  | Pxor _ -> Env.not_yet env "xor"
  | Pimplies(p1, p2) ->
    let rewritten = (* (p1 ==> p2) <==> !p1 || p2 *)
      Logic_const.por ~loc ((Logic_const.pnot ~loc p1), p2)
    in
    Typing.preprocess_predicate ~logic_env rewritten;
    to_exp ~adata ~name:"implies" kf env rewritten
  | Piff(p1, p2) ->
    let rewritten = (* (p1 <==> p2) <==> (p1 ==> p2 && p2 ==> p1) *)
      Logic_const.pand ~loc
        (Logic_const.pimplies ~loc (p1, p2),
         Logic_const.pimplies ~loc (p2, p1))
    in
    Typing.preprocess_predicate ~logic_env rewritten;
    to_exp ~adata ~name:"equiv" kf env rewritten
  | Pnot p ->
    let e, adata, env = to_exp ~adata kf env p in
    Smart_exp.lnot ~loc e, adata, env
  | Pif(c, p2, p3) ->
    Extlib.flatten @@ Env.with_params_and_result ~rte:true ~env (fun env ->
        let e1, adata, env1 = to_exp ~adata kf env c in
        let e2, adata, env2 =
          to_exp ~adata kf (Env.push env1) p2 in
        let res2 = e2, env2 in
        let e3, adata, env3 =
          to_exp ~adata kf (Env.push env2) p3
        in
        let res3 = e3, env3 in
        Extlib.nest
          adata
          (Translate_utils.conditional_to_exp ~loc kf None e1 res2 res3)
      )
  | Plet(li, p) ->
    (* Translate the term registered to the \let logic variable *)
    let adata, env = Translate_utils.env_of_li ~adata ~loc kf env li in
    (* Register the logic var to the logic scope *)
    let lvs = Lvs_let(li.l_var_info, Misc.term_of_li li) in
    let env = Env.Logic_scope.extend env lvs in
    (* Translate the body of the \let *)
    let e, adata, env = to_exp ~adata kf env p in
    (* Remove the logic var from the logic scope *)
    let env = Env.Logic_scope.remove env lvs in
    e, adata, env
  | Pforall _ | Pexists _ ->
    let e, env = Quantif.quantif_to_exp kf env p in
    let adata = Assert.register_pred ~loc env p e adata in
    e, adata, env
  | Pat(p', label) ->
    if inplace || label = BuiltinLabel Here then
      to_exp ~adata kf env p'
    else
      Translate_ats.to_exp ~loc ~adata kf env (PoT_pred p) label
  | Pvalid_read(BuiltinLabel Here, t) as pc
  | (Pvalid(BuiltinLabel Here, t) as pc)
  | (Pobject_pointer(BuiltinLabel Here, t) as pc) ->
    begin
      match pc, Memory_tracking.SpecialPointers.pointer_of_term t with
      (* static resolution: \valid(stdout) ≡ 1; \valid(stdin) ≡ 0; etc. *)
      | Pvalid _, Some spec -> of_bool spec.writeable, adata, env
      (* static resolution: \valid_read(stdin) ≡ 1; \valid_read(&errno) ≡ 1; etc. *)
      | Pvalid_read _, Some _spec -> of_bool true, adata, env
      | _ ->
        let call_valid ~adata p =
          let e, adata, env =
            Memory_translate.call_valid ~adata ~loc kf Cil_const.intType env p
          in
          let adata = Assert.register_pred ~loc env p e adata in
          e, adata, env
        in
        (* we already transformed \valid(t) into \initialized(&t) && \valid(t):
           now convert this right-most valid. *)
        call_valid ~adata p
    end
  | Pvalid _ -> Env.not_yet env "labeled \\valid"
  | Pvalid_read _ -> Env.not_yet env "labeled \\valid_read"
  | Pobject_pointer _ -> Env.not_yet env "labeled \\object_pointer"
  | Pseparated tlist ->
    let env =
      List.fold_left
        (fun env t ->
           let name = "separated_guard" in
           let p =
             Logic_const.pvalid_read ~loc ~names:[name] (BuiltinLabel Here, t)
           in
           let tp = Logic_const.toplevel_predicate ~kind:Assert p in
           let annot = Logic_const.new_code_annotation (AAssert ([],tp)) in
           Typing.preprocess_rte ~logic_env:(Env.Logic_env.get env) annot;
           Translate_rtes.translate_rte_annots
             Printer.pp_code_annotation
             annot
             kf
             env
             [annot]
        )
        env
        tlist
    in
    let e, adata, env =
      Memory_translate.call_with_size
        ~adata
        ~loc
        kf
        Cil_const.intType
        env
        p
    in
    let adata = Assert.register_pred ~loc env p e adata in
    e, adata, env
  | Paligned (ptr, align) ->
    let (align_e, adata), env =
      Env.with_params_and_result ~rte:false ~env (fun env ->
          let align_e, adata, env =
            Translate_terms.to_exp ~adata kf env align
          in
          (align_e, adata), env
        )
    in
    let (ptr_e, adata), env =
      Env.with_params_and_result ~rte:false ~env (fun env ->
          let ptr_e, adata, env = Translate_terms.to_exp ~adata kf env ptr in
          (ptr_e, adata), env
        )
    in
    let e, env =
      Memory_translate.call
        ~loc
        kf
        "aligned"
        Cil_const.intType
        env
        [ptr_e; align_e]
    in
    let adata = Assert.register_pred ~loc env p e adata in
    e, adata, env
  | Pinitialized(BuiltinLabel Here, t) ->
    begin
      match Memory_tracking.SpecialPointers.pointer_of_term t with
      (* static resolutions: \initialized(stdout) ≡ 1; etc. *)
      | Some spec -> of_bool spec.initialized, adata, env
      | None ->
        match t.term_node with
        (* optimisation when we know that the initialisation is ok *)
        | TAddrOf (TResult _, TNoOffset) -> Cil.one ~loc, adata, env
        | TAddrOf (TVar { lv_origin = Some vi }, TNoOffset)
          when
            vi.vformal || vi.vglob || Functions.RTL.is_generated_name vi.vname ->
          Cil.one ~loc, adata, env
        | _ ->
          let e, adata, env =
            Memory_translate.call_with_size
              ~adata
              ~loc
              kf
              Cil_const.intType
              env
              p
          in
          let adata = Assert.register_pred ~loc env p e adata in
          e, adata, env
    end
  | Pinitialized _ -> Env.not_yet env "labeled \\initialized"
  | Pallocable _ -> Env.not_yet env "\\allocate"
  | Pfreeable(BuiltinLabel Here, t) -> begin
      match Memory_tracking.SpecialPointers.pointer_of_term t with
      (* static resolutions: \freeable(stdout) ≡ 0; etc. *)
      | Some spec -> of_bool spec.freeable, adata, env
      | None ->
        let (t_exp, adata), env =
          Env.with_params_and_result ~rte:true ~env (fun env ->
              let t_exp, adata, env = Translate_terms.to_exp ~adata kf env t in
              (t_exp, adata), env
            )
        in
        let e, env =
          Memory_translate.call ~loc kf "freeable" Cil_const.intType env [t_exp]
        in
        let adata = Assert.register_pred ~loc env p e adata in
        e, adata, env
    end
  | Pfreeable _ -> Env.not_yet env "labeled \\freeable"
  | Pfresh _ -> Env.not_yet env "\\fresh"

and predicate_content_to_exp ~adata ?inplace ?name kf env p =
  let loc = p.pred_loc in
  Interlang_trans.try_il_compiler ~loc ~adata ~env ~kf
    predicate_content_to_exp_il
    (predicate_content_to_exp_old ?inplace ?name)
    p

and to_exp_old ~rte ~loc:_ ?inplace ?name ~adata ~env ~kf p =
  Extlib.flatten @@ Env.with_params_and_result ~rte:false ~env (fun env ->
      let e, adata, env =
        predicate_content_to_exp ?inplace ~adata ?name kf env p
      in
      let env = if rte then Translate_rtes.translate_rte_exp kf env e else env in
      (e, adata), env)

and to_exp_il ~rte p =
  if rte
  then M.not_covered ~pre:"with RTE" Printer.pp_predicate p
  else predicate_content_to_exp_il p

(** [to_exp ~adata ?inplace ?name kf ?rte env p] converts an ACSL predicate into
    a corresponding C expression.
    - [adata]: assertion context.
    - [inplace]: if the root predicate has a label, indicates if it should be
      immediately translated or if [Translate_ats] should be used to retrieve
      the translation.
    - [name]: name to use for generated variables.
    - [kf]: the enclosing function.
    - [rte]: if true, generate and translate RTE before translating the
      predicate.
    - [env]: the current environment.
    - [p]: the predicate to translate. *)
and to_exp ~adata ?inplace ?name kf ?rte env p =
  let open Current_loc.Operators in
  let loc = p.pred_loc in
  let<> UpdatedCurrentLoc = loc in
  Assert.push_pending_register_data();
  let rte = match rte with None -> Env.generate_rte env | Some b -> b in
  let e, adata, env =
    Interlang_trans.try_il_compiler ~loc ~adata ~env ~kf
      (to_exp_il ~rte)
      (to_exp_old ~rte ?name ?inplace)
      p
  in
  let env = Assert.do_pending_register_data env in
  e, adata, env

let generalized_untyped_to_exp ~adata ?name kf ?rte env p =
  (* If [rte] is true, it means we're translating the root predicate of an
     assertion and we need to generate the RTE for it. The typing environment
     must be cleared. Otherwise, if [rte] is false, it means we're already
     translating RTE predicates as part of the translation of another root
     predicate, and the typing environment must be kept. *)
  let rte = match rte with None -> Env.generate_rte env | Some b -> b in
  let e, adata, env = to_exp ~adata ?name kf ~rte env p in
  assert (Typ.equal (Cil.typeOf e) Cil_const.intType);
  let env = Env.Logic_scope.reset env in
  e, adata, env

let do_it kf env p =
  match p.tp_kind with
  | Assert | Check ->
    Options.feedback ~dkey ~level:3 "translating predicate %a"
      Printer.pp_toplevel_predicate p;
    let adata, env = Assert.empty ~loc:p.tp_statement.pred_loc kf env in
    let e, adata, env =
      generalized_untyped_to_exp ~adata kf env p.tp_statement
    in
    let stmt, env =
      Assert.runtime_check
        ~adata
        ~pred_kind:p.tp_kind
        (Env.annotation_kind env)
        kf
        env
        e
        p.tp_statement
    in
    Env.add_stmt env stmt
  | Admit -> env

let rte_guards_to_exp_il t =
  Rte_analysis.fold_guards_il ~default:(M.return []) t @@ fun p guards ->
  let* e : IL.rte = M.map (Interlang.Rte.make p) @@ to_exp_il ~rte:true p in
  let* guards = guards in
  M.return (e :: guards)

let rte_guards_to_exp_old ~loc ~kf t env =
  Rte_analysis.fold_guards_old ~default:env t @@ fun p env ->
  Assert.push_pending_register_data ();
  let adata, env = Assert.empty ~loc kf env in
  let cil, adata, env = to_exp ~adata ~rte:true kf env p in
  let stmt, env = Assert.runtime_check
      ~adata
      ~pred_kind:Assert
      RTE
      kf
      env
      cil
      p
  in
  let env = Assert.do_pending_register_data env in
  let env = Env.add_stmt ~annot:p env stmt in
  env

let predicate_to_exp_without_rte ~adata kf env p =
  (* forget optional argument ?rte and ?name*)
  to_exp ~adata kf env p

let predicate_to_exp_without_inplace ~adata ?name kf ?rte env p =
  to_exp ~adata ?name kf ?rte env p

let () =
  Translate_utils.predicate_to_exp_ref := predicate_to_exp_without_inplace;
  Translate_ats.predicate_to_exp_ref := to_exp;
  Loops.translate_predicate_ref := do_it;
  Loops.predicate_to_exp_ref := predicate_to_exp_without_rte;
  Quantif.predicate_to_exp_ref := predicate_to_exp_without_rte;
  Memory_translate.predicate_to_exp_ref := predicate_to_exp_without_rte;
  Logic_functions.predicate_to_exp_ref := predicate_to_exp_without_rte;
  Translate_terms.Translate_predicates.rte_guards_to_exp_old_ref :=
    rte_guards_to_exp_old;
  Translate_terms.Translate_predicates.rte_guards_to_exp_il_ref :=
    rte_guards_to_exp_il;
  Translate_terms.Translate_predicates.to_exp_ref := to_exp

exception No_simple_translation of predicate

(* This function is used by Guillaume.
   However, it is correct to use it only in specific contexts. *)
let untyped_to_exp p =
  let env = Env.push Env.empty in
  let env = Env.set_rte env false in
  let e, _, env =
    try generalized_untyped_to_exp
          ~adata:Assert.no_data
          Cil_datatype.Kf.dummy
          env
          p
    with Rtl.Symbols.Unregistered _ -> raise (No_simple_translation p)
  in
  if not (Env.has_no_new_stmt env)
  then raise (No_simple_translation p);
  e
