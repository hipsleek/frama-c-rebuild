(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Utility functions for generating C implementations. *)

open Cil_types
open Analyses_types
module Error = Translation_error

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

module Translate_terms = struct
  let to_exp_ref
    : (adata:Assert.t ->
       ?inplace:bool ->
       kernel_function ->
       Env.t ->
       term ->
       exp * Assert.t * Env.t) ref
    = ref @@ fun ~adata:_ ?inplace:_ _kf _env _t -> Extlib.mk_labeled_fun "Translate_utils.Translate_terms.to_exp_ref"
  let to_exp ~adata ?inplace kf env t = !to_exp_ref ~adata ?inplace kf env t
end

let predicate_to_exp_ref
  : (adata:Assert.t ->
     ?name:string ->
     kernel_function ->
     ?rte:bool ->
     Env.t ->
     predicate ->
     exp * Assert.t * Env.t) ref
  =
  ref (fun ~adata:_ ?name:_ _kf ?rte:_ _env _p ->
      Extlib.mk_labeled_fun "predicate_to_exp_ref")

let predicate_to_exp ~adata ?name kf ?rte env p =
  !predicate_to_exp_ref ~adata ?name kf ?rte env p

(**************************************************************************)
(********************** Utility functions *********************************)
(**************************************************************************)

let must_translate ppt =
  Options.Valid.get ()
  || match Property_status.get ppt with
  | Never_tried
  | Inconsistent _
  | Best ((False_if_reachable | False_and_reachable | Dont_know), _) ->
    true
  | Best (True, _) ->
    (* [TODO] generating code for "valid under hypotheses" properties could be
       useful for some use cases (in particular, when E-ACSL does not stop on
       the very first error).
       ==> introduce a new option or modify the behavior of -e-acsl-valid,
       see e-acsl#35 *)
    false

let must_translate_opt = function
  | None -> false
  | Some ppt -> must_translate ppt

let () =
  E_acsl_visitor.must_translate_ppt_ref := must_translate;
  E_acsl_visitor.must_translate_ppt_opt_ref := must_translate_opt

let gmp_to_sizet ~adata ~loc ~name ?(check_lower_bound=true) ?pp kf env t =
  let logic_env = Env.Logic_env.get env in
  let pp = match pp with Some size_pp -> size_pp | None -> t in
  let sizet = Machine.sizeof_type () in
  let stmts = [] in
  (* Lower guard *)
  let stmts, env =
    if check_lower_bound then begin
      let zero_term = Cil.lzero ~loc () in
      let pred_name = Format.sprintf "%s_greater_or_eq_than_0" name in
      let lower_guard_pp =
        Logic_const.prel ~loc ~names:[pred_name] (Rge, pp, zero_term)
      in
      let lower_guard = Logic_const.prel ~loc (Rge, t, zero_term) in
      Typing.preprocess_predicate ~logic_env lower_guard;
      let adata_lower_guard, env = Assert.empty ~loc kf env in
      let lower_guard, adata_lower_guard, env =
        predicate_to_exp ~adata:adata_lower_guard kf env lower_guard
      in
      let assertion, env =
        Assert.runtime_check
          ~adata:adata_lower_guard
          ~pred_kind:Assert
          RTE
          kf
          env
          lower_guard
          lower_guard_pp
      in
      assertion :: stmts, env
    end
    else stmts, env
  in
  (* Upper guard *)
  let sizet_max =
    Logic_const.tint ~loc (Cil.max_unsigned_number (Cil.bitsSizeOf sizet))
  in
  let pred_name = Format.sprintf "%s_lesser_or_eq_than_SIZE_MAX" name in
  let upper_guard_pp =
    Logic_const.prel ~loc ~names:[pred_name] (Rle, pp, sizet_max)
  in
  let upper_guard = Logic_const.prel ~loc (Rle, t, sizet_max) in
  Typing.preprocess_predicate ~logic_env upper_guard;
  let adata_upper_guard, env = Assert.empty ~loc kf env in
  let upper_guard, adata_upper_guard, env =
    predicate_to_exp ~adata:adata_upper_guard kf env upper_guard
  in
  let assertion, env =
    Assert.runtime_check
      ~adata:adata_upper_guard
      ~pred_kind:Assert
      RTE
      kf
      env
      upper_guard
      upper_guard_pp
  in
  let stmts = assertion :: stmts in
  (* Translate term [t] into an exp of type [size_t] *)
  let gmp_e, adata, env = Translate_terms.to_exp ~adata kf env t in
  let  _, e, env = Env.new_var
      ~loc
      ~name:"size"
      env
      kf
      None
      sizet
      (fun vi _ ->
         let rtl_call =
           Smart_stmt.rtl_call ~loc
             ~result:(Cil.var vi)
             ~prefix:""
             "__gmpz_get_ui"
             [ gmp_e ]
         in
         List.rev (rtl_call :: stmts))
  in
  e, adata, env

let () =
  Memory_translate.gmp_to_sizet_ref := gmp_to_sizet

let comparison_to_exp
    ~loc kf env ity bop e1 e2 ?(name = Misc.name_of_binop bop) t_opt =
  let ty1 = Cil.typeOf e1 in
  let ty2 = Cil.typeOf e2 in
  let e, env =
    match Logic_aggr.get_t ty1, Logic_aggr.get_t ty2 with
    | Logic_aggr.Array, Logic_aggr.Array ->
      Logic_array.comparison_to_exp
        ~loc
        kf
        env
        ~name
        bop
        e1
        e2
    | Logic_aggr.StructOrUnion, Logic_aggr.StructOrUnion ->
      Env.not_yet env "comparison between two structs or unions"
    | Logic_aggr.NotAggregate, Logic_aggr.NotAggregate -> begin
        match ity with
        | C_integer _ | C_float _ | Nan ->
          Misc.make_binop ~loc bop e1 e2, env
        | Gmpz -> Gmp.Z.cmp ~loc name t_opt bop env kf e1 e2
        | Rational -> Gmp.Q.cmp ~loc name t_opt bop env kf e1 e2
        | Real -> Error.not_yet "comparison involving real numbers"
      end
    | _, _ ->
      Options.fatal
        ~current:true
        "Comparison involving incompatible types: '%a' and '%a'"
        Printer.pp_typ ty1
        Printer.pp_typ ty2
  in
  e, env

let () =
  Logic_array.Translate_utils.comparison_to_exp_ref := comparison_to_exp

let conditional_to_exp ?(name="if") ~loc kf t_opt e1 (e2, env2) (e3, env3) =
  let env = Env.pop (Env.pop env3) in
  match e1.enode with
  | Const(CInt64(n, _, _)) when Z.is_zero n ->
    e3, Env.transfer ~from:env3 env
  | Const(CInt64(n, _, _)) when Z.is_one n ->
    e2, Env.transfer ~from:env2 env
  | _ ->
    let ty = match t_opt with
      | None (* predicate *) -> Cil_const.intType
      | Some t -> Typing.get_typ ~logic_env:(Env.Logic_env.get env) t
    in
    let _, e, env =
      Env.new_var
        ~loc
        ~name
        env
        kf
        t_opt
        ty
        (fun v ev ->
           let lv = Cil.var v in
           let ty = Cil.typeOf ev in
           let init_set =
             assert (not (Gmp_types.Q.is_t ty));
             Gmp.init_set
           in
           let affect e = init_set ~loc lv ev e in
           let then_blk, _ =
             let s = affect e2 in
             Env.pop_and_get ~kf env2 s ~global_clear:false Env.Middle
           in
           let else_blk, _ =
             let s = affect e3 in
             Env.pop_and_get ~kf env3 s ~global_clear:false Env.Middle
           in
           [ Smart_stmt.if_stmt ~loc ~cond:e1 then_blk ~else_blk ])
    in
    e, env

let env_of_li ~adata ~loc kf env li =
  match li.l_var_info.lv_type with
  | Ctype _ | Linteger | Lreal ->
    let t = Misc.term_of_li li in
    let logic_env = Env.Logic_env.get env in
    let ty = Typing.get_typ ~logic_env t in
    let vi, vi_e, env = Env.Logic_binding.add ~ty env kf li.l_var_info in
    let e, adata, env = Translate_terms.to_exp ~adata kf env t in
    let stmt = match Typing.get_number_ty ~logic_env t with
      | C_integer _ | C_float _ | Nan ->
        Smart_stmt.assigns ~loc ~result:(Cil.var vi) e
      | Gmpz | Rational -> Gmp.init_set ~loc (Cil.var vi) vi_e e
      | Real -> Error.not_yet "real number"
    in
    adata, Env.add_stmt env stmt
  | Lboolean ->
    Env.not_yet env "booleans"
  | Ltype _ ->
    Env.not_yet env "user-defined logic type"
  | Lvar _ ->
    Env.not_yet env "type variable"
  | Larrow _ ->
    Env.not_yet env "lambda-abstraction"
