(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Generate C implementations of E-ACSL terms. *)

open Cil_types
open Analyses_types
open Analyses_datatype
let dkey = Options.Dkey.translation

module IL = struct
  include Interlang
end
module M = Interlang_gen.M
open Interlang_gen.M.Operators


(********************** Forward references ********************************)

let translate_rte_exp_ref
  : (?filter:(code_annotation -> bool) ->
     kernel_function ->
     Env.t ->
     exp ->
     Env.t) ref
  =
  ref (fun ?filter:_ _kf _env _e ->
      Extlib.mk_labeled_fun "translate_rte_exp_ref")

module Translate_predicates = struct
  let to_exp_ref :
    (adata:Assert.t ->
     ?inplace:bool ->
     ?name:string ->
     kernel_function ->
     ?rte:bool ->
     Env.t ->
     predicate ->
     exp * Assert.t * Env.t) ref
    = ref @@ fun ~adata:_ ?inplace:_ ?name:_ _kf ?rte:_ _env _p ->
    Extlib.mk_labeled_fun "Translate_terms.Translate_predicates.to_exp_ref"

  let to_exp ~adata ?inplace ?name kf ?rte env p =
    !to_exp_ref ~adata ?inplace ?name kf ?rte env p

  let rte_guards_to_exp_old_ref
    : (loc:location ->
       kf:kernel_function ->
       term ->
       Env.t ->
       Env.t) ref
    = ref (fun ~loc:_ ~kf:_ _data _env ->
        Extlib.mk_labeled_fun
          "Translate_terms.Translate_predicates.rte_guards_to_exp_old_ref")

  let rte_guards_to_exp_il_ref : (term -> IL.rte list M.t) ref
    = ref (fun _g ->
        Extlib.mk_labeled_fun
          "Translate_terms.Translate_predicates.rte_guards_to_exp_il_ref")

  let rte_guards_to_exp_old ~loc ~kf p env =
    !rte_guards_to_exp_old_ref ~loc ~kf p env

  let rte_guards_to_exp_il p = !rte_guards_to_exp_il_ref p
end

(* ************************************************************************** *)
(* Transforming terms into C expressions (if any) *)
(* ************************************************************************** *)

let constant_to_exp_il t c =
  match c with
  | Integer (n, _) ->
    let* logic_env = M.read_logic_env in
    let ity = Typing.get_number_ty ~logic_env t in
    M.return @@ IL.Exp.integer ~origin:t ~ity n
  | _ -> M.not_covered Printer.pp_term t

let constant_to_exp_old ~loc env t c =
  let mk_real s =
    let s = Gmp.Q.normalize_str s in
    Cil.mkAddrOrStartOf ~loc
      (Cil.var (Globals.Vars.add_string_literal ~loc (Str s))),
    Analyses_types.Str_R
  in
  match c with
  | Boolean b ->
    (if b then Cil.one ~loc else Cil.zero ~loc), Analyses_types.C_number
  | Integer(n, _repr) ->
    let logic_env = Env.Logic_env.get env in
    let ity = Typing.get_number_ty ~logic_env t in
    (match ity with
     | Nan -> assert false
     | Real -> Error.not_yet "real number constant"
     | Rational -> mk_real (Z.to_string n)
     | Gmpz ->
       Cil.mkAddrOrStartOf ~loc
         (Cil.var
            (Globals.Vars.add_string_literal ~loc
               (Str (Z.to_string n)))),
       Analyses_types.Str_Z
     (* too large integer *)
     | C_float fkind ->
       Cil.kfloat ~loc fkind (Int64.to_float (Z.to_int64 n)), C_number
     | C_integer kind ->
       let cast = Typing.get_cast ~logic_env t in
       match cast, kind with
       | Some ty, (ILongLong | IULongLong) when Gmp_types.Z.is_t ty ->
         (* too large integer *)
         Cil.mkAddrOrStartOf ~loc
           (Cil.var
              (Globals.Vars.add_string_literal ~loc
                 (Str (Z.to_string n)))),
         Analyses_types.Str_Z
       | Some ty, _ when Gmp_types.Q.is_t ty ->
         mk_real (Z.to_string n)
       | (None | Some _), _ ->
         (* do not keep the initial string representation because the generated
            constant must reflect its type computed by the type system. For
            instance, when translating [INT_MAX+1], we must generate a [long
            long] addition and so [1LL]. If we keep the initial string
            representation, the kind would be ignored in the generated code and
            so [1] would be generated. *)
         Cil.kinteger64 ~loc ~kind n, Analyses_types.C_number)
  | LStr _ | LWStr _ -> Options.not_yet_implemented "ACSL String literals"
  | LChr c -> Cil.new_exp ~loc (Const (CChr c)), Analyses_types.C_number
  | LReal lr ->
    if lr.r_lower = lr.r_upper
    then Cil.kfloat ~loc FDouble lr.r_nearest, Analyses_types.C_number
    else mk_real lr.r_literal
  | LEnum e -> Cil.new_exp ~loc (Const (CEnum e)), Analyses_types.C_number

(* Create and initialize a variable in the [env] according to [ty], [name] and
   [exp_init], return a tuple [varinfo * exp] and the [env] extended with the
   new variable. *)
let create_and_init_var ~loc kf ty name exp_init env =
  Env.new_var
    ~loc
    ~name
    env
    kf
    None
    ty
    (fun v_as_varinfo v_as_exp ->
       [ Gmp.init_set ~loc (Cil.var v_as_varinfo) v_as_exp  exp_init ])

let with_env ?rte ?kinstr ~env f =
  let (e', adata), env =
    Env.with_params_and_result ?rte ?kinstr ~env
      (fun env -> let e', adata, env = f env in (e', adata), env)
  in
  e', adata, env

(* Create a statement that assigns [exp1 binop exp2] to [var]. [exp_type] allows
   to decide if the binary operation is carried out using a function of gmp or
   not.*)
let affect_binop ~loc var_as_varinfo var_as_exp binop exp_type exp1 exp2 =
  if Gmp_types.Z.is_t exp_type then
    (match var_as_exp with
     | None ->
       Smart_stmt.rtl_call
         ~loc
         ~result:(Cil.var var_as_varinfo)
         ~prefix:""
         "__gmpz_cmp"
         [exp1; exp2]
     | Some e ->
       let name = Gmp.Z.name_arith_bop binop in
       Smart_stmt.rtl_call ~loc ~prefix:"" name [e; exp1; exp2])
  else if Gmp_types.Q.is_t exp_type then
    Error.not_yet "rational in affect_binop"
  else
    Smart_stmt.assigns ~loc
      ~result:(Cil.var var_as_varinfo)
      (Misc.make_binop ~loc binop exp1 exp2 )

let rec thost_to_host kf env th = match th with
  | TVar { lv_origin = Some v } ->
    Var v, env, v.vname
  | TVar ({ lv_origin = None } as logic_v) ->
    let v' = Env.Logic_binding.get env logic_v in
    Var v', env, logic_v.lv_name
  | TResult _typ ->
    let lhost = Misc.result_lhost kf in
    (match lhost with
     | Var v -> Var v, env, "result"
     | _ -> assert false)
  | TMem t ->
    let e, _, env = to_exp ~adata:Assert.no_data kf env t in
    Mem e, env, ""

and thost_to_host_il host =
  let* {env; kf} = M.read in
  match host with
  | TVar {lv_origin = Some v} ->
    M.return @@ IL.Lhost.var v
  | TVar ({lv_origin = None} as logic_v) ->
    let v' = Env.Logic_binding.get env logic_v in
    M.return @@ IL.Lhost.var ~name:logic_v.lv_name v'
  | TResult _typ ->
    let lhost = Misc.result_lhost kf in
    (match lhost with
     | Var v -> M.return @@ IL.Lhost.var ~name:"result" v
     | _ -> assert false)
  | TMem t -> M.map IL.Lhost.mem @@ to_exp_il t

and toffset_to_offset kf env = function
  | TNoOffset -> NoOffset, env
  | TField(f, offset) ->
    let offset, env = toffset_to_offset kf env offset in
    Field(f, offset), env
  | TIndex(t, offset) ->
    let e, _, env = to_exp ~adata:Assert.no_data kf env t in
    let offset, env = toffset_to_offset kf env offset in
    Index(e, offset), env
  | TModel _ -> Env.not_yet env "model"

and toffset_to_offset_il = function
  | TNoOffset -> M.return IL.NoOffset
  | TField(f, offset) ->
    let offset = toffset_to_offset_il offset in
    M.map (fun offset -> IL.Field(f, offset)) offset
  | TIndex(t, offset) ->
    let* e = to_exp_il t in
    let+ offset = toffset_to_offset_il offset in IL.Index (e, offset)
  | TModel _ as offset -> M.not_covered Printer.pp_term_offset offset

and tlval_to_lval_il (host, offset) =
  let* host_res = thost_to_host_il host in
  let+ offset_res = toffset_to_offset_il offset in (host_res, offset_res)

and tlval_to_lval kf env (host, offset) =
  let host, env, name = thost_to_host kf env host in
  let offset, env = toffset_to_offset kf env offset in
  let name = match offset with NoOffset -> name | Field _ | Index _ -> "" in
  (host, offset), env, name

(* Translate extended_quantifier terms to expressions in a given environment.
   [t] is the extended_quantifier term, [t_min] the lower bound, [t max] the
   upper bound, [lambda] the lambda term and [name] is the identifier of the
   extended quantifier ("\sum" or "\product").  Do not take care of "\numof"
   that have already been converted to "\sum". *)
and extended_quantifier_to_exp ~adata ~loc kf env t t_min t_max lambda name =
  let logic_env = Env.Logic_env.get env in
  let t = Logic_normalizer.get_term t in
  match lambda.term_node with
  | Tlambda([ k ] ,lt)
    when name.lv_name = "\\product" || name.lv_name = "\\sum"
    ->
    let ty_op = Typing.get_typ ~logic_env t in
    let ty_k = match Typing.get_cast ~logic_env t_min with
      | Some e -> e
      | None -> ty_op
    in
    let e_min, adata, env = to_exp ~adata kf env t_min in
    let e_max, adata, env = to_exp ~adata kf env t_max in
    let k_as_varinfo, k_as_exp, env = Env.Logic_binding.add ~ty:ty_k env kf k in
    let init_k_stmt = Gmp.init_set ~loc (Cil.var k_as_varinfo) k_as_exp e_min in
    (* variable initialization *)
    (* one = 1; *)
    let _, one_as_exp, env =
      create_and_init_var ~loc kf ty_k "one" (Cil.one ~loc) env
    in
    (* cond = 0; *)
    let cond_as_varinfo, cond_as_exp, env =
      create_and_init_var ~loc kf Cil_const.intType "cond" (Cil.zero ~loc) env
    in
    (* lbda = 0; *)
    let lbd_as_varinfo, lbd_as_exp, env =
      create_and_init_var ~loc kf ty_op "lambda" (Cil.zero ~loc) env
    in
    (* accumulator = neutral value; *)
    let acc_as_varinfo, acc_as_exp, env = if name.lv_name="\\sum" then
        create_and_init_var ~loc kf ty_op "accumulator" (Cil.zero ~loc) env
      else
        create_and_init_var ~loc kf ty_op "accumulator" (Cil.one ~loc) env
    in
    (* lambda_as_varinfo assignment *)
    let env = Env.push env in
    let e_lbd, _, env = to_exp ~adata:Assert.no_data kf env lt in
    let lbd_stmt,env =
      Env.pop_and_get ~kf env
        (Gmp.assign ~loc (Cil.var lbd_as_varinfo) lbd_as_exp e_lbd)
        ~global_clear:false Env.Middle
    in
    (* statement construction *)
    (* cond = k > e_max; or cond =  __gmpz_cmp(k,e_max) *)
    let cond_stmt =
      affect_binop ~loc cond_as_varinfo None Gt ty_k k_as_exp e_max
    in
    (* acc = acc op lbda; or __gmpz_op(acc,acc,lbda); *)
    let op = if name.lv_name = "\\sum" then PlusA else Mult in
    let acc_plus_lbd_stmt =
      affect_binop
        ~loc
        acc_as_varinfo
        (Some acc_as_exp)
        op
        ty_op
        acc_as_exp
        lbd_as_exp
    in
    (* k = k + one; or __gmpz_add(k,k,one); *)
    let k_plus_one_stmt =
      affect_binop
        ~loc k_as_varinfo (Some k_as_exp) PlusA ty_k k_as_exp one_as_exp
    in
    (* if [ty_k] is gmpz, then the result of the comparison must be interpreted
       as [true] if [cond == 1] and as [false] if [cond == 0] or [-1] because of
       the semantics of __gmpz_cmp. That differs from the conventional
       interpretation. *)
    let cond_as_exp =
      if Gmp_types.Z.is_t ty_k then
        (Misc.make_binop ~loc Gt cond_as_exp (Cil.zero ~loc))
      else
        cond_as_exp
    in
    (* statement combination *)
    let if_stmt =
      Smart_stmt.if_stmt
        ~loc
        ~cond:cond_as_exp
        ~else_blk:
          (Cil.mkBlock
             [ Smart_stmt.block_stmt lbd_stmt;
               acc_plus_lbd_stmt;
               k_plus_one_stmt ])
        (Cil.mkBlock [ Smart_stmt.break ~loc ])
    in
    let for_stmt =
      Smart_stmt.stmt
        (Loop([],Cil.mkBlock [ cond_stmt; if_stmt ],loc,None,None))
    in
    let final_stmt  = (Cil.mkBlock [ init_k_stmt; for_stmt ]) in
    Env.Logic_binding.remove env k;
    let env = Env.add_stmt env (Smart_stmt.block_stmt final_stmt) in
    let adata = Assert.register_term ~loc t acc_as_exp adata in
    acc_as_exp, adata, env, Analyses_types.C_number, ""
  | _ ->
    assert false

and context_insensitive_term_to_exp_il ?inplace t =
  ignore inplace; (* will be required for implementing Tat *)
  let t = Logic_normalizer.get_term t in
  match t.term_node with
  | TConst c -> constant_to_exp_il t c
  | TLval lv ->
    let* l = tlval_to_lval_il lv in
    M.return @@ IL.Exp.lval ~origin:t l
  | TSizeOf ty -> M.return @@ IL.Exp.sizeof ~origin:t ty
  | TCast (true, _, t) -> to_exp_il t
  | TBinOp(PlusA | MinusA | Mult as bop, t1, t2) ->
    let* logic_env = M.read_logic_env in
    let* e1 = to_exp_il t1 in
    let+ e2 = to_exp_il t2 in
    let ity = Typing.get_number_ty ~logic_env t in
    let ty = Typing.get_typ ~logic_env t in
    if not (Gmp_types.Z.is_t ty) && not (Gmp_types.Q.is_t ty) then
      assert (Logic_utils.is_integral_type t.term_type);
    Interlang.Exp.binop ~origin:t (Interlang_gen.of_binop bop) ity e1 e2
  | TBinOp ((Lt | Gt | Le | Ge | Eq | Ne) as bop, t1, t2) ->
    let* logic_env = M.read_logic_env in
    let ity =
      Typing.join
        (Typing.get_effective_ty ~logic_env t1)
        (Typing.get_effective_ty ~logic_env t2)
    in
    let* e1 = to_exp_il t1 in
    let+ e2 = to_exp_il t2 in
    Interlang.Exp.binop ~origin:t (Interlang_gen.of_binop bop) ity e1 e2
  | TBinOp (Div | Mod as binop, t1, t2) ->
    let* logic_env = M.read_logic_env in
    let ty = Typing.get_typ ~logic_env t in
    let* e1 = to_exp_il t1 in
    let+ e2 = to_exp_il t2 in
    let ity = Typing.get_number_ty ~logic_env t in
    if not (Gmp_types.Z.is_t ty || Gmp_types.Q.is_t ty) then
      assert (Logic_utils.is_integral_type t.term_type);
    Interlang.Exp.binop ~origin:t (Interlang_gen.of_binop binop) ity e1 e2
  | _ -> M.not_covered Printer.pp_term t

and context_insensitive_term_to_exp_old ~adata ?(inplace=false) kf env t =
  let t = Logic_normalizer.get_term t in
  let loc = t.term_loc in
  (* translation of all guards associated to [t] in [Guards.table] *)
  let env = Translate_predicates.rte_guards_to_exp_old ~loc ~kf t env in
  let open Current_loc.Operators in
  let<> UpdatedCurrentLoc = loc in
  let logic_env = Env.Logic_env.get env in
  match t.term_node with
  | TConst c ->
    let c, strnum = constant_to_exp_old ~loc env t c in
    c, adata, env, strnum, ""
  | TLval lv ->
    let lv, env, name = tlval_to_lval kf env lv in
    let e = Smart_exp.lval ~loc lv in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, name
  | TSizeOf ty ->
    let e = Cil.sizeOf ~loc ty in
    let adata = Assert.register_term ~loc ~force:true t e adata in
    e, adata, env, Analyses_types.C_number, "sizeof"
  | TSizeOfE t' ->
    let e', _, env = to_exp ~adata:Assert.no_data kf env t' in
    let e = Cil.sizeOf ~loc (Cil.typeOf e') in
    let adata = Assert.register_term ~loc ~force:true t e adata in
    e, adata, env, Analyses_types.C_number, "sizeof"
  | TAlignOf ty ->
    let e = Cil.new_exp ~loc (AlignOf(ty, `Standard)) in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, "alignof"
  | TAlignOfE t' ->
    let e', _, env = to_exp ~adata:Assert.no_data kf env t' in
    let e = Cil.new_exp ~loc (AlignOfE (e', `Standard)) in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, "alignof"
  | TUnOp(Neg | BNot as op, t') ->
    let ty = Typing.get_typ ~logic_env t in
    let e, adata, env = to_exp ~adata kf env t' in
    if Gmp_types.Z.is_t ty then
      let name, vname = match op with
        | Neg -> "__gmpz_neg", "neg"
        | BNot -> "__gmpz_com", "bnot"
        | LNot -> assert false
      in
      let e, env =
        Gmp.Z.new_var
          ~loc
          env
          kf
          ~name:vname
          (Some t)
          (fun _ ev -> [ Smart_stmt.rtl_call ~loc ~prefix:"" name [ ev; e ] ])
      in
      e, adata, env, Analyses_types.C_number, ""
    else if Gmp_types.Q.is_t ty then
      Env.not_yet env "reals: Neg | BNot"
    else
      Cil.new_exp ~loc (UnOp(op, e, ty)), adata, env, Analyses_types.C_number, ""
  | TUnOp(LNot, t) ->
    let ty = Typing.get_effective_typ ~logic_env t in
    if Gmp_types.Z.is_t ty then
      (* [!t] is converted into [t == 0] *)
      let zero = Logic_const.tinteger 0 in
      let ctx = Typing.get_number_ty ~logic_env t in
      Typing.preprocess_term ~use_gmp_opt:true ~ctx ~logic_env zero;
      let t_exp, adata, env = to_exp ~adata kf env t in
      let zero_exp, adata, env = to_exp ~adata kf env zero in
      let e, env =
        Translate_utils.comparison_to_exp
          kf
          ~loc
          ~name:"not"
          env
          Typing.gmpz
          Eq
          t_exp
          zero_exp
          (Some t)
      in
      e, adata, env, Analyses_types.C_number, ""
    else begin
      assert (Ast_types.is_integral ty);
      let e, adata, env = to_exp ~adata kf env t in
      let e = Smart_exp.lnot ~loc e in
      e, adata, env, Analyses_types.C_number, ""
    end
  | TBinOp(PlusA | MinusA | Mult as bop, t1, t2) ->
    let t1 = Logic_normalizer.get_term t1 in
    let t2 = Logic_normalizer.get_term t2 in
    let ty = Typing.get_typ ~logic_env t in
    let e1, adata, env = to_exp ~adata kf env t1 in
    let e2, adata, env = to_exp ~adata kf env t2 in
    if Gmp_types.Z.is_t ty then
      let e, env = Gmp.Z.binop ~loc (Some t) bop env kf e1 e2 in
      e, adata, env, Analyses_types.C_number, ""
    else if Gmp_types.Q.is_t ty then
      let e, env = Gmp.Q.binop ~loc (Some t) bop env kf e1 e2 in
      e, adata, env, Analyses_types.C_number, ""
    else begin
      assert (Logic_utils.is_integral_type t.term_type);
      let e = Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)) in
      e, adata, env, Analyses_types.C_number, ""
    end
  | TBinOp(Div | Mod as bop, t1, t2) ->
    let ty = Typing.get_typ ~logic_env t in
    let e1, adata, env = to_exp ~adata kf env t1 in
    let t2_to_exp adata env = to_exp ~adata kf env t2 in
    if Gmp_types.Z.is_t ty then
      let e2, adata, env = t2_to_exp adata env in
      let mk_stmts _v e =
        assert (Gmp_types.Z.is_t ty);
        let name = Gmp.Z.name_arith_bop bop in
        let instr = Smart_stmt.rtl_call ~loc ~prefix:"" name [ e; e1; e2 ] in
        [ instr ]
      in
      let name = Misc.name_of_binop bop in
      let e, env = Gmp.Z.new_var ~loc ~name env kf (Some t) mk_stmts in
      e, adata, env, Analyses_types.C_number, ""
    else if Gmp_types.Q.is_t ty then
      let e2, adata, env = t2_to_exp adata env in
      let e, env = Gmp.Q.binop ~loc (Some t) bop env kf e1 e2 in
      e, adata, env, Analyses_types.C_number, ""
    else begin
      assert (Logic_utils.is_integral_type t.term_type);
      (* no guard required since RTEs are generated separately *)
      let e2, adata, env = t2_to_exp adata env in
      let e = Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)) in
      e, adata, env, Analyses_types.C_number, ""
    end
  | TBinOp(Lt | Gt | Le | Ge | Eq | Ne as bop, t1, t2) ->
    (* comparison operators *)
    let ity =
      Typing.join
        (Typing.get_effective_ty ~logic_env t1)
        (Typing.get_effective_ty ~logic_env t2)
    in
    let e1, adata, env = to_exp ~adata kf env t1 in
    let e2, adata, env = to_exp ~adata kf env t2 in
    let e, env =
      Translate_utils.comparison_to_exp
        ~loc
        kf
        env
        ity
        bop
        e1
        e2
        (Some t)
    in
    e, adata, env, Analyses_types.C_number, ""
  | TBinOp((Shiftlt | Shiftrt) as bop, t1, t2) ->
    (* left/right shift *)
    let ty = Typing.get_typ ~logic_env t in
    let t1_to_exp adata env = to_exp ~adata kf env t1 in
    let t2_to_exp adata env = to_exp ~adata kf env t2 in
    if Gmp_types.Z.is_t ty then
      (* Creating secondary assertion contexts [adata1] and [adata2] to hold the
         data contributing to the guards of [t1] and [t2].
         Both secondary contexts will be merged to [adata] afterward so that the
         calling assertion context holds all data. *)
      let adata1, env = Assert.empty ~loc kf env in
      let adata2, env = Assert.empty ~loc kf env in
      let e1, adata1, env = t1_to_exp adata1 env in
      let e2, adata2, env = t2_to_exp adata2 env in
      let adata, env = Assert.merge_right ~loc env adata1 adata in
      let adata, env = Assert.merge_right ~loc env adata2 adata in
      (* If the given term is an lvalue variable or a cast from an lvalue
         variable, retrieve the name of this variable. Otherwise return
         default *)
      let rec term_to_name t =
        match t.term_node with
        | TConst _ -> "cst_"
        | TLval (TVar { lv_name }, _) -> lv_name ^ "_"
        | TCast (_,_, t) -> term_to_name t
        | _ -> ""
      in
      let ctx = Typing.get_number_ty ~logic_env t in
      let bop_name = Misc.name_of_binop bop in
      let e1_name = term_to_name t1 in
      let e2_name = term_to_name t2 in
      let zero = Logic_const.tinteger 0 in
      Typing.preprocess_term ~use_gmp_opt:true ~ctx ~logic_env zero;
      (* Check that e2 is representable in mp_bitcnt_t *)
      let coerce_guard, env =
        let name = e2_name ^ bop_name ^ "_guard" in
        let _vi, e, env =
          Env.new_var
            ~loc
            ~scope:Varname.Block
            ~name
            env
            kf
            None
            Cil_const.intType
            (fun vi _e ->
               let result = Cil.var vi in
               let fname = "__gmpz_fits_ulong_p" in
               [ Smart_stmt.rtl_call ~loc ~result ~prefix:"" fname [ e2 ] ])
        in
        e, env
      in
      (* Coerce e2 to mp_bitcnt_t *)
      let coerce_guard_cond, env =
        let max_bitcnt =
          Cil.lconstant ~loc @@
          Cil.max_unsigned_number @@ Cil.bitsSizeOf @@ Gmp_types.bitcnt_t ()
        in
        let pred =
          Logic_const.pand
            ~loc
            ~names:[bop_name ^ "_rhs_fits_in_mp_bitcnt_t"]
            (Logic_const.prel ~loc (Rle, zero, Misc.Id_term.deep_copy t2),
             Logic_const.prel ~loc (Rle, Misc.Id_term.deep_copy t2, max_bitcnt))
        in
        Typing.preprocess_predicate ~logic_env:(Env.Logic_env.get env) pred;
        let cond, env =
          Assert.runtime_check
            ~adata:adata2
            ~pred_kind:Assert
            RTE
            kf
            env
            coerce_guard
            pred
        in
        cond, env
      in
      let mk_coerce_stmts vi _e =
        let result = Cil.var vi in
        let name = "__gmpz_get_ui" in
        let instr = Smart_stmt.rtl_call ~loc ~result ~prefix:"" name [ e2 ] in
        [ coerce_guard_cond; instr ]
      in
      let name = e2_name ^ bop_name ^ "_coerced" in
      let _e2_as_bitcnt_vi, e2_as_bitcnt_e, env =
        Env.new_var
          ~loc
          ~scope:Varname.Block
          ~name
          env
          kf
          None
          (Gmp_types.bitcnt_t ())
          mk_coerce_stmts
      in

      (* Create the shift instruction *)
      let mk_shift_instr result_e =
        let name = Gmp.Z.name_arith_bop bop in
        Smart_stmt.rtl_call ~loc
          ~prefix:""
          name
          [ result_e; e1; e2_as_bitcnt_e ]
      in

      (* Put t in an option to use with Translate_utils.comparison_to_exp and
         Gmp.Z.new_var *)
      let t = Some t in

      (* TODO: let RTE generate the undef behaviors assertions *)

      (* Boolean to choose whether the guard [e1 >= 0] should be added *)
      let should_guard_e1 =
        match bop with
        | Shiftlt -> Kernel.LeftShiftNegative.get ()
        | Shiftrt -> Kernel.RightShiftNegative.get ()
        | _ -> assert false
      in

      (* Create the statements to initialize [e1 shift e2] *)
      let e1_guard_opt, env =
        if should_guard_e1 then
          (* Future RTE:
             if (warn left shift negative and left shift)
                or (warn right shift negative and right shift)
             then check e1 >= 0 *)
          let zero_exp, _, env = to_exp ~adata:Assert.no_data kf env zero in
          let e1_guard, env =
            let name = e1_name ^ bop_name ^ "_guard" in
            Translate_utils.comparison_to_exp
              ~loc
              kf
              env
              Typing.gmpz
              ~name
              Ge
              e1
              zero_exp
              t
          in
          let e1_guard_cond, env =
            let pred =
              Logic_const.prel ~loc
                (Rge, Misc.Id_term.deep_copy t1, Misc.Id_term.deep_copy zero)
            in
            Typing.preprocess_predicate ~logic_env:(Env.Logic_env.get env) pred;
            let cond, env =
              Assert.runtime_check
                ~adata:adata1
                ~pred_kind:Assert
                RTE
                kf
                env
                e1_guard
                pred
            in
            cond, env
          in
          Some e1_guard_cond, env
        else
          (* Manual clean because [runtime_check] has not been called on
             [adata1]. *)
          let env = Assert.clean ~loc env adata1 in
          None, env
      in
      let mk_stmts _ e =
        let shift_instr = mk_shift_instr e in
        match e1_guard_opt with
        | None -> [ shift_instr ]
        | Some e1_guard -> [ e1_guard; shift_instr ]
      in
      let name = bop_name in
      let e, env = Gmp.Z.new_var ~loc ~name env kf t mk_stmts in
      e, adata, env, Analyses_types.C_number, ""
    else begin
      assert (Logic_utils.is_integral_type t.term_type);
      let e1, adata, env = t1_to_exp adata env in
      let e2, adata, env = t2_to_exp adata env in
      let e = Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)) in
      e, adata, env, Analyses_types.C_number, ""
    end
  | TBinOp(LOr, t1, t2) ->
    (* t1 || t2 <==> if t1 then true else t2 *)
    let (e, adata), env =
      Env.with_params_and_result ~rte:true ~env (fun env ->
          let e1, adata, env1 = to_exp ~adata kf env t1 in
          let env' = Env.push env1 in
          let e2, adata, env2 =
            to_exp ~adata kf (Env.push env') t2
          in
          let res2 = e2, env2 in
          Extlib.nest
            adata
            (Translate_utils.conditional_to_exp
               ~name:"or" ~loc kf (Some t) e1 (Cil.one ~loc, env') res2)
        )
    in
    e, adata, env, Analyses_types.C_number, ""
  | TBinOp(LAnd, t1, t2) ->
    (* t1 && t2 <==> if t1 then t2 else false *)
    let (e, adata), env =
      Env.with_params_and_result ~rte:true ~env (fun env ->
          let e1, adata, env1 = to_exp ~adata kf env t1 in
          let e2, adata, env2 =
            to_exp ~adata kf (Env.push env1) t2
          in
          let res2 = e2, env2 in
          let env3 = Env.push env2 in
          Extlib.nest
            adata
            (Translate_utils.conditional_to_exp
               ~name:"and" ~loc kf (Some t) e1 res2 (Cil.zero ~loc, env3))
        )
    in
    e, adata, env, Analyses_types.C_number, ""
  | TBinOp((BOr | BXor | BAnd) as bop, t1, t2) ->
    (* other logic/arith operators  *)
    let ty = Typing.get_typ ~logic_env t in
    let e1, adata, env = to_exp ~adata kf env t1 in
    let e2, adata, env = to_exp ~adata kf env t2 in
    if Gmp_types.Z.is_t ty then
      let mk_stmts _v e =
        let name = Gmp.Z.name_arith_bop bop in
        let instr = Smart_stmt.rtl_call ~loc ~prefix:"" name [ e; e1; e2 ] in
        [ instr ]
      in
      let name = Misc.name_of_binop bop in
      let t = Some t in
      let e, env = Gmp.Z.new_var ~loc ~name env kf t mk_stmts in
      e, adata, env, Analyses_types.C_number, ""
    else begin
      assert (Logic_utils.is_integral_type t.term_type);
      let e = Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)) in
      e, adata, env, Analyses_types.C_number, ""
    end
  | TBinOp(PlusPI | MinusPI as bop, t1, t2) ->
    if Misc.is_set_of_ptr_or_array t1.term_type ||
       Misc.is_set_of_ptr_or_array t2.term_type then
      (* case of arithmetic over set of pointers (due to use of ranges)
         should have already been handled in [Memory_translate.call_with_ranges]
      *)
      assert false;
    (* binary operation over pointers *)
    let ty = match t1.term_type with
      | Ctype ty -> ty
      | _ -> assert false
    in
    let e1, adata, env = to_exp ~adata kf env t1 in
    let e2, adata, env = to_exp ~adata kf env t2 in
    let e = Cil.new_exp ~loc (BinOp(bop, e1, e2, ty)) in
    e, adata, env, Analyses_types.C_number, ""
  | TBinOp(MinusPP, t1, t2) ->
    begin match Typing.get_number_ty ~logic_env t with
      | C_integer _ ->
        let e1, adata, env = to_exp ~adata kf env t1 in
        let e2, adata, env = to_exp ~adata kf env t2 in
        let ty = Typing.get_typ ~logic_env t in
        let e = Cil.new_exp ~loc (BinOp(MinusPP, e1, e2, ty)) in
        e, adata, env, Analyses_types.C_number, ""
      | Gmpz ->
        Env.not_yet env "pointer subtraction resulting in gmp"
      | C_float _ | Rational | Real | Nan ->
        assert false
    end
  | TCast (false, Ctype ty, t') ->
    let t' = Logic_normalizer.get_term t' in
    let e, adata, env = to_exp ~adata kf env t' in
    let e, env =
      Typed_number.add_cast
        ~loc
        ~name:"cast"
        env
        kf
        (Some ty)
        Analyses_types.C_number
        (Some t)
        e
    in
    e, adata, env, Analyses_types.C_number, ""
  | TCast (true, _, _) when Inductive.is_fallthrough_term t ->
    let e = Cil.zero ~loc in
    let annot_kind = Env.annotation_kind env in
    let stmt, env =
      Assert.runtime_check_with_msg
        ~adata
        ~loc
        "Incomplete inductive function"
        ~pred_kind:Assert
        annot_kind
        kf
        env
        e
    in
    let env = Env.add_stmt env stmt in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, ""
  | TCast (true, _, t) -> context_insensitive_term_to_exp_old ~adata kf env t
  | TCast (false, _,_) -> assert false
  | TAddrOf lv ->
    let lv, env, _ = tlval_to_lval kf env lv in
    let e = Cil.mkAddrOf ~loc lv in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, "addrof"
  | TStartOf lv ->
    let lv, env, _ = tlval_to_lval kf env lv in
    let e = Cil.mkAddrOrStartOf ~loc lv in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, "startof"
  | Tapp(li, _, [ t1; t2; {term_node = Tlambda([ _ ], _)} as lambda ])
    when li.l_body = LBnone && (li.l_var_info.lv_name = "\\sum" ||
                                li.l_var_info.lv_name = "\\product")->
    let t1 = Logic_normalizer.get_term t1 in
    let t2 = Logic_normalizer.get_term t2 in
    extended_quantifier_to_exp ~adata ~loc kf env t t1 t2 lambda li.l_var_info
  | Tapp(li, _, _)
    when li.l_body = LBnone && li.l_var_info.lv_name = "\\numof" ->
    assert false
  | Tapp(li, labels, args) when Misc.labels_are_all_here labels ->
    let args = List.map Logic_normalizer.get_term args in
    let e, adata, env =
      Logic_functions.app_to_exp ~adata ~loc ~tapp:t kf env li args
    in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, "app"
  | Tapp(_, _, _) ->
    Env.not_yet env "logic functions with labels"
  | Tlambda(_, lt) ->
    let exp, adata, env = to_exp ~adata kf env lt in
    exp, adata, env, Analyses_types.C_number, ""
  | TDataCons _ -> Env.not_yet env "constructor"
  | Tif(cond, t1, t2) ->
    let cond = Logic_normalizer.get_pred cond in
    let t1 = Logic_normalizer.get_term t1 in
    let t2 = Logic_normalizer.get_term t2 in
    let (e, adata), env =
      Env.with_params_and_result ~rte:true ~env (fun env ->
          let e1, adata, env1 = Translate_predicates.to_exp ~adata kf env cond in
          let e2, adata, env2 =
            to_exp ~adata kf (Env.push env1) t1
          in
          let res2 = e2, env2 in
          let e3, adata, env3 =
            to_exp ~adata kf (Env.push env2) t2
          in
          let res3 = e3, env3 in
          Extlib.nest
            adata
            (Translate_utils.conditional_to_exp
               ~loc
               kf
               (Some t)
               e1
               res2
               res3)
        )
    in
    e, adata, env, Analyses_types.C_number, ""
  | Tat(t', label) ->
    let e, adata, env =
      if inplace || label = BuiltinLabel Here then
        to_exp ~adata kf env t'
      else
        Translate_ats.to_exp ~loc ~adata kf env (PoT_term t) label
    in
    e, adata, env, Analyses_types.C_number, ""
  | Tbase_addr(BuiltinLabel Here, t') ->
    let name = "base_addr" in
    let e', _, env =
      with_env ~rte:true ~env (fun env -> to_exp ~adata:Assert.no_data kf env t')
    in
    let e, env =
      Memory_translate.call
        ~loc
        kf
        name
        Cil_const.voidPtrType
        env
        [e']
    in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, name
  | Tbase_addr _ -> Env.not_yet env "labeled \\base_addr"
  | Toffset(BuiltinLabel Here, t') ->
    let size_t = Machine.sizeof_type () in
    let name = "offset" in
    let e', adata, env =
      with_env ~rte:true ~env (fun env -> to_exp ~adata kf env t')
    in
    let e, env =
      Memory_translate.call ~loc kf name size_t env [e']
    in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, name
  | Toffset _ -> Env.not_yet env "labeled \\offset"
  | Tblock_length(BuiltinLabel Here, t') ->
    let size_t = Machine.sizeof_type () in
    let name = "block_length" in
    let e', adata, env =
      with_env ~rte:true ~env (fun env -> to_exp ~adata kf env t')
    in
    let e, env =
      Memory_translate.call ~loc kf name size_t env [e']
    in
    let adata = Assert.register_term ~loc t e adata in
    e, adata, env, Analyses_types.C_number, name
  | Tblock_length _ -> Env.not_yet env "labeled \\block_length"
  | Tnull ->
    let e = Smart_exp.null ~loc in
    e, adata, env, Analyses_types.C_number, "null"
  | TUpdate _ -> Env.not_yet env "functional update"
  | Ttypeof _ -> Env.not_yet env "typeof"
  | Ttype _ -> Env.not_yet env "C type"
  | Tempty_set -> Env.not_yet env "empty tset"
  | Tunion _ -> Env.not_yet env "union of tsets"
  | Tinter _ -> Env.not_yet env "intersection of tsets"
  | Tcomprehension _ -> Env.not_yet env "tset comprehension"
  | Trange _ -> Env.not_yet env "range"
  | Tlet(li, t) ->
    let t = Logic_normalizer.get_term t in
    (* Translate the term registered to the \let logic variable *)
    let adata, env = Translate_utils.env_of_li ~adata ~loc kf env li in
    (* Register the logic var to the logic scope *)
    let lvs = Lvs_let(li.l_var_info, Misc.term_of_li li) in
    let env = Env.Logic_scope.extend env lvs in
    (* Translate the body of the \let *)
    let e, adata, env = to_exp ~adata kf env t in
    (* Remove the logic var from the logic scope *)
    let env = Env.Logic_scope.remove env lvs in
    e, adata, env, Analyses_types.C_number, ""

and to_exp_il ?inplace t =
  let* {env} = M.read in
  let* () =
    if Env.generate_rte env
    then M.not_covered ~pre:"with RTE" Printer.pp_term t
    else M.return ()
  in
  (* translates RTE guards of the current term *)
  let* rte_guards = Translate_predicates.rte_guards_to_exp_il t in
  let* e = context_insensitive_term_to_exp_il ?inplace t in
  let e = IL.Helpers.attach_rtes rte_guards e in
  let e = match Typing.get_cast ~logic_env:(Env.Logic_env.get env) t with
    | None -> e
    | Some typ -> IL.Exp.coerce ?origin:e.origin ~coerce_to:typ e
  in
  Options.debug ~dkey ~level:4 "to_exp_il {%a} %a = %a (%a)"
    Profile.pretty (Env.Logic_env.get_profile env)
    Printer.pp_term t
    Interlang.Pretty.pp_exp e
    Interlang.Pretty.pp_rtes e.rtes;
  M.return e

(* Convert an ACSL term into a corresponding C expression (if any) in the given
   environment. Also extend this environment in order to include the generating
   constructs. *)
and to_exp_old ?inplace ~loc:_ ~adata ~env ~kf t =
  let generate_rte = Env.generate_rte env in
  Options.feedback ~dkey ~level:5 "translating term %a (rte? %b) in local \
                                   environment '%a'"
    Printer.pp_term t generate_rte Profile.pretty
    (Env.Logic_env.get_profile env);
  let logic_env = Env.Logic_env.get env in
  let t = Logic_normalizer.get_term t in
  let rexp, _, _ as result =
    Extlib.flatten @@ Env.with_params_and_result ~rte:false ~env (fun env ->
        let e, adata, env, sty, name =
          context_insensitive_term_to_exp_old ?inplace ~adata kf env t
        in
        let env =
          if generate_rte then !translate_rte_exp_ref kf env e else env
        in
        let cast = Typing.get_cast ~logic_env t in
        let name = if name = "" then None else Some name in
        Extlib.nest
          adata
          (Typed_number.add_cast
             ~loc:t.term_loc
             ?name
             env
             kf
             cast
             sty
             (Some t)
             e)
      )
  in
  Options.debug ~dkey ~level:4 "to_exp_old %a {%a} %a = %a"
    Kernel_function.pretty kf Profile.pretty (Env.Logic_env.get_profile env)
    Printer.pp_term t Printer.pp_exp rexp;
  result

and to_exp ~adata ?inplace kf env t =
  let loc = t.term_loc in
  Interlang_trans.try_il_compiler ~loc ~adata ~env ~kf
    (to_exp_il ?inplace)
    (to_exp_old ?inplace)
    t

let () =
  Translate_utils.Translate_terms.to_exp_ref := to_exp;
  Translate_ats.Translate_terms.to_exp_ref := to_exp;
  Loops.Translate_terms.to_exp_ref := to_exp;
  Memory_translate.Translate_terms.to_exp_ref := to_exp;
  Logic_functions.Translate_terms.to_exp_ref := to_exp

exception No_simple_translation of term

(* This function is used by plug-in [Cfp]. *)
let untyped_to_exp typ t =
  (* infer a context from the given [typ] whenever possible *)
  let ctx_of_typ ty =
    if Gmp_types.Z.is_t ty then Typing.gmpz
    else if Gmp_types.Q.is_t ty then Typing.rational
    else
      match ty.tnode with
      | TInt ik | TEnum { ekind = ik } -> Typing.ikind ik
      | TFloat fk -> Typing.fkind fk
      | _ -> Typing.nan
  in
  let ctx = Option.map ctx_of_typ typ in
  let logic_env = Logic_env.empty in
  Typing.preprocess_term ~use_gmp_opt:true ~logic_env ?ctx t;
  let env = Env.push Env.empty in
  let env = Env.set_rte env false in
  let e, _, env =
    try to_exp ~adata:Assert.no_data Cil_datatype.Kf.dummy env t
    with Rtl.Symbols.Unregistered _ -> raise (No_simple_translation t)
  in
  if not (Env.has_no_new_stmt env) then raise (No_simple_translation t);
  e
