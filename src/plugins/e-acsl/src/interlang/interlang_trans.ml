(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* The new compilation scheme of E-ACSL to Cil is implemented as a two-stage
   process, where E-ACSL is first translated into an intermediate language
   Interlang and only then into Cil. This module implements the second stage.
   To this end we define here a monad M for specifying computations that
   generate Cil expressions, and that while doing so modifies the assertion
   data (of type Assert.t) and the environment (of type Env.t). *)

open Interlang

let dkey = Options.Dkey.interlang_translation

module Conf = struct
  (* The Reader variable of M. See Monad_rws.Conf.env *)
  type env = {kf : Cil_types.kernel_function;
              loc : Cil_types.location;
              adata_register : bool}

  (* The State variable of M. The monad generates Cil expressions, all the
     while making modifications to the current environment (of type Env.t) and
     the recorded assertion data (of type Assert.t). *)
  type state = {env : Env.t; adata : Assert.t}

  (** The out variable of {!M} contains a list of RTE guards which is enriched
      with new elements during the translation from Interlang to Cil. Using
      [bind], [update] and [flush], the list can be initialized, merged with
      another one and finally retrieved and cleared. *)
  type out = rte list (* The Writer variable of M. *)
  let merge_out l1 l2 = l2 @ l1
  let empty_out () = []
end

(** The intermediate language translation monad. It is used for translating
    expressions of the E-ACSL intermediate language (see {!Interlang}) to Cil. *)
module M = struct
  include Monad_rws.Make (Conf)
  open Operators

  let modify_adata f = modify (fun s -> {s with adata = f s.adata})

  let without_registering_adata m =
    with_env (fun env -> {env with adata_register = false}) m

  let with_loc loc m = with_env (fun env -> {env with loc}) m
  let maybe_with_term_loc t_opt m =
    match t_opt with | None -> m | Some t -> with_loc t.Cil_types.term_loc m

  let do_if_registering_adata m =
    let* env = read in
    if env.adata_register then m else return ()

  let modifying_env f =
    let* {env} as state = get in
    let e, env = f env in
    let* () = set {state with env} in
    return e
end

open M.Operators

let compile_binop = function
  | Interlang.Plus -> Cil_types.PlusA
  | Minus -> MinusA
  | Mult -> Mult
  | Div -> Div
  | Mod -> Mod
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge
  | Eq -> Eq
  | Ne -> Ne

let assert_register_term ~loc ?force e t =
  M.do_if_registering_adata @@
  M.modify_adata @@ fun a ->
  Assert.register_term ~loc ?force t e a

let rec compile ?(flush_rtes=false) exp =
  let* e, coerce, cast_info = compile_with_rtes ~flush_rtes exp in
  match cast_info with (* [cast_info] specifies type type we cast from. *)
  | Some (strnum, name) ->
    let name = if name = "" then None else Some name in
    let* {kf; loc} = M.read in
    let loc = match exp.origin with
      | Some t -> t.term_loc
      | None -> loc
    in
    M.modifying_env (fun env ->
        Typed_number.add_cast ~loc
          ?name
          env
          kf
          coerce
          strnum
          exp.origin
          e)
  | None -> M.return e (* no cast required *)

and compile_with_rtes ?(flush_rtes=false) exp =
  let res = M.update exp.rtes @@ compile_context_insensitive exp in
  if flush_rtes then compile_rte_guards res else res

and compile_context_insensitive {Interlang.enode; origin} =
  let* {kf; loc} = M.read in
  match enode with
  | True -> M.return (Cil.one ~loc, None, Some (Analyses_types.C_number, ""))
  | False -> M.return (Cil.zero ~loc, None, Some (Analyses_types.C_number, ""))
  | Integer {n; ity} ->
    (* cf Translate_terms.constant_to_exp *)
    let e, strnum =
      let open Analyses_types in
      match ity with
      | Nan -> assert false
      | Real -> Error.not_yet "real number constant"
      | Rational ->
        let s = Gmp.Q.normalize_str (Z.to_string n) in
        let vi = Globals.Vars.add_string_literal ~loc @@ Str s in
        Cil.mkAddrOrStartOf ~loc (Cil.var vi), Str_R
      | Gmpz ->
        let vi = Globals.Vars.add_string_literal ~loc @@ Str (Z.to_string n) in
        Cil.mkAddrOrStartOf ~loc (Cil.var vi), Str_Z
      | C_float fkind ->
        Cil.kfloat ~loc fkind (Int64.to_float (Z.to_int64 n)), C_number
      | C_integer kind ->
        (* do not keep the initial string representation because the generated
           constant must reflect its type computed by the type system. For
           instance, when translating [INT_MAX+1], we must generate a [long
           long] addition and so [1LL]. If we keep the initial string
           representation, the kind would be ignored in the generated code and
           so [1] would be generated. *)
        Cil.kinteger64 ~loc ~kind n, C_number
    in
    M.return (e, None, Some (strnum, ""))
  | BinOp {ity; binop = Lt | Gt | Le | Ge | Eq | Ne as binop; op1; op2} ->
    let binop = compile_binop binop in
    let* e1 = compile ~flush_rtes:true op1 in
    let* e2 = compile ~flush_rtes:true op2 in
    let name = Misc.name_of_binop binop in
    let* e = M.modifying_env @@ fun env ->
      Translate_utils.comparison_to_exp ~loc kf env ity binop e1 e2 ~name origin
    in
    M.return (e, None, Some (Analyses_types.C_number, name))
  | BinOp {ity; binop = Plus | Minus | Mult as binop; op1; op2} ->
    let binop = compile_binop binop in
    let* e1 = compile op1 in
    let* e2 = compile op2 in
    let* e = match ity with
      | Gmpz ->
        M.modifying_env @@ fun env ->
        Gmp.Z.binop ~loc origin binop env kf e1 e2
      | Rational ->
        M.modifying_env @@ fun env ->
        Gmp.Q.binop ~loc origin binop env kf e1 e2
      | Analyses_types.C_integer _
      | Analyses_types.C_float _
      | Analyses_types.Real
      | Analyses_types.Nan ->
        let ty = Typing.typ_of_number_ty ity in
        M.return @@ Cil.new_exp ~loc @@ BinOp (binop, e1, e2, ty)
    in
    M.return (e, None, Some (Analyses_types.C_number, Misc.name_of_binop binop))
  | BinOp ({binop = Div | Mod} as binop_node) ->
    compile_div_mod ~origin binop_node
  | Lval lval ->
    M.maybe_with_term_loc origin @@
    let* lval, name = M.without_registering_adata @@ compile_lval lval in
    let* {loc} = M.read in
    let e = Smart_exp.lval ~loc lval in
    let* () = M.Option.iter (assert_register_term ~loc e) origin in
    M.return (e, None, Some (Analyses_types.C_number, name))
  | SizeOf ty ->
    let e = Cil.sizeOf ~loc ty in
    let* () = M.Option.iter (assert_register_term ~loc ~force:true e) origin in
    M.return (e, None, Some (Analyses_types.C_number, "sizeof"))
  | Coerce {coerce_to = typ; coerced = exp} ->
    let* e, coerce, cast_info = compile_with_rtes exp in
    ignore coerce; (* coerce to A and then B ⇒ just coerce directly to B *)
    M.return (e, Some typ, cast_info)

and compile_div_mod ~origin {ity; binop; op1; op2} =
  assert (Interlang.Helpers.is_div_or_mod binop);
  let* {kf; loc} = M.read in
  let ty = Typing.typ_of_number_ty ity in
  let binop = compile_binop binop in
  let* e1 = compile op1 in
  let* e =
    match ity with
    | Gmpz ->
      let* e2 = compile op2 in
      let mk_stmts _v e =
        assert (Gmp_types.Z.is_t ty);
        let name = Gmp.Z.name_arith_bop binop in
        let instr = Smart_stmt.rtl_call ~loc ~prefix:"" name [ e; e1; e2 ] in
        [ instr ]
      in
      let name = Misc.name_of_binop binop in
      M.modifying_env (fun env -> Gmp.Z.new_var ~loc ~name env kf None mk_stmts)
    | Rational ->
      let* e2 = compile op2 in
      M.modifying_env (fun env -> Gmp.Q.binop ~loc origin binop env kf e1 e2)
    | Analyses_types.C_integer _
    | Analyses_types.C_float _
    | Analyses_types.Real
    | Analyses_types.Nan ->
      let* e2 = compile op2 in
      M.return @@ Cil.new_exp ~loc @@ BinOp (binop, e1, e2, ty)
  in
  M.return (e, None, Some (Analyses_types.C_number, ""))

and compile_lhost = function
  | Var vi -> M.return (Cil_types.Var vi, vi.vorig_name)
  | Mem exp ->
    let* exp = M.without_registering_adata @@ compile exp in
    M.return (Cil_types.Mem exp, "")

and compile_offset = function
  | NoOffset -> M.return @@ Cil_types.NoOffset
  | Field (fieldinfo, offset) ->
    let* offset = compile_offset offset in
    M.return @@ Cil_types.Field (fieldinfo, offset)
  | Index (e, offset) ->
    let* e = M.without_registering_adata @@ compile e in
    let* offset = M.without_registering_adata @@ compile_offset offset in
    M.return @@ Cil_types.Index (e, offset)

and compile_lval (host, offset) =
  let* host, name = compile_lhost host in
  let* offset = compile_offset offset in
  M.return ((host, offset), name)

and compile_rte_guards cil =
  let* ({loc; kf}) = M.read in
  let compile_rte_guard rte =
    let* orig_state = M.get in
    let* () = M.modify @@ fun { env } ->
      Assert.push_pending_register_data ();
      let adata, env = Assert.empty ~loc kf env in
      Conf.{adata; env}
    in
    let* cil = compile @@ Interlang.Exp.rte rte in
    M.modify @@ fun {adata;env} ->
    let stmt, env =
      Assert.runtime_check
        ~adata
        ~pred_kind:Assert
        RTE
        kf
        env
        cil
        rte.rorigin
    in
    let env = Assert.do_pending_register_data env in
    let env = Env.add_stmt ~annot:rte.rorigin env stmt in
    {orig_state with env}
  in
  let* (cil,rtes) = M.flush cil in
  let* () = M.List.iter compile_rte_guard rtes in
  M.return cil

let generate_and_compile ~loc ~adata ~env ~kf m source =
  let interlang, _, _ =
    let env = Interlang_gen.{kf; loc; env; rte = true;
                             vars = Cil_datatype.Term.Map.empty} in
    let state = Cil_datatype.Term.Map.empty (* local variables *) in
    Interlang_gen.M.run ~env ~state @@ m source
  in
  Options.debug ~dkey ~level:3
    "@[interlang:@ @[%a@]@]" Interlang.Pretty.pp_exp interlang;
  let cil, _, Conf.{env; adata} =
    M.run ~env:{Conf.kf; loc; adata_register = true} ~state:Conf.{env; adata} @@
    compile ~flush_rtes:true interlang
  in
  Options.debug ~dkey ~level:4
    "@[Cil output:@ @[%a@]@]" Printer.pp_exp cil;
  cil, adata, env

let try_interlang il old =
  try if Options.Interlang.get () || Options.Interlang_force.get ()
    then il ()
    else old ()
  with Interlang_gen.Not_covered ->
    if Options.Interlang_force.get ()
    then Options.fatal
        "encountered construct unsupported by indirect compilation scheme;\
         run with \"-e-acsl-msg-key interlang:not_covered\" for details."
    else old ()

type 'a il_compiler = 'a -> Interlang.exp Interlang_gen.m

type 'a compiler =
  loc:Cil_types.location ->
  adata:Assert.t ->
  env:Env.t ->
  kf:Cil_types.kernel_function ->
  'a ->
  Cil_types.exp * Assert.t * Env.t

let try_il_compiler il old ~loc ~adata ~env ~kf x =
  try_interlang
    (fun () -> generate_and_compile ~loc ~adata ~env ~kf il x)
    (fun () -> old ~loc ~adata ~env ~kf x)
