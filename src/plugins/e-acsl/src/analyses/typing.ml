(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Analyses_types
open Analyses_datatype

(* Implement Figure 4 of J. Signoles' JFLA'15 paper "Rester statique pour
   devenir plus rapide, plus précis et plus mince". *)

let dkey = Options.Dkey.typing
module Error = Error.Make(struct let phase = dkey end)

(* In order to properly handle recursive functions the typing method has to
   store the result of the fixpoint algorithm on intervals before typing
   the inner block of the function. To this end, we stop the recursive
   depth-first descent of the AST at the level of function calls, and perform a
   breadth-first descent from the functions calls. We achieve this with
   a stack [pending_typing] which stores the roots of the functions calls from
   which a new descent is starting *)
let pending_typing : (unit -> unit) Stack.t = Stack.create ()

(******************************************************************************)
(** Datatype and constructor *)
(******************************************************************************)

let ikind ik = C_integer ik
let c_int = ikind IInt
let gmpz = Gmpz
let fkind fk = C_float fk
let rational = Rational
let nan = Nan

(******************************************************************************)
(** Basic operations *)
(******************************************************************************)
let ty_of_interv = Interval.ty_of_interv

let join_cty ty1 ty2 =
  let ty = Cil.arithmeticConversion ty1 ty2 in
  match ty.tnode with
  | TInt ik -> C_integer ik
  | TFloat fk -> C_float fk
  | _ ->
    Options.fatal "[typing] join failure: unexpected result %a"
      Printer.pp_typ ty

let join ty1 ty2 =
  if ty1 == ty2 then ty1
  else
    match ty1, ty2 with
    | Nan, Nan | Real, Real | Rational, Rational | Gmpz, Gmpz ->
      assert false
    | Nan, (C_integer _ | C_float _ | Gmpz | Rational | Real as ty)
    | (C_integer _ | C_float _ | Gmpz | Rational | Real as ty), Nan ->
      Options.fatal "[typing] join failure: number %a and nan"
        Number_ty.pretty
        ty
    | Real, (C_integer _ | C_float _ | Gmpz | Rational)
    | (C_integer _ | C_float _ | Rational | Gmpz), Real ->
      Real
    | Rational, (C_integer _ | C_float _ | Gmpz)
    | (C_integer _ | C_float _ | Gmpz), Rational
    | C_float _, Gmpz
    | Gmpz, C_float _ ->
      Rational
    | Gmpz, C_integer _
    | C_integer _, Gmpz ->
      Gmpz
    | C_float f1, C_float f2 ->
      join_cty (Cil_const.mk_tfloat f1) (Cil_const.mk_tfloat f2)
    | C_float f, C_integer n
    | C_integer n, C_float f ->
      join_cty (Cil_const.mk_tfloat f) (Cil_const.mk_tint n)
    | C_integer i1, C_integer i2 ->
      if Options.Gmp_only.get () then Gmpz
      else join_cty (Cil_const.mk_tint i1) (Cil_const.mk_tint i2)

exception Not_a_number
let typ_of_number_ty = function
  | C_integer _ when Options.Gmp_only.get () -> Gmp_types.Z.t ()
  | C_integer ik -> Cil_const.mk_tint ik
  | C_float _ when Options.Gmp_only.get () -> Gmp_types.Q.t ()
  | C_float fk -> Cil_const.mk_tfloat fk
  | Gmpz -> Gmp_types.Z.t ()
  (* for the time being, no reals but rationals instead *)
  | Rational -> Gmp_types.Q.t ()
  | Real -> Error.not_yet "real number type"
  | Nan -> raise Not_a_number

let typ_of_lty = function
  | Ctype cty -> cty
  | Linteger -> Gmp_types.Z.t ()
  | Lreal -> Error.not_yet "real type"
  | Lboolean | Ltype _ | Lvar _ | Larrow _ ->
    Options.fatal "unexpected logic type"

(******************************************************************************)
(** Memoization *)
(******************************************************************************)

type computed_info =
  { ty: Number_ty.t;  (* type required for the term *)
    cast: Number_ty.t option; (* if not [None], type of the context which the term
                                 must be cast to. If [None], no cast needed. *)
  }

let pp_computed_info fmt ci =
  let pp_cast fmt c = Format.fprintf fmt "(%a)" Number_ty.pretty c in
  Format.fprintf fmt "%a%a" (Pretty_utils.pp_opt pp_cast) ci.cast Number_ty.pretty ci.ty

(* Memoization module which retrieves the computed info of some terms. If the
   info is already computed for a term, it is never recomputed *)
module Memo: sig
  val memo:
    profile: Profile.t ->
    (term -> computed_info) ->
    term ->
    computed_info Error.result
  val get: profile: Profile.t -> term -> computed_info Error.result
  val clear: unit -> unit
end = struct

  (* The comparison over terms is the physical equality. It cannot be the
     structural one (given by [Cil_datatype.Term.equal]) because the very same
     term can be used in 2 different contexts which lead to different casts.

     By construction (see prepare_ast.ml), there are no physically equal terms
     in the E-ACSL's generated AST. Consequently the memoisation should be fully
     useless. However:
     - type info of many terms are accessed several times
     - the translation of E-ACSL guarded quantifications generates
       new terms (see module {!Quantif}) which must be typed. The term
       corresponding to the bound variable [x] is actually used twice: once in
       the guard and once for encoding [x+1] when incrementing it. The
       memoization is only useful here and indeed prevent the generation of one
       extra variable in some cases. *)
  let tbl : computed_info Error.result Misc.Id_term.Hashtbl.t =
    Misc.Id_term.Hashtbl.create 97

  (* The type of the logic function
     \\@ logic integer f (integer x) = x + 1;
     depends on the type of [x]. But our type system does not handle dependent
     types, which could let us express this dependency natively. Instead,
     we use the following trick to simulate the dependency: we type the
     corresponding definition (in the example [x+1]) several times,
     corresponding to the various calls to the function [f] in the program.
     We distinguish the calls to the function by storing the type of the
     arguments corresponding to each call, and we weaken the typing so that it
     is invariant when the arguments have the same type. *)
  let dep_tbl : computed_info Error.result Id_term_in_profile.Hashtbl.t
    = Id_term_in_profile.Hashtbl.create 97

  let get_dep profile t =
    try Id_term_in_profile.Hashtbl.find dep_tbl (t,profile)
    with Not_found -> Error.not_memoized ()

  let get_nondep t =
    try Misc.Id_term.Hashtbl.find tbl t
    with Not_found -> Error.not_memoized ()

  let get ~profile t =
    if Profile.is_empty profile
    then get_nondep t
    else get_dep profile t

  let memo_nondep f t =
    try Misc.Id_term.Hashtbl.find tbl t
    with Not_found ->
      let x =
        try Result.Ok (f t)
        with Error.Not_yet _ | Error.Typing_error _ as exn -> Result.Error exn
      in
      Misc.Id_term.Hashtbl.add tbl t x;
      x

  let memo_dep f t profile =
    try
      Id_term_in_profile.Hashtbl.find dep_tbl (t, profile)
    with Not_found ->
      let x =
        try Result.Ok (f t)
        with Error.Not_yet _ | Error.Typing_error _ as exn -> Result.Error exn
      in
      Id_term_in_profile.Hashtbl.add dep_tbl (t, profile) x;
      x

  let memo ~profile f t =
    if Profile.is_empty profile
    then memo_nondep f t
    else memo_dep f t profile

  let clear () =
    Options.debug ~dkey ~level:4 "clearing the typing tables";
    Misc.Id_term.Hashtbl.clear tbl;
    Id_term_in_profile.Hashtbl.clear dep_tbl

end

module Recursive_pred : sig
  val add: Profile.t -> logic_info -> unit
  val is_done: Profile.t -> logic_info -> bool
end = struct
  let known = ref LFProf.Set.empty

  let add profile li =
    known := LFProf.Set.add (li, profile) !known

  let is_done profile li = LFProf.Set.mem (li, profile) !known

end

(******************************************************************************)
(** {2 Coercion rules} *)
(******************************************************************************)

let assert_nan = function
  | Nan -> ()
  | C_integer _
  | C_float _
  | Gmpz
  | Rational
  | Real ->
    Options.abort ~current:true "got a number type where NaN was expected"

(* compute a new {!computed_info} by coercing the given type [ty] to the given
   context [ctx]. [op] is the type for the operator. *)
let coerce ~arith_operand ~ctx ty =
  if Number_ty.compare ty ctx = 1 then
    (* type larger than the expected context,
       so we must introduce an explicit cast *)
    { ty; cast = Some ctx }
  else
    (* only add an explicit cast if the context is [Gmp] and [ty] is not;
       or if the term corresponding to [ty] is an operand of an arithmetic
       operation which must be explicitly coerced in order to force the
       operation to be of the expected type. *)
  if (ctx = Gmpz && ty <> Gmpz) || arith_operand
  then { ty; cast = Some ctx }
  else
  if ctx = Rational && ty <> Rational
  then {ty; cast = Some ctx}
  else {ty; cast = None}

let number_ty_of_typ ~post ty =
  (* Consider GMP types only in a post typing phase *)
  if post && Gmp_types.Z.is_t ty then Gmpz
  else if post && Gmp_types.Q.is_t ty then Rational
  else
    match Ast_types.unroll_node ty with
    | TInt ik | TEnum { ekind = ik } -> C_integer ik
    | TFloat fk -> C_float fk
    | TVoid | TPtr _ | TArray _ | TFun _ | TComp _ | TBuiltin_va_list -> Nan
    | TNamed _ -> assert false

let ty_of_logic_ty ?term ~profile lty =
  let get_ty = function
    | Linteger -> Gmpz
    | Ctype ty -> number_ty_of_typ ~post:false ty
    | Lreal -> Real
    | Larrow _ -> Nan
    | Lboolean -> Error.not_yet "boolean logic type"
    | Ltype _ -> Error.not_yet "user-defined logic type"
    | Lvar _ -> Error.not_yet "type variable"
  in
  match term with
  | None -> get_ty lty
  | Some t ->
    if Options.Gmp_only.get () && lty = Linteger then Gmpz
    else
      let i = Interval.get_from_profile ~profile t in
      ty_of_interv i

(******************************************************************************)
(** {2 Type system} *)
(******************************************************************************)

(* generate a context [c]. Take --e-acsl-gmp-only into account iff [use_gmp_opt]
   is true. *)
let mk_ctx ~use_gmp_opt = function
  | C_float _ as f ->
    if use_gmp_opt && Options.Gmp_only.get () then Rational
    else f
  | C_integer _ as c ->
    if use_gmp_opt && Options.Gmp_only.get () then Gmpz
    else c
  | Gmpz | Rational | Real | Nan as c -> c

(* If a term has a C type, return it when it is smaller than [int] or [int], \
   return [None] if the term has no C type *)
let c_type_or_int_in_ival_of t i =
  let t = Logic_utils.remove_logic_coerce t in
  match t.term_type with
  | Ctype typ ->
    (match Ast_types.unroll_node typ with
     | TInt ik | TEnum { ekind = ik } when
         Interval.is_included_in_typ i typ
       ->
       if Cil.intTypeIncluded ik IInt
       then Some (C_integer IInt)
       else Some (C_integer ik)
     | TInt _ | TEnum _ | TFloat _ | TVoid | TPtr _ | TArray _ | TFun _
     | TComp _ | TBuiltin_va_list -> None
     | TNamed _ -> assert false)
  | _ -> None

(* the number_ty corresponding to [t] whenever use as an offset.
   In that case, it cannot be a GMP, so it must be coerced to an integral type
   in that case *)
let type_offset ~profile t =
  let i = Interval.get_from_profile ~profile t in
  match ty_of_interv i with
  | Gmpz -> C_integer ILongLong (* largest possible type *)
  | ty -> ty

(* type the term [t] in a context [ctx] by taking --e-acsl-gmp-only into account
   iff [use_gmp_opt] is true. *)
let rec type_term
    ~use_gmp_opt
    ?(under_lambda=false)
    ?(arith_operand=false)
    ?ctx
    ~profile
    t =
  Options.debug ~dkey ~level:5 "typing (sub-)term '%a'%a"
    Printer.pp_term t
    (Pretty_utils.pp_opt ~pre:" in context '" ~suf:"'" Number_ty.pretty) ctx;
  let open Current_loc.Operators in
  let<> UpdatedCurrentLoc = t.term_loc in
  let ctx = Option.map (mk_ctx ~use_gmp_opt) ctx in
  let compute_ctx ?ctx i =
    (* in order to get a minimal amount of generated casts for operators, the
       result is typed in the given context [ctx], but not the operands.
       This function returns a tuple (ctx_of_result, ctx_of_operands) *)
    match ctx with
    | None ->
      (* no context: factorize *)
      let ctx =  mk_ctx ~use_gmp_opt:true (ty_of_interv i) in
      ctx, ctx
    | Some ctx ->
      mk_ctx ~use_gmp_opt:true (ty_of_interv ~ctx i),
      mk_ctx ~use_gmp_opt:true (ty_of_interv i)
  in
  let infer t =
    (* this pattern matching implements the formal rules of the JFLA's paper
       (and of course also covers the missing cases). Also enforce the invariant
       that every subterm is typed, even if it is not an integer. *)
    match t.term_node with
    | TConst (Boolean _ | Integer _ | LChr _ | LEnum _ | LReal _)
    | TSizeOf _
    | TAlignOf _ ->
      let i = Interval.get_from_profile ~profile t in
      (* a constant or a left value directly under a lambda should be a gmp
         if the inferred context for the lambda is gmp *)
      ty_of_interv ?ctx ~use_gmp_opt:under_lambda i

    | TLval ((TVar {lv_type = Ctype { tnode = TInt ik }}, _) as tlv) ->
      type_term_lval ~profile tlv;
      C_integer ik

    | TLval tlv ->
      let i = Interval.get_from_profile ~profile t in
      type_term_lval ~profile tlv;
      ty_of_interv ?ctx ~use_gmp_opt:under_lambda i

    | Toffset(_, t')
    | Tblock_length(_, t')
    | TSizeOfE t'
    | TAlignOfE t' ->
      let i = Interval.get_from_profile ~profile t in
      (* [t'] must be typed, but it is a pointer *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan ~profile t');
      ty_of_interv ?ctx i

    | TBinOp (MinusPP, t1, t2) ->
      let i = Interval.get_from_profile ~profile t in
      (* [t1] and [t2] must be typed, but they are pointers *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan ~profile t1);
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan ~profile t2);
      ty_of_interv ?ctx i

    | TUnOp (LNot, t') ->
      let ctx =
        try
          let i = Interval.joins_from_profile ~profile [t'; t] in
          Some (mk_ctx ~use_gmp_opt:true (ty_of_interv i))
        (* during the typing phase, we catch the Not_yet exception so that [t']
           gets typed even if [t] is not. This prevents exceptions during the
           translation phase *)
        with Error.Not_yet _ ->
          None
      in
      ignore (type_term ~use_gmp_opt:true ~arith_operand:true ?ctx ~profile t');
      c_int (* converted into [t == 0] in case of GMP *)

    | TUnOp ((Neg | BNot), t') ->
      let ctx_res, ctx =
        compute_ctx ?ctx (Interval.joins_from_profile ~profile [t'; t])
      in
      ignore (type_term ~use_gmp_opt:true ~arith_operand:true ~ctx ~profile t');
      ctx_res


    | TBinOp ((PlusA | MinusA | Mult | Div | Mod | Shiftlt | Shiftrt | BAnd
              | BOr | BXor), t1, t2)
      ->
      let ctx_res, ctx =
        compute_ctx ?ctx (Interval.joins_from_profile ~profile [t2; t1; t])
      in
      (* it is enough to explicitly coerce when required one operand to [ctx]
         (through [arith_operand]) in order to force the type of the
         operation.  Heuristic: coerce the operand which is not a lval in
         order to lower the number of explicit casts *)
      let rec cast_first t1 t2 = match t1.term_node with
        | TLval _ -> false
        | TCast (true, _, t) -> cast_first t t2
        | _ -> true
      in
      let cast_first = cast_first t1 t2 in
      ignore
        (type_term ~use_gmp_opt:true ~arith_operand:cast_first ~ctx ~profile t1);
      ignore
        (type_term ~use_gmp_opt:true ~arith_operand:(not cast_first) ~ctx ~profile t2);
      ctx_res

    | TBinOp ((Lt | Gt | Le | Ge | Eq | Ne), t1, t2) ->
      assert
        (match ctx with
         | None -> true
         | Some c -> Number_ty.compare c c_int >= 0);
      let ctx = ctx_relation ~profile t1 t2 in
      ignore (type_term ~use_gmp_opt:true ~ctx ~profile t1);
      ignore (type_term ~use_gmp_opt:true ~ctx ~profile t2);
      c_int

    | TBinOp ((LAnd | LOr), t1, t2) ->
      let
        ty = ty_of_interv ?ctx (Interval.joins_from_profile ~profile [t2; t1])
      in
      (* both operands fit in an int. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:c_int ~profile t1);
      ignore (type_term ~use_gmp_opt:true ~ctx:c_int ~profile t2);
      ty

    | TCast (false, Ctype _, t') ->
      (* compute the smallest interval from the whole term [t] *)
      let i = Interval.get_from_profile ~profile t in
      (* nothing more to do: [i] is already more precise than what we could
         infer from the arguments of the cast. *)
      let ctx = ty_of_interv ?ctx i in
      ignore (type_term ~use_gmp_opt:true ~ctx ~profile t');
      ctx

    | TCast (true, _, t') ->
      let i = Interval.get_from_profile ~profile t in
      ignore (type_term ~use_gmp_opt ~arith_operand ?ctx ~profile t');
      ty_of_interv ~use_gmp_opt i
    (* TODO (NB) : Maybe can be merged ? *)

    | TCast (false, _,_) -> assert false

    | Tif (cond, t1, t2) ->
      ignore (type_predicate ~profile cond);
      let ctx =
        ty_of_interv ?ctx (Interval.joins_from_profile ~profile [t2; t1; t])
      in
      let ctx = mk_ctx ~use_gmp_opt:true ctx in
      ignore (type_term ~use_gmp_opt:true ~ctx ~profile t1);
      ignore (type_term ~use_gmp_opt:true ~ctx ~profile t2);
      ctx

    | Tat (t, _) ->
      (type_term ~use_gmp_opt ~arith_operand ?ctx ~profile t).ty

    | TAddrOf tlv
    | TStartOf tlv ->
      (* it is a pointer, but subterms must be typed. *)
      type_term_lval tlv ~profile;
      Nan

    | Tbase_addr (_, t) ->
      (* it is a pointer, but subterms must be typed. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan ~profile t);
      Nan

    | TBinOp ((PlusPI | MinusPI), t1, t2) ->
      (* both [t1] and [t2] must be typed. *)
      ignore (type_term ~use_gmp_opt:true ~ctx:Nan ~profile t1);
      let ctx = type_offset ~profile t2 in
      ignore (type_term ~use_gmp_opt:false ~ctx ~profile t2);
      Nan

    | Tapp(li, _, args) ->
      let type_arg x =
        let ctx = match x.term_type with
          | Linteger ->
            (* If the function parameter is an integer,
               the kernel introduces a coercion to integer, so we will
               always see integer here.*)
            begin match x.term_node with
              | TCast (true,_,_) -> None
              |_ -> if Options.Gmp_only.get() then Some Gmpz else None
            end
          | Lreal -> Some Real
          | Lboolean | Ctype _ | Ltype _ | Larrow _ | Lvar _ -> None
        in
        ignore
          (type_term
             ~use_gmp_opt:true
             ~under_lambda:true
             ~arith_operand
             ?ctx
             ~profile x)
      in
      let type_args type_arg =
        (* type all arguments using typing function [type_arg] while
           making sure that later arguments are typed even if an exception is
           raised during the typing of an earlier argument. *)
        let exn_o = ref None in
        let type_arg arg =
          try type_arg arg
          with exn -> match !exn_o with None -> exn_o := Some exn | Some _ -> ()
        in
        List.iter (fun arg -> type_arg arg) args;
        Option.iter raise !exn_o
      in
      if Builtins.mem li.l_var_info.lv_name then
        let typ_arg lvi arg =
          (* a built-in is a C function, so the context is necessarily a C
             type. *)
          let ctx = ty_of_logic_ty ~profile lvi.lv_type in
          ignore (type_term ~use_gmp_opt:false ~ctx ~profile arg)
        in
        List.iter2 typ_arg li.l_profile args;
        (* [li.l_type is [None] for predicate only: not possible here.
           Thus using [Option.get] is fine *)
        ty_of_logic_ty ~profile (Option.get li.l_type)
      else begin
        (* TODO: what if the type of the parameter is smaller than the inferred
           type of the argument? For now, it is silently ignored (both
           statically and at runtime)... *)
        (* TODO: recursive call in arguments of function call *)
        match li.l_body with
        | LBpred p ->
          (* possible to have an [LBpred] here because we transformed
             [Papp] into [Tapp] *)
          type_args type_arg;
          let new_profile =
            Profile.make
              li.l_profile
              (List.map (Interval.get_from_profile ~profile) args)
          in
          Stack.push
            (fun () ->
               ignore (type_predicate ~profile:new_profile p))
            pending_typing;
          c_int
        | LBterm t_body ->
          type_args type_arg;
          let new_profile =
            Profile.make
              li.l_profile
              (List.map (Interval.get_from_profile ~profile) args)
          in
          let gmp,ctx_body = match li.l_type with
            | Some (Ctype typ) ->
              false, Some (number_ty_of_typ ~post:false typ)
            | _ ->
              true, if Options.Gmp_only.get() then Some Gmpz else ctx
          in
          Stack.push
            (fun () ->
               ignore
                 (type_term
                    ~use_gmp_opt:false
                    ~under_lambda:true
                    ~arith_operand
                    ?ctx:ctx_body
                    ~profile:new_profile
                    t_body))
            pending_typing;
          (* If the logic function has a given C number type, we generate a
             function returning this type, otherwise we use the interval
             inference *)
          (match li.l_type with
           | Some (Ctype { tnode = TInt ikind }) ->
             C_integer ikind
           | Some (Ctype { tnode = TFloat fkind }) ->
             C_float fkind
           | None
           | Some (Ctype { tnode =
                             ( TVoid
                             | TPtr _
                             | TEnum _
                             | TArray _
                             | TFun _
                             | TNamed _
                             | TComp _
                             | TBuiltin_va_list) })
           | Some (Lboolean | Linteger | Lreal | Ltype _ | Lvar _ | Larrow _) ->
             ty_of_interv
               ?ctx:ctx_body
               ~use_gmp_opt:(gmp && use_gmp_opt)
               (Interval.get_from_profile ~profile t))
        | LBnone ->
          (match args with
           | [ t1; t2; {term_node = Tlambda([ _ ], _)} as lambda ] ->
             let range = ty_of_interv (Interval.join_plus_one ~profile t2 t1) in
             ignore
               (type_term
                  ~use_gmp_opt:true
                  ~arith_operand:true
                  ~profile
                  ~ctx:range
                  t1);
             ignore
               (type_term
                  ~use_gmp_opt
                  ~arith_operand
                  ~profile
                  ~ctx:range
                  t2);
             let ival = Interval.get_from_profile ~profile t in
             let ty = ty_of_interv ival ~use_gmp_opt:true ?ctx in
             ignore (type_term ~use_gmp_opt:true ?ctx ~profile lambda);
             ty
           | [] | _ :: _ ->
             let type_arg arg =
               ignore @@ type_term ~use_gmp_opt:true ~arith_operand:false ~profile arg
             in
             type_args type_arg;
             (* TODO : improve error message to distinguish error messages
                corresponding to unsupported primitives and wrong application
                of supported primitive
                (one is a fatal and the other is a not_yet) *)
             Error.not_yet "logic functions or predicates with no definition \
                            nor reads clause")
        | LBreads _ ->
          type_args type_arg;
          Error.not_yet "logic functions or predicates performing read accesses"
        | LBinductive _ ->
          type_args type_arg;
          Error.not_yet "inductive definitions outside of \
                         subset described in the reference manual"
      end

    | Tunion _ -> Error.not_yet "tset union"
    | Tinter _ -> Error.not_yet "tset intersection"
    | Tcomprehension (_,_,_) -> Error.not_yet "tset comprehension"

    | Trange(None, _) | Trange(_, None) ->
      Options.abort ~current:true "unbounded ranges are not part of E-ACSl"
    | Trange(Some n1, Some n2) ->
      ignore (type_term ~use_gmp_opt ~profile n1);
      ignore (type_term ~use_gmp_opt ~profile n2);
      let i = Interval.get_from_profile ~profile t in
      ty_of_interv ?ctx i

    | Tlet(li, t) ->
      let li_t = Misc.term_of_li li in
      ignore (type_term ~use_gmp_opt:true ~profile li_t);
      (type_term ~use_gmp_opt:true ?ctx ~profile t).ty
    | Tlambda ([ _ ],lt) ->
      (type_term ~use_gmp_opt:true ~under_lambda:true ?ctx ~profile lt).ty
    | Tlambda (_,_) -> Error.not_yet "lambda"
    | TDataCons (_,_) -> Error.not_yet "datacons"
    | TUpdate (_,_,_) -> Error.not_yet "update"

    | Tnull
    | TConst (LStr _ | LWStr _)
    | Ttypeof _
    | Ttype _
    | Tempty_set  -> Nan
  in
  let t = Logic_normalizer.get_term t in
  let pp_call_with_result fmt result =
    Format.fprintf fmt "type_term ~profile:%a ~use_gmp_opt:%b"
      Profile.pretty profile
      use_gmp_opt;
    Option.iter (Format.fprintf fmt " ~ctx:%a" Number_ty.pretty) ctx;
    if not @@ Profile.is_empty profile then
      Format.fprintf fmt " ~profile:%a" Profile.pretty profile;
    if under_lambda then Format.fprintf fmt " ~under_lambda:true";
    if arith_operand then Format.fprintf fmt " ~arith_operand:true";
    Format.fprintf fmt " %a = %a" Printer.pp_term t pp_computed_info result;
  in
  match
    Memo.memo ~profile
      (fun t ->
         let ty = infer t in
         let ctx =
           match ctx with
           | None -> { ty; cast = None }
           | Some ctx -> coerce ~arith_operand ~ctx ty
         in
         Rte_analysis.iter_on_guards t (type_predicate ~profile);
         ctx)
      t
  with
  | Result.Ok result ->
    Options.debug ~dkey ~level:5 "%a" pp_call_with_result result;
    result
  | Result.Error exn -> raise exn

and type_term_lval ~profile (host, offset) =
  type_term_lhost ~profile host;
  type_term_offset ~profile offset

and type_term_lhost ~profile t  = match t with
  | TVar _
  | TResult _ -> ()
  | TMem t ->
    let computed_ty = type_term ~use_gmp_opt:false ~ctx:Nan ~profile t
    in assert_nan computed_ty.ty

and type_term_offset ~profile t = match t with
  | TNoOffset -> ()
  | TField(_, toff)
  | TModel(_, toff) -> type_term_offset ~profile toff
  | TIndex(t, toff) ->
    let ctx = type_offset ~profile t in
    ignore (type_term ~use_gmp_opt:false ~ctx ~profile t);
    type_term_offset ~profile toff

(* assign a number type to a variable bound by a quantifiers. See [ctx_relation]
   for an explanation of the cases *)
and number_ty_bound_variable ~profile (t1, lv, t2) =
  let i = Interval.joins_from_profile ~profile [t2; t1] in
  match lv.lv_type with
  | Linteger ->
    let ty =
      match c_type_or_int_in_ival_of t2 i with
      | Some ty -> ty
      | None -> ty_of_interv ~ctx:Gmpz i
    in mk_ctx ~use_gmp_opt:true ty
  | Ctype ty ->
    let ty = Ast_types.unroll ty in
    (match ty.tnode with
     | TInt ik | TEnum { ekind = ik} ->
       join
         (ty_of_interv i)
         (mk_ctx ~use_gmp_opt:true (C_integer ik))
     | _ ->
       Options.fatal "unexpected C type %a for quantified variable %a"
         Printer.pp_typ ty
         Printer.pp_logic_var lv)
  | lty ->
    Options.fatal "unexpected logic type %a for quantified variable %a"
      Printer.pp_logic_type lty
      Printer.pp_logic_var lv

and type_bound_variables ~profile (t1, lv, t2) =
  let ctx = number_ty_bound_variable ~profile (t1, lv, t2) in
  (* forcing when typing bounds prevents to generate an extra useless
     GMP variable when --e-acsl-gmp-only *)
  ignore(type_term ~use_gmp_opt:false ~ctx ~profile t1);
  ignore(type_term ~use_gmp_opt:false ~ctx ~profile t2)

and type_predicate ~profile p =
  Options.debug ~dkey ~level:3 "typing predicate: %a" Printer.pp_predicate p;
  let
    do_both f g = (try f() with e -> try g(); raise e with | _ -> raise e); g()
  in
  let p = Logic_normalizer.get_pred p in
  let open Current_loc.Operators in
  let<> UpdatedCurrentLoc = p.pred_loc in
  (* this pattern matching also follows the formal rules of the JFLA's paper *)
  match p.pred_content with
  | Pfalse | Ptrue -> ()
  | Papp(li, _, args) ->
    begin
      List.iter
        (fun x -> ignore (type_term ~use_gmp_opt: true ~profile x))
        args;
      match li.l_body with
      | LBpred p ->
        let new_profile =
          Profile.make
            li.l_profile
            (List.map (Interval.get_from_profile ~profile) args)
        in
        if not (Recursive_pred.is_done new_profile li)
        then
          (Recursive_pred.add new_profile li;
           ignore (type_predicate ~profile:new_profile p))
      | LBnone -> ()
      | LBreads _ -> ()
      | LBinductive _ -> ()
      | LBterm _ ->
        Options.fatal "unexpected logic definition"
          Printer.pp_predicate p
    end
  | Pdangling _ -> Error.not_yet "\\dangling"
  | Prel(_, t1, t2) ->
    let ctx =
      try
        Some (ctx_relation ~profile t1 t2)
      (* as before, we catch the exception Not_yet to ensure that [t1] and [t2]
         get typed in all cases. *)
      with Error.Not_yet _ ->
        None
    in
    let on t () = ignore (type_term ~use_gmp_opt:true ?ctx ~profile t) in
    do_both (on t1) (on t2);
  | Pand(p1, p2)
  | Por(p1, p2)
  | Pxor(p1, p2)
  | Pimplies(p1, p2)
  | Piff(p1, p2) ->
    let on p () = type_predicate ~profile p in
    do_both (on p1) (on p2)
  | Pnot p ->
    type_predicate ~profile p
  | Pif(c, p1, p2) ->
    ignore (type_predicate ~profile c);
    let on p  () = type_predicate ~profile p in
    do_both (on p1) (on p2)
  | Plet(li, p) ->
    let li_t = Misc.term_of_li li in
    ignore (type_term ~use_gmp_opt:true ~profile li_t);
    type_predicate ~profile p
  | Pforall _
  | Pexists _ ->
    let guards, goal = Bound_variables.get_preprocessed_quantifier p in
    List.iter
      (fun (t1, x, t2) -> type_bound_variables ~profile (t1, x, t2))
      guards;
    type_predicate ~profile goal
  | Pseparated tlist ->
    List.iter
      (fun t -> ignore (type_term ~use_gmp_opt:false ~ctx:Nan ~profile t))
      tlist
  | Pinitialized(_, t)
  | Pfreeable(_, t)
  | Pallocable(_, t)
  | Pvalid(_, t)
  | Pvalid_read(_, t)
  | Pobject_pointer(_,t)
  | Pvalid_function t ->
    ignore (type_term ~use_gmp_opt:false ~ctx:Nan ~profile t)
  | Paligned (t, n) ->
    let sizet_kind = Machine.sizeof_kind () in
    let size_t = C_integer sizet_kind in
    ignore (type_term ~use_gmp_opt:false ~ctx:Nan ~profile t);
    ignore (type_term ~use_gmp_opt:true ~ctx:size_t ~profile n)
  | Pat(p, _) -> type_predicate ~profile p
  | Pfresh _ -> Error.not_yet "\\fresh"

(** When typing a binary relation, generate the context in which the relation
    should be typed, to avoid spurious casts:
    - Compute the union of the interval of the two terms
    - Check if any of the term has a number C type that contains this union,
      and if so use this C type, or [int] if this type is smaller than [int]
    - Otherwise use the type corresponding to the union *)
and ctx_relation ~profile t1 t2 =
  let i = Interval.joins_from_profile ~profile [t2; t1] in
  let ty =
    match c_type_or_int_in_ival_of t1 i, c_type_or_int_in_ival_of t2 i with
    | Some ty, _
    | None, Some ty -> ty
    | None, None -> ty_of_interv ~ctx:c_int i
  in mk_ctx ~use_gmp_opt:true ty

let type_term ~use_gmp_opt ?ctx ~profile t =
  Options.debug ~dkey ~level:4 "typing term '%a'%a."
    Printer.pp_term t
    (Pretty_utils.pp_opt ~pre:" in context '" ~suf:"'" Number_ty.pretty) ctx;
  ignore (type_term ~use_gmp_opt ?ctx ~profile t);
  while not (Stack.is_empty pending_typing) do
    Stack.pop pending_typing ()
  done

let type_named_predicate ~profile p =
  Options.debug ~dkey ~level:3 "typing predicate: %a."
    Printer.pp_predicate p;
  ignore (type_predicate ~profile p);
  while not (Stack.is_empty pending_typing) do
    Stack.pop pending_typing ()
  done

let unsafe_set t ?ctx ~logic_env ty =
  let profile = Logic_env.get_profile logic_env in
  let ctx = match ctx with None -> ty | Some ctx -> ctx in
  let mk _ = coerce ~arith_operand:false ~ctx ty in
  ignore (Memo.memo mk ~profile t)

(******************************************************************************)
(** {2 Getters} *)
(******************************************************************************)

let get_number_ty ~logic_env t =
  let profile = Logic_env.get_profile logic_env in
  (Error.retrieve_preprocessing "typing" (Memo.get ~profile) t Printer.pp_term).ty

(* {!typ_of_integer}, but handle the not-integer cases. *)
let extract_typ t ty =
  try typ_of_number_ty ty
  with Not_a_number ->
  match t.term_type with
  | Ctype _ as lty -> Logic_utils.logicCType lty
  | Lboolean | Linteger | Lreal ->
    Options.fatal "unexpected context NaN for term %a" Printer.pp_term t
  | Ltype _ -> Error.not_yet "unsupported logic type: user-defined type"
  | Lvar _ -> Error.not_yet "unsupported logic type: type variable"
  | Larrow _ -> Error.not_yet "unsupported logic type: type arrow"

let get_typ ~logic_env t =
  extract_typ t (get_number_ty ~logic_env t)

let get_cast ~logic_env t =
  let profile = Logic_env.get_profile logic_env in
  let info =
    Error.retrieve_preprocessing "typing" (Memo.get ~profile) t Printer.pp_term
  in
  try Option.map typ_of_number_ty info.cast
  with Not_a_number -> None

let get_effective_ty ~logic_env t =
  let profile = Logic_env.get_profile logic_env in
  let info =
    Error.retrieve_preprocessing "typing" (Memo.get ~profile) t Printer.pp_term
  in
  match info.cast with
  | Some ty -> ty
  | None -> info.ty

let get_effective_typ ~logic_env t =
  extract_typ t (get_effective_ty ~logic_env t)

let clear = Memo.clear

let typing_visitor profile = object
  inherit E_acsl_visitor.visitor dkey

  (* global logic functions and predicates are evaluated are callsites *)
  method !glob_annot _ = Cil.SkipChildren

  method !vpredicate p =
    (* Do not raise a warning for e-acsl errors at preprocessing time,
       those errors are stored in the table and warnings are raised at
       translation time *)
    ignore
      (try type_named_predicate ~profile p
       with Error.Not_yet _ | Error.Typing_error _  -> ());
    Cil.SkipChildren
end

let type_program ast =
  let visitor = typing_visitor Profile.empty in
  visitor#visit_file ast

let type_code_annot lenv annot =
  let visitor = typing_visitor lenv in
  ignore @@ visitor#visit_code_annot annot

let preprocess_predicate ~logic_env p =
  Logic_normalizer.preprocess_predicate p;
  Bound_variables.preprocess_predicate p;
  Interval.preprocess_predicate ~logic_env p;
  let profile = Logic_env.get_profile logic_env in
  let visitor = typing_visitor profile in
  ignore @@ visitor#visit_predicate p

let preprocess_rte ~logic_env rte =
  Logic_normalizer.preprocess_annot rte;
  Bound_variables.preprocess_annot rte;
  ignore (Interval.preprocess_code_annot ~logic_env rte);
  let profile = Logic_env.get_profile logic_env in
  type_code_annot profile rte

let preprocess_term ~use_gmp_opt ?ctx ~logic_env t =
  ignore (Interval.preprocess_term ~logic_env t);
  let profile = Logic_env.get_profile logic_env in
  ignore (type_term ~use_gmp_opt ?ctx ~profile t);
