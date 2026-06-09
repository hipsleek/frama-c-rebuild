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
open Interval_utils
open Cil_datatype

(* Implements Figure 3 of J. Signoles' JFLA'15 paper "Rester statique pour
   devenir plus rapide, plus précis et plus mince".
   Also implements a support for real numbers. *)
let dkey = Options.Dkey.interval
module Error = Error.Make(struct let phase = dkey end)

(* ********************************************************************* *)
(* Basic datatypes and operations *)
(* ********************************************************************* *)

(* TODO: soundness of any downcast is not checked *)
let cast ~src ~dst = match src, dst with
  | Ival i1, Ival i2 ->
    Ival (Ival.meet i1 i2)
  | _, Float(_, Some _) ->
    assert false
  | Rational, Real
  | Float _, (Rational | Real) ->
    src
  | _, _ ->
    (* No need to optimize the other cases: if someone writes a cast
       (in particular, from integer to float/real or conversely), it is
       certainly on purpose . *)
    dst

(* a-b; or 0 if negative *)
let length a b = Z.max Z.zero (Z.succ (Z.sub a b))

(* minimal distance between two intervals given by their respective lower and
   upper bounds, i.e. the length between the lower bound of the second interval
   bound and the upper bound of the first interval. *)
let min_delta (_, max1) (min2, _) = match max1, min2 with
  | Some m1, Some m2 -> length m2 m1
  | _, None | None, _ -> Z.zero

(* maximal distance between between two intervals given by their respective
   lower and upper bounds, i.e. the length between the upper bound of the second
   interval and the lower bound of the first interval.
   @return None for \infty *)
let max_delta (min1, _) (_, max2) = match min1, max2 with
  | Some m1, Some m2 -> Some (length m2 m1)
  | _, None | None, _ -> None

(* Compute the smallest type (bigger than [int]) which can contain the whole
   interval. It is the \theta operator of the JFLA's paper. *)
let ty_of_interv ?ctx ?(use_gmp_opt = false) = function
  | Float(fk, _) -> C_float fk
  | Rational -> Rational
  | Real -> Real
  | Nan -> Nan
  | Ival iv ->
    try
      let kind = ikind_of_ival iv in
      (match ctx with
       | None
       | Some Nan ->
         C_integer kind
       | Some Gmpz ->
         if use_gmp_opt then Gmpz else C_integer kind
       | Some (C_integer ik as ctx) ->
         (* return [ctx] type for types smaller than int to prevent superfluous
            casts in the generated code *)
         if Cil.intTypeIncluded kind ik then ctx else C_integer kind
       | Some (C_float _ | Rational | Real as ty) ->
         ty)
    with Interval_utils.Not_representable_ival ->
    match ctx with
    | None | Some(C_integer _ | Gmpz | Nan) -> Gmpz
    | Some (C_float _ | Rational) -> Rational
    | Some Real -> Real

let is_included_in_typ i typ = is_included i (interv_of_typ typ)

(* ********************************************************************* *)
(* main algorithm *)
(* ********************************************************************* *)

(* Memoization module which retrieves the computed info of some terms *)
module Memo: sig
  val memo:
    force_infer:bool -> Profile.t -> (term -> ival) -> term -> ival Error.result
  val get: Profile.t -> term -> ival Error.result
  val replace : Profile.t -> term -> ival -> unit
  val clear: unit -> unit
end = struct
  (* The comparison over terms is the physical equality. It cannot be the
     structural one (given by [Cil_datatype.Term.equal]) for efficiency.

     By construction (see prepare_ast.ml), there are no physically equal terms
     in the E-ACSL's generated AST, but
     - type info of many terms are accessed several times
     - the translation of E-ACSL guarded quantifications generates
       new terms (see module {!Quantif}) which must be typed. The term
       corresponding to the bound variable [x] is actually used twice: once in
       the guard and once for encoding [x+1] when incrementing it. *)
  let nondep_tbl : ival Error.result Misc.Id_term.Hashtbl.t =
    Misc.Id_term.Hashtbl.create 97

  (* The interval of the logic function
     //@ logic integer f (integer x) = x + 1;
     depends on the interval of [x]. The same term [x+1] can be inferred to be
     in different intervals if the function [f] is applied several times with
     different arguments. In this case, we add the interval of [x] as a key
     to retrieve the type of [x+1].
     There are two other kinds of binders for logical variables: [TLet] and
     the quantifiers, however in those cases, a term is only ever translated
     once. *)

  let dep_tbl : ival Error.result Id_term_in_profile.Hashtbl.t
    = Id_term_in_profile.Hashtbl.create 97

  (* Small functor to access the result of a memoized inference *)
  module Accesses (X : Datatype.S_with_hashtbl)
      (Tbl: sig val tbl : ival Error.result X.Hashtbl.t end)
    : sig
      val get : X.Hashtbl.key -> ival Error.result
      val memo : force_infer:bool -> (term -> ival) -> term -> X.Hashtbl.key ->
        (ival, exn) Result.t
      val replace : X.Hashtbl.key -> ival -> unit
    end = struct
    let get k =
      try X.Hashtbl.find Tbl.tbl k
      with Not_found -> Error.not_memoized ()

    let memo ~force_infer f t k =
      if force_infer then
        let x =
          try Result.Ok (f t);
          with Error.Not_yet _ | Error.Typing_error _ as exn -> Result.Error exn
        in
        X.Hashtbl.replace Tbl.tbl k x;
        x
      else
        try X.Hashtbl.find Tbl.tbl k
        with Not_found ->
          let x =
            try Result.Ok (f t);
            with
              Error.Not_yet _ | Error.Typing_error _ as exn -> Result.Error exn
          in
          X.Hashtbl.add Tbl.tbl k x;
          x

    let replace x i =
      X.Hashtbl.replace Tbl.tbl x (Ok i)
  end

  module Nondep = Accesses (Misc.Id_term) (struct let tbl = nondep_tbl end)
  module Dep = Accesses (Id_term_in_profile) (struct let tbl = dep_tbl end)

  let get profile t =
    if Profile.is_empty profile
    then Nondep.get t
    else Dep.get (t,profile)

  let memo ~force_infer profile f t =
    if Profile.is_empty profile
    then Nondep.memo ~force_infer f t t
    else Dep.memo ~force_infer f t (t,profile)

  let replace profile t i =
    if Profile.is_empty profile
    then Nondep.replace t i
    else Dep.replace (t,profile) i

  let clear () =
    Options.feedback ~level:4 "clearing the typing tables";
    Misc.Id_term.Hashtbl.clear nondep_tbl;
    Id_term_in_profile.Hashtbl.clear dep_tbl
end

(* When performing fixpoint algorithm on function arguments, we may want to
   forcefully replace the interval inferred by the algorithm with a larger
   interval
   -[replace_args_ival] performs this operation for a given list of arguments
   corresponding to a particular call
   - [replace_all_args_ival] performs this operation for all arguments that have
     been called by this function during the same recursive calls. *)

let replace_args_ival ~logic_env li args args_ival =
  let profile = Logic_env.get_profile logic_env in
  List.iter2
    (fun x t -> Memo.replace profile t (Logic_var.Map.find x args_ival))
    li.l_profile
    args

let replace_all_args_ival li args_ival =
  let args_map = LF_env.find_args li in
  List.iter
    (fun x ->
       let i = Logic_var.Map.find x args_ival in
       (Misc.Id_term.Hashtbl.iter
          (fun t profile -> Memo.replace profile t i)
          (Logic_var.Map.find x args_map)))
    li.l_profile

(* ********************************************************************* *)
(* Main functions *)
(* ********************************************************************* *)

let infer_sizeof ty =
  try singleton_of_int (Cil.bytesSizeOf ty)
  with Cil.SizeOfError _ -> interv_of_typ (Machine.sizeof_type ())

let infer_alignof ty =
  try singleton_of_int (Cil.bytesAlignOf ty)
  with Cil.SizeOfError _ -> interv_of_typ (Machine.sizeof_type ())

(* Infer the interval of an extended quantifier \sum or \product.
   [lambda] is the interval of the lambda term, [min] (resp. [max]) is the
   interval of the minimum (resp. maximum) and [oper] is the identifier of the
   extended quantifier (\sum, or \product). The returned ival is the interval of
   the extended quantifier. *)
let infer_sum_product oper lambda min max = match lambda, min, max with
  | Ival lbd_iv, Ival lb_iv, Ival ub_iv ->
    (try
       let min_lambda, max_lambda = Ival.min_and_max lbd_iv in
       let minmax_lb = Ival.min_and_max lb_iv in
       let minmax_ub = Ival.min_and_max ub_iv in
       let lb, ub = match oper.lv_name with
         | "\\sum" ->
           (* the lower (resp. upper) bound is the min (resp. max) value of the
              lambda term, times the min (resp. max) distance between them if
              the sign is positive, or conversely if the sign is negative *)
           let lb = match min_lambda with
             | None -> None
             | Some z ->
               if Z.sign z = -1
               then Option.map (Z.mul z) (max_delta minmax_lb minmax_ub)
               else Some (Z.mul z (min_delta minmax_lb minmax_ub))
           in
           let ub = match max_lambda with
             | None -> None
             | Some z ->
               if Z.sign z = -1
               then Some (Z.mul z (min_delta minmax_lb minmax_ub))
               else Option.map (Z.mul z) (max_delta minmax_lb minmax_ub)
           in
           lb, ub
         | "\\product" ->
           (* the lower (resp. upper) bound is the min (resp. max) value of the
              lambda term in absolute value, power the min (resp. max) distance
              between them if the sign is positive, or conversely for both the
              lambda term and the exponent if the sign is negative. If the sign
              is negative, the minimum is also negative. *)
           let min, max =
             match min_lambda, max_lambda with
             | None, None as res -> res
             | None, Some m | Some m, None -> Some m, None
             | Some min, Some max ->
               let abs_min = Z.abs min in
               let abs_max = Z.abs max in
               Some (Z.min abs_min abs_max), Some (Z.max abs_min abs_max)
           in
           let lb = match min_lambda with
             | None -> None
             | Some z ->
               if Z.sign z = -1 then
                 (* the lower bound is (possibly) negative *)
                 Option.map2
                   (fun m max ->
                      match min_lambda, max_lambda with
                      | Some mil, Some mal when Z.lt (Z.abs mil) (Z.abs mal) ->
                        (* [lambda] contains both positive and negative values
                           and |mil| < |mal|: instead of [-mal^m], the min is
                           optimized to [mil * mal^(m-1)] *)
                        Z.mul mil (Z.pow max (Z.to_int m - 1))
                      | None, _ | _, None | Some _, Some _ ->
                        Z.neg (Z.pow max (Z.to_int m)))
                   (max_delta minmax_lb minmax_ub)
                   max
               else
                 (* all numbers are positive:
                    the lower bound is necessarily positive *)
                 Option.map
                   (fun m -> Z.pow m (Z.to_int (min_delta minmax_lb minmax_ub)))
                   min
           in
           let ub =
             Option.map2
               (fun m max ->
                  match max_lambda with
                  | Some ml when Z.lt ml Z.zero && not (Z.is_one m) ->
                    (* when [lambda] is necessarily negative with an odd number
                       of iterations (>1), the result is necessarily negative,
                       so smaller than the maximal (positive) value. Therefore,
                       it is possible to reduce the number of iteration by 1. *)
                    let exp = Z.to_int m in
                    Z.pow max (exp - exp mod 2)
                  | None | Some _ ->
                    Z.pow max (Z.to_int m))
               (max_delta minmax_lb minmax_ub)
               max
           in
           lb, ub
         | s ->
           Options.fatal "unexpected logic function '%s'" s
       in
       Ival (Ival.inject_range lb ub)
     with
     | Abstract_interp.Error_Bottom -> bottom
     | Z.Overflow (* if the exponent of \product is too high *) -> top_ival)
  | _ -> Error.not_yet "extended quantifiers with non-integer parameters"

let rec infer ~force ~logic_env t =
  let debug_analysis_result ~level t ival =
    Error.iter (fun ival ->
        Options.debug ~dkey ~level "%a : %a"
          Printer.pp_term t
          Analyses_types.pp_ival ival
      ) ival;
    ival
  in
  debug_analysis_result ~level:4 t @@
  let recurse ?(force = force) ?(logic_env = logic_env) t =
    infer ~force ~logic_env t
  in
  let get_cty t = match t.term_type with Ctype ty -> ty | _ -> assert false in
  let get_res = Error.map (fun x -> x) in
  let t = Logic_normalizer.get_term t in
  let ival_arith_binop = function
    | PlusA -> Ival.add_int
    | MinusA -> Ival.sub_int
    | Mult -> Ival.mul
    | Div -> Ival.div
    | Mod -> Ival.c_rem
    | Shiftlt -> Ival.shift_left
    | Shiftrt -> Ival.shift_right
    | BAnd -> Ival.bitwise_and
    | BXor -> Ival.bitwise_xor
    | BOr -> Ival.bitwise_or
    | _ -> assert false
  in
  let compute t =
    match t.term_node with
    | TConst (Boolean b) -> singleton (if b then Z.one else Z.zero)
    | TConst (Integer (n, _)) -> singleton n
    | TConst (LChr c) ->
      let n = Cil.charConstToInt c in
      singleton n
    | TConst (LEnum enumitem) ->
      let rec find_idx n = function
        | [] -> assert false
        | ei :: l -> if ei == enumitem then n else find_idx (n + 1) l
      in
      let n = Z.of_int (find_idx 0 enumitem.eihost.eitems) in
      singleton n
    | TLval lv -> infer_term_lval ~force ~logic_env lv
    | TSizeOf ty -> infer_sizeof ty
    | TSizeOfE t ->
      ignore (recurse t);
      infer_sizeof (get_cty t)
    | TAlignOf ty -> infer_alignof ty
    | TAlignOfE t ->
      ignore (recurse t);
      infer_alignof (get_cty t)

    | TUnOp (Neg, t) ->
      let i = recurse t in
      Error.map (lift_unop Ival.neg_int) i
    | TUnOp (BNot, t) ->
      let i = recurse t in
      Error.map (lift_unop Ival.bitwise_signed_not) i
    | TUnOp (LNot, t) ->
      ignore (recurse t);
      Ival Ival.zero_or_one

    | TBinOp ((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr), t1, t2) ->
      ignore (recurse t1);
      ignore (recurse t2);
      Ival Ival.zero_or_one

    | TBinOp ((PlusA | MinusA | Mult | Div | Mod | Shiftlt
              | Shiftrt | BAnd | BXor | BOr) as op , t1, t2) ->
      let i1 = recurse t1 in
      let i2 = recurse t2 in
      Error.map2 (lift_arith_binop (ival_arith_binop op)) i1 i2

    | TCast (false, Ctype ty, t) ->
      let src = recurse t in
      let dst = interv_of_typ ty in
      Error.map (fun src -> cast ~src ~dst) src
    | TCast (true, _, t) -> get_res (recurse t)
    | TCast (false, _,_) -> assert false
    | Tif (cond, t1, t2) ->
      ignore (infer_predicate ~logic_env cond);
      let logic_env_tbranch, logic_env_fbranch =
        compute_logic_env_if_branches logic_env cond
      in
      let i2 = recurse ~logic_env:logic_env_tbranch t1 in
      let i3 = recurse ~logic_env:logic_env_fbranch t2 in
      Error.map2 join i2 i3
    | Tat (t, _) ->
      get_res (recurse t)
    | TBinOp (MinusPP, t1, t2) ->
      ignore (recurse t1);
      ignore (recurse t2);
      (match Ast_types.unroll (get_cty t1) with
       | { tnode = TArray(_, _) } as ta ->
         begin
           try
             let n = Cil.bitsSizeOf ta in
             (* the second argument must be in the same block than [t].
                Consequently the result of the difference belongs to
                [0; \block_length(t)] *)
             let nb_bytes = if n mod 8 = 0 then n / 8 else n / 8 + 1 in
             ival Z.zero (Z.of_int nb_bytes)
           with Cil.SizeOfError _ ->
             Lazy.force interv_of_unknown_block
         end
       | { tnode = TPtr _ } -> Lazy.force interv_of_unknown_block
       | _ -> assert false)
    | Tblock_length (_, t)
    | Toffset (_, t) ->
      ignore (recurse t);
      (match Ast_types.unroll (get_cty t) with
       | { tnode = TArray (_, _) } as ta ->
         begin
           try
             let n = Cil.bitsSizeOf ta in
             let nb_bytes = if n mod 8 = 0 then n / 8 else n / 8 + 1 in
             singleton_of_int nb_bytes
           with Cil.SizeOfError _ ->
             Lazy.force interv_of_unknown_block
         end
       | { tnode = TPtr _ } -> Lazy.force interv_of_unknown_block
       | _ -> assert false)
    | Tnull  -> singleton_of_int 0
    | Tapp (li,_,args) ->
      (match li.l_body with
       | LBpred _ | LBterm _ ->
         let call_profile =
           Profile.make
             li.l_profile
             (List.map (fun arg -> get_res @@ recurse arg) args)
         in
         if LF_env.is_rec li then
           try
             let known_profile, ival = LF_env.find_profile_ival li in
             if Profile.is_included call_profile known_profile
             then
               (replace_args_ival ~logic_env li args known_profile;
                ival)
             else
               let
                 ext_profile =
                 Widening.widen_profile li known_profile call_profile
               in
               initiate_fixpoint ~logic_env li ext_profile args
           with Not_found ->
             let known_profile = Profile.make
                 li.l_profile
                 (List.init (List.length li.l_profile) (fun _ -> bottom))
             in
             let
               ext_profile =
               Widening.widen_profile li known_profile call_profile
             in
             initiate_fixpoint ~logic_env li ext_profile args
         else
           let logic_env = Logic_env.make call_profile in
           (match li.l_body with
            | LBpred p ->
              ignore (infer_predicate ~logic_env p);
              Ival Ival.zero_or_one
            | LBterm t' ->
              (* If the logic function returns a C type, then its application
                 ranges inside this C type *)
              (match li.l_type with
               | Some (Ctype typ) ->
                 ignore (recurse ~logic_env t');
                 interv_of_typ typ;
               | None | Some _ ->
                 get_res (recurse ~logic_env t'))
            | _ -> assert false)
       | LBnone when li.l_var_info.lv_name = "\\sum" ||
                     li.l_var_info.lv_name = "\\product" ->
         (match args with
          | [ t1; t2; { term_node = Tlambda([ k ], _) } as lambda ] ->
            let t1_iv = recurse t1 in
            let t2_iv = recurse t2 in
            let k_iv = Error.map2 join t1_iv t2_iv in
            let logic_env_with_k =
              Logic_env.add logic_env k k_iv
            in
            let lambda_iv = recurse ~logic_env:logic_env_with_k lambda in
            Error.map3 (infer_sum_product li.l_var_info) lambda_iv t1_iv t2_iv
          | _ -> Error.not_yet "extended quantifiers without lambda term")
       | LBnone
       | LBreads _ ->
         List.iter (fun arg -> ignore @@ recurse arg) args;
         (match li.l_type with
          | None -> assert false
          | Some ret_type -> interv_of_logic_typ ret_type)
       | LBinductive _ ->
         Error.not_yet "logic functions inductively defined")

    | Tunion _ -> Error.not_yet "tset union"
    | Tinter _ -> Error.not_yet "tset intersection"
    | Tcomprehension (_,_,_) -> Error.not_yet "tset comprehension"
    | Trange (Some n1, Some n2) ->
      let i1 = recurse n1 in
      let i2 = recurse n2 in
      Error.map2 join i1 i2
    | Trange(None, _) | Trange(_, None) ->
      Options.abort ~current:true "unbounded ranges are not part of E-ACSl"

    | Tlet (li,t) ->
      let li_t = Misc.term_of_li li in
      let li_v = li.l_var_info in
      let i1 = recurse li_t in
      let logic_env =
        Error.map (Logic_env.add logic_env li_v) i1
      in
      get_res (recurse ~logic_env t)
    | TConst (LReal lr) ->
      if lr.r_lower = lr.r_upper then Float(FDouble, Some lr.r_nearest)
      else Rational
    | Tlambda ([ _ ],lt) ->
      get_res (recurse lt)

    | TStartOf lv
    | TAddrOf lv ->
      ignore (infer_term_lval ~force ~logic_env lv);
      Nan

    | TBinOp (PlusPI, t1 ,t2)
    | TBinOp (MinusPI, t1, t2) ->
      ignore (recurse t1);
      ignore (recurse t2);
      Nan

    | Tbase_addr (_,t)
    | Ttypeof t ->
      ignore (recurse t);
      Nan

    | TDataCons (_,l) ->
      List.iter (fun t -> ignore (recurse t)) l;
      Nan

    | TUpdate (t1, toff, t2) ->
      ignore (recurse t1);
      ignore (recurse t2);
      infer_term_offset ~force ~logic_env toff;
      Nan

    | Tlambda (_,_)
    | TConst (LStr _ | LWStr _)
    | Ttype _
    | Tempty_set ->
      Nan
  in
  Memo.memo
    ~force_infer:force
    (Logic_env.get_profile logic_env)
    (fun t ->
       let res = compute t in
       (* inferring interval for each guards associated to [t] *)
       Rte_analysis.iter_on_guards t (infer_predicate ~logic_env:logic_env);
       res
    )
    t

and initiate_fixpoint ~logic_env li args_ival args =
  let logic_env_call = Logic_env.make args_ival in
  let ival, pot = match li.l_body with
    | LBpred p -> Ival (Ival.zero_or_one), PoT_pred p
    | LBterm t ->
      (match li.l_type with
       | Some (Ctype typ) ->
         interv_of_typ typ, PoT_term t
       | None | Some _ ->
         Ival (Ival.bottom), PoT_term t)
    | _ -> assert false
  in
  LF_env.add ~logic_env li args_ival ival args;
  replace_all_args_ival li args_ival;
  let res = fixpoint logic_env_call li pot in
  LF_env.decrease li;
  res

and fixpoint logic_env li pot =
  let get_res = Error.map (fun x -> x) in
  let ival = LF_env.find_ival li in
  let inferred_ival =
    match pot with
    | PoT_term t -> get_res (infer ~force:true ~logic_env t)
    | PoT_pred p -> infer_predicate ~logic_env p; Ival Ival.zero_or_one
  in
  if is_included inferred_ival ival
  then
    ival
  else
    (LF_env.update_ival li (Widening.widen li ival inferred_ival);
     let ival = fixpoint logic_env li pot
     in LF_env.decrease li; ival)

and infer_term_lval ~force ~logic_env (host, offset as tlv) =
  match offset with
  | TNoOffset -> infer_term_host ~force ~logic_env host
  | _ ->
    ignore (infer_term_host ~force ~logic_env host);
    infer_term_offset ~force ~logic_env offset;
    let ty = Logic_utils.logicCType (Cil.typeOfTermLval tlv) in
    interv_of_typ ty

and infer_term_host ~force ~logic_env thost =
  match thost with
  | TVar v ->
    (try Logic_env.find logic_env v with Not_found ->
     match v.lv_type with
     | Lboolean -> ival Z.zero Z.one
     | Linteger -> top_ival
     | Ctype { tnode = TFloat fk } -> Float(fk, None)
     | Lreal -> Real
     | Ctype _ -> interv_of_typ (Logic_utils.logicCType v.lv_type)
     | Ltype _ | Lvar _ | Larrow _ ->
       Options.fatal "unexpected logic type")
  | TResult ty ->
    interv_of_typ ty
  | TMem t ->
    ignore (infer ~force ~logic_env t);
    let ty = Logic_utils.logicCType t.term_type in
    match Ast_types.unroll_node ty with
    | TPtr ty | TArray (ty, _) ->
      interv_of_typ ty
    | _ ->
      Options.fatal "unexpected type %a for term %a"
        Printer.pp_typ ty
        Printer.pp_term t

and infer_term_offset ~force ~logic_env t =
  match t with
  | TNoOffset -> ()
  | TField(_, toff)
  | TModel(_, toff) -> infer_term_offset ~force ~logic_env toff
  | TIndex(t, toff) ->
    ignore (infer ~force ~logic_env t);
    infer_term_offset ~force ~logic_env toff

(* Update the interval of variables when they appear in a comparison of the form
   [x op t] or [t op x] *)
and compute_logic_env_if_branches logic_env p =
  let get_res = Error.map (fun x -> x) in
  let ival v = infer ~force:false ~logic_env v in
  let add_ub logic_env x v =
    let max = Option.bind Ival.max_int (Error.map extract_ival @@ ival v) in
    Logic_env.refine logic_env x (Ival (Ival.inject_range None max))
  in
  let add_lb logic_env x v =
    let min = Option.bind Ival.min_int (Error.map extract_ival @@ ival v) in
    Logic_env.refine logic_env x (Ival (Ival.inject_range min None))
  in
  let add_eq logic_env x v = Logic_env.refine logic_env x (get_res (ival v)) in
  let t_branch, f_branch =
    (* we do not discriminate between strict and weak inequalities. This is
       slightly less precise but allow for better reusing of the code in the
       case of recursive functions, the main advantage in typing
       conditionals is for recursive functions. *)
    match p.pred_content with
    | Prel (op, {term_node = TLval(TVar x, TNoOffset)}, v) ->
      begin
        match op with
        | Rlt | Rle ->
          add_ub logic_env x v,
          add_lb logic_env x v
        | Rgt | Rge ->
          add_lb logic_env x v,
          add_ub logic_env x v
        | Req ->
          add_eq logic_env x v,
          logic_env
        | Rneq ->
          logic_env,
          add_eq logic_env x v
      end
    | _ -> logic_env, logic_env
  in
  match p.pred_content with
  | Prel (op, u, {term_node = TLval(TVar y, TNoOffset)}) ->
    begin
      match op with
      | Rlt | Rle ->
        add_lb t_branch y u,
        add_ub f_branch y u
      | Rgt | Rge ->
        add_ub t_branch y u,
        add_lb f_branch y u
      | Req ->
        add_eq t_branch y u,
        logic_env
      | Rneq ->
        logic_env,
        add_eq f_branch y u
    end
  | _ -> t_branch, f_branch

(* [infer_bound_variables] infers an interval associated with each of
   the provided bounds of a quantified variable, and provides a term
   accordingly. It could happen that the bounds provided for a quantifier
   [lv] are bigger than its type. [type_bound_variables] handles such cases
   and provides smaller bounds whenever possible.
   Let B be the inferred interval and R the range of [lv.typ]
   - Case 1: B \subseteq R
     Example: [\forall unsigned char c; 4 <= c <= 100 ==> 0 <= c <= 255]
     Return: B
   - Case 2: B \not\subseteq R and the bounds of B are inferred exactly
     Example: [\forall unsigned char c; 4 <= c <= 300 ==> 0 <= c <= 255]
     Return: B \intersect R
   - Case 3: B \not\subseteq R and the bounds of B are NOT inferred exactly
     Example: [\let m = n > 0 ? 4 : 341; \forall char u; 1 < u < m ==> u > 0]
     Return: R with a guard guaranteeing that [lv] does not overflow *)
and infer_bound_variable ~loc ~logic_env (t1, lv, t2) =
  let get_res = Error.map (fun x -> x) in
  let i1 = get_res (infer ~force:false ~logic_env t1) in
  let i2 = get_res (infer ~force:false ~logic_env t2) in
  let i = unify (join i1 i2) in
  let t1, t2, i =
    match lv.lv_type with
    | Ltype _ | Lvar _ | Lreal | Lboolean | Larrow _ ->
      Error.not_yet "quantification over non-integer type"
    | Linteger -> t1, t2, i
    | Ctype ty ->
      let ity = extended_interv_of_typ ty in
      if is_included i ity then
        (* case 1 *)
        t1, t2, i
      else if is_singleton_int i1 &&
              is_singleton_int i2 then
        begin
          (* case 2 *)
          let i = meet i ity in
          (* We can now update the bounds in the preprocessed form
             that come from the meet of the two intervals *)
          match extract_ival i with
          | None -> t1, t2, i
          | Some ival ->
            let min, max = Misc.finite_min_and_max ival in
            let t1 = Logic_const.tint ~loc min in
            let t2 = Logic_const.tint ~loc max in
            t1, t2, i
        end
      else
        (* case 3 *)
        match extract_ival ity with
        | None -> t1, t2, i
        | Some ival ->
          let min, max = Misc.finite_min_and_max ival in
          let guard_lower = Logic_const.tint ~loc min in
          let guard_upper = Logic_const.tint ~loc max in
          let lv_term = Logic_const.tvar ~loc lv in
          let guard_lower = Logic_const.prel ~loc (Rle, guard_lower, lv_term) in
          let guard_upper = Logic_const.prel ~loc (Rlt, lv_term, guard_upper) in
          let guard = Logic_const.pand ~loc (guard_lower, guard_upper) in
          ignore (infer_predicate ~logic_env guard);
          Bound_variables.add_guard_for_small_type lv guard;
          t1, t2, i
  in
  ignore (infer ~force:false ~logic_env t1);
  ignore (infer ~force:false ~logic_env t2);
  Logic_env.add logic_env lv i, (t1, lv, t2)

and infer_predicate ~logic_env p =
  let get_res = Error.map (fun x -> x) in
  let p = Logic_normalizer.get_pred p in
  match p.pred_content with
  | Pfalse | Ptrue -> ()
  | Papp(li, _, args) ->
    let profile =
      Profile.make
        li.l_profile
        (List.map
           (fun arg -> get_res (infer ~force:false ~logic_env arg))
           args)
    in
    (match li.l_body with
     | LBpred _ when LF_env.is_rec li ->
       (try
          let known_profile = LF_env.find_profile li in
          if Profile.is_included profile known_profile
          then
            replace_args_ival ~logic_env li args known_profile
          else
            let ext_profile = Widening.widen_profile li known_profile profile in
            ignore (initiate_fixpoint ~logic_env li ext_profile args)
        with Not_found -> ignore (initiate_fixpoint ~logic_env li profile args))
     | LBpred p ->
       let logic_env = Logic_env.make profile in
       ignore (infer_predicate ~logic_env p)
     | LBnone -> ()
     | LBreads _ -> ()
     | LBinductive _ -> ()
     | LBterm _ ->
       Options.fatal "unexpected logic definition"
         Printer.pp_predicate p
    )
  | Pdangling _ -> ()
  | Prel(_, t1, t2) ->
    ignore (infer ~force:false ~logic_env t1);
    ignore (infer ~force:false ~logic_env t2)
  | Pand(p1, p2)
  | Por(p1, p2)
  | Pxor(p1, p2)
  | Pimplies(p1, p2)
  | Piff(p1, p2) ->
    infer_predicate ~logic_env p1;
    infer_predicate ~logic_env p2
  | Pnot p ->
    infer_predicate ~logic_env p;
  | Pif(c, p1, p2) ->
    infer_predicate ~logic_env c;
    let logic_env_tbranch, logic_env_fbranch =
      compute_logic_env_if_branches logic_env c
    in
    infer_predicate ~logic_env:logic_env_tbranch p1;
    infer_predicate ~logic_env:logic_env_fbranch p2
  | Plet(li, p) ->
    let li_t = Misc.term_of_li li in
    let li_v = li.l_var_info in
    let i = infer ~force:false ~logic_env li_t in
    let logic_env =
      Error.map (Logic_env.add logic_env li_v) i
    in
    infer_predicate ~logic_env p
  | Pforall _
  | Pexists _ ->
    let guards, goal = Bound_variables.get_preprocessed_quantifier p in
    let loc = p.pred_loc in
    let rec do_analysis guards new_guards logic_env =
      match guards with
      | [] -> logic_env, new_guards
      | guard :: guards ->
        let  logic_env, new_guard =
          infer_bound_variable ~loc ~logic_env guard
        in
        do_analysis guards (new_guard :: new_guards) logic_env
    in
    let logic_env, new_guards = do_analysis guards [] logic_env in
    Bound_variables.replace p new_guards goal;
    infer_predicate ~logic_env goal
  | Pseparated tlist ->
    List.iter
      (fun t -> ignore (infer ~force:false ~logic_env t))
      tlist;
  | Pinitialized(_, t)
  | Pfreeable(_, t)
  | Pallocable(_, t)
  | Pvalid(_, t)
  | Pvalid_read(_, t)
  | Pobject_pointer(_,t)
  | Pvalid_function t ->
    ignore (infer ~force:false ~logic_env t);
  | Pat(p, _) -> infer_predicate ~logic_env p
  | Paligned (t, n) ->
    ignore (infer ~force:false ~logic_env t);
    ignore (infer ~force:false ~logic_env n);
  | Pfresh _ -> Error.not_yet "\\fresh"

include Ival_datatype

let typer_visitor ~logic_env = object
  inherit E_acsl_visitor.visitor dkey

  (* global logic functions and predicates are evaluated are callsites *)
  method !glob_annot _ = Cil.SkipChildren

  method !vpredicate p =
    (* Do not raise a warning for e-acsl errors at preprocessing time,
       those errors are stored in the table and warnings are raised at
       translation time *)
    (try infer_predicate ~logic_env p
     with Error.Not_yet _ | Error.Typing_error _  -> ());
    Cil.SkipChildren
end

let infer_program ast =
  let visitor = typer_visitor ~logic_env:Logic_env.empty in
  visitor#visit_file ast

let preprocess_predicate ~logic_env p =
  let visitor = typer_visitor ~logic_env in
  ignore @@ visitor#visit_predicate p

let preprocess_code_annot ~logic_env annot =
  let visitor = typer_visitor ~logic_env in
  ignore @@ visitor#visit_code_annot annot

let preprocess_term ~logic_env t =
  ignore (infer ~force:false ~logic_env t)

let get_from_profile ~profile t =
  let t = Logic_normalizer.get_term t in
  Error.retrieve_preprocessing
    "Interval inference"
    (Memo.get profile)
    t
    Printer.pp_term

let get ~logic_env =
  get_from_profile ~profile:(Logic_env.get_profile logic_env)

let joins ~logic_env terms =
  List.fold_right (fun t acc -> join (get ~logic_env t) acc) terms bottom

let joins_from_profile ~profile terms =
  List.fold_right (fun t acc -> join (get_from_profile ~profile t) acc) terms bottom

let join_plus_one ~profile t1 t2 =
  let plus_one i = lift_arith_binop Ival.add_int i (singleton Z.one)
  in
  join (plus_one (get_from_profile ~profile t1)) (get_from_profile ~profile t2)

let get_ival ~logic_env t =
  extract_ival (get ~logic_env t)

let clear () =
  Memo.clear();
  LF_env.clear()
