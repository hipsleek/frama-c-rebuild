(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval
open Eva_ast

(* If [true], checks invariants of the states created by most functions. *)
let debug = false

(* Whether the domain infers non-relational intervals (ivals) to improve the
   precision of the join operation: this avoids losing all relations that have
   been inferred in only one side of the join. Enhances the domain accuracy
   for a minimal drop in efficiency. *)
let infer_intervals = true

(* Whether the domain saturates the octagons: from a relation between (x, y)
   and a relation between (y, z), infers the relation between (x, z).
   The saturation is currently partial. Improves the domain accuracy for a
   minimal drop in efficiency. *)
let saturate_octagons = true

(* Is the domain intraprocedural, according to the -eva-octagon-through-calls
   option. In this case, the analysis of each function starts with an empty
   state, and the relations inferred in a function are not propagated back to
   the caller either. *)
let intraprocedural () = not (Parameters.OctagonCall.get ())

module Hptmap_Info = struct
  let initial_values = []
  let dependencies = [ Ast.self ]
end

(* -------------------------------------------------------------------------- *)
(*                  Basic types: pair of variables and Ival.t                 *)
(* -------------------------------------------------------------------------- *)

type kind = Integer | Float

let typ_kind typ =
  match Ast_types.unroll_node typ with
  | TInt _ | TEnum  _ | TPtr _ -> Integer
  | TFloat _ -> Float
  | _ -> assert false

type dependencies = Deps.t = {
  data: Memory_zone.t;
  indirect: Memory_zone.t;
}

type evaluator = exp -> Cvalue.V.t or_top

(* Abstract interface for the variables used by the octagons. *)
module type Variable = sig
  include Datatype.S_with_collections
  (* Creates a variable from a lvalue. *)
  val make_lval: lval -> t
  val make_int: Cil_types.varinfo -> t
  val make_startof: Cil_types.varinfo -> t
  val kind: t -> kind (* The kind of the variable: integer or float. *)
  val lval: t -> lval option (* The CIL lval corresponding to the variable. *)
  val id: t -> int (* Unique id, needed to use variables as hptmap keys. *)
  val deps: eval_loc:(lval -> Precise_locs.precise_location) ->
    t -> dependencies
end

(* Variables of the octagons. Should be extended later to also include
   symbolic lvalues. *)
module Variable : Variable = struct
  module HCE = Hcexprs.HCE

  type var =
    | Var of Cil_types.varinfo
    | Int of Cil_types.varinfo
    | StartOf of Cil_types.varinfo
    | Lval of HCE.t

  type tt = var * int

  (* The use of [HCE.id] makes this id non-deterministic. This can have an
     impact when printing the domain state, as the order between variables
     depends on this id. Option -deterministic circumvents this issue. *)
  let make_id = function
    | Var vi -> 4 * vi.vid
    | Int vi -> 4 * vi.vid + 1
    | StartOf vi -> 4 * vi.vid + 2
    | Lval lval -> 4 * HCE.id lval + 3

  let id (_var, id) = id

  module Datatype_Input = struct
    include Datatype.Undefined
    type t = tt
    let name = "Eva.Octagons.Variable"
    let structural_descr = Structural_descr.t_abstract
    let reprs = [ Var (List.hd Cil_datatype.Varinfo.reprs), 0 ]

    let compare x y = id y - id x
    let equal = Datatype.from_compare
    let hash = id
    let rehash = Datatype.identity

    let pretty fmt (var, _id) =
      match var with
      | Var vi | StartOf vi -> Printer.pp_varinfo fmt vi
      | Int vi -> Format.fprintf fmt "(integer)%a" Printer.pp_varinfo vi
      | Lval lval -> HCE.pretty fmt lval

  end

  include Datatype.Make_with_collections (Datatype_Input)

  let make var = var, make_id var

  let make_lval (lval : lval) =
    match lval.node with
    | Var vi, NoOffset -> make (Var vi)
    | _ -> make (Lval (HCE.of_lval lval))

  let make_int vi = make (Int vi)
  let make_startof vi = make (StartOf vi)

  let kind (var, _) =
    match var with
    | Var vi -> typ_kind vi.vtype
    | Int _ | StartOf _ -> Integer
    | Lval lval -> typ_kind (HCE.type_of lval)

  let lval (var, _) =
    match var with
    | Var vi -> Some (Eva_ast.Build.var vi)
    | Lval lval -> HCE.to_lval lval
    | Int _ | StartOf _ -> None

  let deps ~eval_loc (var, _) =
    match var with
    | Var vi | Int vi ->
      {
        data = Locations.zone_of_varinfo vi;
        indirect = Memory_zone.bottom;
      }
    | StartOf _ ->
      { data = Memory_zone.bottom ; indirect = Memory_zone.bottom }
    | Lval lval ->
      Eva_ast.PreciseDepsOf.deps_of_lval
        eval_loc Read (Option.get (HCE.to_lval lval))
end

module VarSet = Hptset.Make (Variable) (Hptmap_Info)

(* Pairs of related variables in an octagon.
   This module imposes an order between the two variables X and Y in a pair
   to avoid creating octagons about X±Y *and* about Y±X. *)
module Pair = struct
  module D = Datatype.Pair (Variable) (Variable)
  module Info = struct
    let name = "Octagons.Pair"
    let dependencies = [ Ast.self ]
    let initial_values = []
  end

  include State_builder.Hashcons (D) (Info)

  (* Creates a pair, and also returns a boolean that is [true] if x, y are
     swapped in the pair. *)
  let make x y =
    let x_id = Variable.id x
    and y_id = Variable.id y in
    assert (x_id <> y_id);
    if debug then assert (Variable.kind x = Variable.kind y);
    let pair, swap = if x_id < y_id then (x, y), false else (y, x), true in
    hashcons pair, swap

  let fst t = fst (get t)
  let kind t = Variable.kind (fst t)
  let variable_list t =
    let (v1,v2) = get t in [v1 ; v2]
end


(* Kind of relation between two variables X and Y: X+Y or X-Y. *)
type operation = Add | Sub

let operation_of_binop = function
  | PlusA | PlusPI -> Add
  | MinusA | MinusPI | MinusPP -> Sub
  | _ -> assert false

(* Extended arithmetic operations over Ival.t. *)
module Arith = struct
  open Ival

  let is_top ival = Ival.(equal top ival || equal top_float ival)

  let narrow x y =
    let r = narrow x y in
    if is_bottom r then `Bottom else `Value r

  let widen = Ival.widen

  (* TODO: do not use Ival.top on floating-point value? *)
  let project_float ival =
    if Ival.(equal top ival) then Fval.top
    else project_float ival

  let neg ival =
    if Ival.is_int ival
    then neg_int ival
    else inject_float (Fval.neg (project_float ival))

  let int_or_float_operation i_op f_op = function
    | Integer -> i_op
    | Float ->
      fun i1 i2 ->
        inject_float (f_op Fval.Real (project_float i1) (project_float i2))

  let sub = int_or_float_operation Ival.sub_int Fval.sub
  let add = int_or_float_operation Ival.add_int Fval.add
  let mul_integer = Ival.scale

  let apply = function
    | Add -> add
    | Sub -> sub

  (* Creates the ival covering the integer range [range]. *)
  let make_range range =
    let min = Eval_typ.range_lower_bound range in
    let max = Eval_typ.range_upper_bound range in
    Ival.inject_range (Some min) (Some max)
end

(* -------------------------------------------------------------------------- *)
(*              Rewriting Cil expressions into mathematical octagons          *)
(* -------------------------------------------------------------------------- *)

(* An octagonal relation between two variables : b ≤ X±Y ≤ e *)
type octagon =
  { variables: Pair.t;      (* The two related variables X and Y. *)
    operation: operation;   (* Whether the relation is about X+Y or X-Y. *)
    value: Ival.t;          (* The interval of X±Y. *)
  }

let _pretty_octagon fmt octagon =
  let x, y = Pair.get octagon.variables in
  let op = match octagon.operation with Add -> "+" | Sub -> "-" in
  Format.fprintf fmt "%a %s %a %t %a"
    Variable.pretty x op Variable.pretty y
    Unicode.pp_in_set Ival.pretty octagon.value

(* An environment associates a term v ± c, where v is a variable and
   c an integer limited to an interval, to some well chosen expressions. A
   request to the environment may return a new or an existing variable.
   The abstract domain chooses which expressions are worth being represented
   by a variable; the environment may return [None] otherwise. *)
type environment = exp -> (Variable.t * Ival.t) option

(* Transforms Cil expressions into mathematical octagons.
   Use Ival.t to evaluate expressions. *)
module Rewriting = struct

  let overflow_alarm range =
    if range.Eval_typ.i_signed
    then Kernel.SignedOverflow.get ()
    else Kernel.UnsignedOverflow.get ()

  let downcast_alarm range =
    if range.Eval_typ.i_signed
    then Kernel.SignedDowncast.get ()
    else Kernel.UnsignedDowncast.get ()

  (* Checks if the interval [ival] fits in the C type [typ].
     This is used to ensure that an expression cannot overflow: this module
     uses the mathematical semantics of arithmetic operations, and cannot
     soundly translate overflows in the C semantics.  *)
  let may_overflow ?(cast=false) typ ival =
    let open Eval_typ in
    match classify_as_scalar typ with
    | None -> assert false (* This should not happen here. *)
    | Some (TSFloat _) -> false
    | Some (TSInt range | TSPtr range) ->
      let alarm =
        if cast
        then downcast_alarm range
        else overflow_alarm range
      in
      not (alarm || Ival.is_included ival (Arith.make_range range))

  (* Simplified form [±X-coeff] for expressions,
     where X is a variable and coeff an interval. *)
  type var_coeff = { var: Variable.t; sign: bool; coeff: Ival.t; }

  let _pretty_var_coeff fmt var_coeff =
    Format.fprintf fmt "%s%a - %a"
      (if var_coeff.sign then "" else "-")
      Variable.pretty var_coeff.var
      Ival.pretty var_coeff.coeff

  (* Negates a simplified form. *)
  let neg { var; sign; coeff } =
    { var; sign = not sign; coeff = Arith.neg coeff }

  (* If a needed interval is unknown, stop the current computation and return
     an empty list. *)
  let (let*) x f = match x with
    | `Top -> []
    | `Value x -> f x

  let project_ival x =
    match x with
    | `Top -> `Top
    | `Value x ->
      try
        `Value (Cvalue.V.project_ival x)
      with Cvalue.V.Not_based_on_null -> `Top

  (* Apply [f typ v1 v2] if the operation [e1 op e2] does not overflow,
     where [v1] and [v2] are the intervals for [e1] and [e2], and [typ] is
     the type of [e1]. Returns the empty list otherwise. *)
  let apply_binop f (eval : evaluator) typ e1 op e2 =
    let kind = typ_kind e1.typ in
    let v1 = eval e1 in
    let v2 = eval e2 in
    if Ast_types.is_ptr typ
    then f kind v1 v2
    else
      let* v1' = project_ival v1 in
      let* v2' = project_ival v2 in
      let result = Arith.apply op kind v1' v2' in
      if may_overflow typ result
      then []
      else f kind v1 v2

  (* Rewrites the Cil expression [expr] into the simplified form [±x-coeff],
     where [x] is a non-singleton variable and [coeff] is an interval. The
     result follows the mathematical semantics.
     If such a simplified form cannot be found, the function returns an empty
     list. If multiple variables occur in the expression, the function tries to
     compute a list of equivalent forms [±x-coeff], one for each variable.  The
     function relies on an evaluation function linking each sub-expression into
     an interval, used for computing sound coefficients. The evaluation may
     return Top for some sub-expression, thus preventing the computation. *)
  let rec rewrite (eval : evaluator) (env : environment) expr =
    match env expr with
    | Some (var, ival) -> [ { var; sign = true; coeff = Ival.neg_int ival } ]
    | None ->
      match expr.node with
      | UnOp (Neg, e, typ) ->
        let* v = project_ival (eval e) in
        if may_overflow typ (Arith.neg v)
        then [] else List.map neg (rewrite eval env e)

      | BinOp (PlusA | MinusA | PlusPI | MinusPI as binop, e1, e2, typ) ->
        let op = operation_of_binop binop in
        let rewrite_binop kind v1 v2 =
          let left_linearized =
            let* v2 = project_ival v2 in
            let inverse_op = if op = Add then Arith.sub else Arith.add in
            try
              let v2 =
                if Ast_types.is_ptr typ
                then
                  let scale = Cil.(bytesSizeOf (Ast_types.direct_pointed_type typ)) in
                  Arith.mul_integer (Z.of_int scale) v2
                else v2
              in
              let add_v2 var =
                { var with coeff = inverse_op kind var.coeff v2 }
              in
              List.map add_v2 (rewrite eval env e1)
            with Cil.SizeOfError _ -> []
          and right_linearized =
            let* v1 = project_ival v1 in
            let add_v1 var =
              let var = if op = Sub then neg var else var in
              { var with coeff = Arith.sub kind var.coeff v1 }
            in
            List.map add_v1 (rewrite eval env e2)
          in
          left_linearized @ right_linearized
        in
        apply_binop rewrite_binop eval typ e1 op e2

      | CastE (typ, e) ->
        if Ast_types.(is_integral typ && is_integral e.typ) then
          let* v = project_ival (eval e) in
          if may_overflow ~cast:true typ v then [] else rewrite eval env e
        else if Ast_types.(is_ptr typ && is_ptr e.typ) then
          rewrite eval env e
        else
          []

      | _ -> []

  (* Rewrites the operation [e1 ± e2] into equivalent octagons ±(X±Y-value). *)
  let rewrite_binop (eval : evaluator) (env : environment) e1 binop e2 =
    let kind = typ_kind e1.typ in
    let vars1 = rewrite eval env e1 in
    let vars2 = rewrite eval env e2 in
    let vars2 = if binop = Sub then List.map neg vars2 else vars2 in
    let aux acc var1 var2 =
      if Variable.equal var1.var var2.var
      then acc
      else
        let variables, swap = Pair.make var1.var var2.var in
        let operation = if var1.sign = var2.sign then Add else Sub in
        let sign = match operation with
          | Add -> var1.sign
          | Sub -> if swap then var2.sign else var1.sign
        in
        let value = Arith.add kind var1.coeff var2.coeff in
        let value = if sign then value else Arith.neg value in
        (sign, { variables; operation; value }) :: acc
    in
    List.product_fold aux [] vars1 vars2

  (* Returns the range of the expression X-Y when the comparison X#Y holds. *)
  let comparison_range =
    function
    | Eva_ast.Lt -> Ival.inject_range None (Some Z.minus_one)
    | Gt -> Ival.inject_range (Some Z.one) None
    | Le -> Ival.inject_range None (Some Z.zero)
    | Ge -> Ival.inject_range (Some Z.zero) None
    | Eq -> Ival.zero
    | Ne -> Ival.top
    | _ -> assert false

  (* Transforms the constraint [expr] ∈ [ival] into a list of octagonal
     constraints. *)
  let make_octagons evaluate env (expr : Eva_ast.exp) ival =
    let make_octagons_from_binop kind e1 op e2 ival =
      (* equivalent octagonal forms ±(X±Y-v) for [e1 op e2]. *)
      let rewritings = rewrite_binop evaluate env e1 op e2 in
      (* create the final octagon, knowing that [e1 op e2] ∈ [ival]. *)
      let make_octagon (sign, octagon) =
        let ival = if sign then ival else Arith.neg ival in
        let value = Arith.add kind ival octagon.value in
        { octagon with value }
      in
      List.map make_octagon rewritings
    in
    match expr.node with
    | BinOp (PlusA | MinusA | PlusPI | MinusPI | MinusPP as binop, e1, e2, typ) ->
      let op = operation_of_binop binop in
      let make_octagons typ _ _ = make_octagons_from_binop typ e1 op e2 ival in
      apply_binop make_octagons evaluate typ e1 op e2
    | BinOp ((Lt | Gt | Le | Ge | Eq | Ne as binop), e1, e2, _typ) ->
      if not (Ast_types.is_integral_or_pointer e1.typ)
      || (Ival.contains_zero ival && Ival.contains_non_zero ival)
      then []
      else
        let comp =
          if Ival.is_zero ival
          then Eva_ast.invert_relation binop
          else binop
        in
        let range = comparison_range comp in
        make_octagons_from_binop (typ_kind e1.typ) e1 Sub e2 range
    | _ -> []

  let overflow_alarms typ expr ival =
    match Eval_typ.classify_as_scalar typ with
    | Some (Eval_typ.TSInt range) ->
      let signed = range.Eval_typ.i_signed in
      let overflow = if signed then Alarms.Signed else Alarms.Unsigned in
      let max_bound = Eval_typ.range_upper_bound range in
      let min_bound = Eval_typ.range_lower_bound range in
      let ival_range = Ival.inject_range (Some min_bound) (Some max_bound) in
      let aux has_better_bound bound bound_kind alarms =
        if Ival.is_bottom ival || has_better_bound ival ival_range >= 0
        then
          let cil_expr = Eva_ast.to_cil_exp expr in
          let alarm = Alarms.Overflow (overflow, cil_expr, bound, bound_kind) in
          Alarmset.set alarm Alarmset.True alarms
        else alarms
      in
      let alarms = Alarmset.all in
      let alarms =
        aux Ival.has_greater_min_bound min_bound Alarms.Lower_bound alarms
      in
      aux Ival.has_smaller_max_bound max_bound Alarms.Upper_bound alarms
    | _ -> Alarmset.all

  (* Evaluates the Cil expression [expr], by rewriting it into octagonal
     constraints using [evaluate_expr] to evaluate sub-expressions, and
     then using [evaluate_octagon] to evaluate the octagons. *)
  let evaluate_through_octagons (eval : evaluator) evaluate_octagon env expr =
    let evaluate_octagon acc (sign, octagon) =
      match evaluate_octagon octagon with
      | None -> acc
      | Some ival ->
        let ival = if sign then ival else Arith.neg ival in
        Ival.narrow acc ival
    in
    let evaluate_octagons octagons =
      List.fold_left evaluate_octagon Ival.top octagons
    in
    let default = Ival.top, Alarmset.all in
    match expr.node with
    | BinOp ((PlusA | MinusA as binop), e1, e2, typ) ->
      let op = if binop = PlusA then Add else Sub in
      let octagons = rewrite_binop eval env e1 op e2 in
      let ival = evaluate_octagons octagons in
      if Ival.(equal top ival) then default else
        let kind = typ_kind e1.typ in
        let ival2 =
          match
            project_ival (eval e1), project_ival (eval e2)
          with
          | `Value v1, `Value v2 -> Arith.apply op kind v1 v2
          | _, _ -> Ival.top
        in
        let ival = Ival.narrow ival ival2 in
        if may_overflow typ ival
        then default
        else ival, overflow_alarms typ expr ival
    | BinOp ((Lt | Gt | Le | Ge | Eq as binop), e1, e2, _typ)
      when Ast_types.is_integral_or_pointer e1.typ ->
      (* Evaluate [e1 - e2] and compare the resulting interval to the interval
         for which the comparison [e1 # e2] holds. *)
      let range = comparison_range binop in
      let octagons = rewrite_binop eval env e1 Sub e2 in
      let ival = evaluate_octagons octagons in
      if Ival.is_included ival range then Ival.one, Alarmset.all
      else if not (Ival.intersects ival range)
      then Ival.zero, Alarmset.all else default
    | _ -> default

end

(* -------------------------------------------------------------------------- *)
(*           Diamonds and octagons: relations between two variables           *)
(* -------------------------------------------------------------------------- *)

(* This domain infers relations between pairs of variables (X, Y), by inferring
   intervals for the mathematical operations X+Y and X-Y.
   It also infers non-relational intervals for the separate variables X and Y
   (they could be seen as intervals for X+X and Y+Y, but we chose to store them
   in another way). These intervals are used to make the join more precise.
   Geometrically, in a plan, intervals for X and Y shape a straight rectangle,
   while intervals for X+Y and X-Y shape a "leaning" rectangle; the intersection
   of these rectangles shapes an octagon.
   Using a misnomer, we call diamonds the intervals for X+Y and X-Y, and
   octagons the maps from variables to diamonds, even if they do not exactly
   shape octagons. *)

(* Relation between a pair of variables (X, Y).
   [add] is an interval for X+Y, and [sub] is an interval for [X-Y]. *)
type diamond = { add: Ival.t; sub: Ival.t }

module DiamondDatatype = struct
  type t = diamond
  include Datatype.Serializable_undefined

  let name = "Octagons.Diamond"
  let structural_descr =
    Structural_descr.t_record [| Ival.packed_descr; Ival.packed_descr |]
  let reprs = [ { add = Ival.top; sub = Ival.top } ]

  let compare x y =
    let c = Ival.compare x.add y.add in
    if c <> 0 then c else Ival.compare x.sub y.sub

  let equal = Datatype.from_compare

  let hash { add; sub } = Hashtbl.hash (Ival.hash add, Ival.hash sub)

  let pretty fmt { add; sub } =
    Format.fprintf fmt "@[<hov>ADD: @[%a@] ; SUB: @[%a@]@]"
      Ival.pretty add Ival.pretty sub
end

module Diamond = struct
  include Datatype.Make (DiamondDatatype)

  let top = { add = Ival.top; sub = Ival.top }

  let is_top diamond = Arith.is_top diamond.add && Arith.is_top diamond.sub

  let is_included x y =
    Ival.is_included x.add y.add && Ival.is_included x.sub y.sub

  let join x y =
    { add = Ival.join x.add y.add; sub = Ival.join x.sub y.sub }

  let widen x y =
    { add = Arith.widen x.add y.add; sub = Arith.widen x.sub y.sub }

  let narrow x y =
    Arith.narrow x.add y.add >>- fun add ->
    Arith.narrow x.sub y.sub >>-: fun sub -> {add; sub}

  (* If [swap] is true, makes a diamond about (X, Y) from a diamond
     about (Y, X). *)
  let reverse_variables swap t =
    if swap then { t with sub = Arith.neg t.sub } else t
end


(* Maps linking pairs of variables (X, Y) to intervals for X+Y and X-Y. *)
module Octagons = struct

  include Hptmap.Make (Pair) (Diamond) (Hptmap_Info)

  let internal_join = join

  let pretty fmt t =
    let iter f = iter (fun k v -> f (k, v)) in
    let pretty fmt (pair, diamond) =
      let x, y = Pair.get pair in
      let pretty_one op ival =
        if not Ival.(equal top ival)
        then
          Format.fprintf fmt "@[@[%a %s %a@] %t @[%a@]@]@,"
            Variable.pretty x op Variable.pretty y
            Unicode.pp_in_set Ival.pretty ival
      in
      pretty_one "+" diamond.add;
      pretty_one "-" diamond.sub
    in
    Pretty_utils.pp_iter
      ~pre:"@[<v 3>{[ " ~suf:" ]}@]" ~sep:""
      iter pretty fmt t

  let top = empty

  let is_included =
    let cache = Hptmap_sig.PersistentCache "Octagons.Octagons.is_included" in
    let decide_fst _ _ = true in
    let decide_snd _ _ = false in
    let decide_both _ x y = Diamond.is_included x y in
    let decide_fast t1 t2 = decide_fast_inclusion t2 t1 in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  exception EBottom

  let narrow_exc =
    let cache = Hptmap_sig.NoCache in
    let decide _pair x y =
      match Diamond.narrow x y with
      | `Value v -> v
      | `Bottom -> raise EBottom
    in
    join ~cache ~symmetric:true ~idempotent:true ~decide

  let narrow x y = try `Value (narrow_exc x y) with EBottom -> `Bottom

  let decide join = fun _pair x y ->
    let d = join x y in
    if Diamond.is_top d then None else Some d

  let simple_join =
    let cache = Hptmap_sig.PersistentCache "Octagons.Octagons.join" in
    inter ~cache ~symmetric:true ~idempotent:true ~decide:(decide Diamond.join)

  let join ~decide_left ~decide_right =
    let cache = Hptmap_sig.NoCache in
    let decide_left = Traversing decide_left
    and decide_right = Traversing decide_right in
    merge ~cache ~symmetric:false ~idempotent:true
      ~decide_left ~decide_right ~decide_both:(decide Diamond.join)

  let simple_widen =
    let cache = Hptmap_sig.PersistentCache "Octagons.Octagons.widen" in
    inter ~cache ~symmetric:false ~idempotent:true ~decide:(decide Diamond.widen)

  let widen ~decide_left ~decide_right =
    let cache = Hptmap_sig.NoCache in
    let decide_left = Traversing decide_left
    and decide_right = Traversing decide_right in
    merge ~cache ~symmetric:false ~idempotent:true
      ~decide_left ~decide_right ~decide_both:(decide Diamond.widen)

  let unsafe_add = add

  let add variables diamond t =
    try
      Diamond.narrow diamond (find variables t) >>-: fun diamond ->
      add variables diamond t
    with Not_found -> `Value (add variables diamond t)

  let add_octagon { variables; operation; value; } t =
    let diamond =
      try find variables t
      with Not_found -> Diamond.top
    in
    let diamond =
      match operation with
      | Add ->
        Arith.narrow diamond.add value >>-: fun add ->
        { diamond with add }
      | Sub ->
        Arith.narrow diamond.sub value >>-: fun sub ->
        { diamond with sub }
    in
    diamond >>-: fun diamond -> unsafe_add variables diamond t

  let evaluate octagon t =
    try
      let diamond = find octagon.variables t in
      let ival = match octagon.operation with
        | Add -> diamond.add
        | Sub -> diamond.sub
      in
      if Ival.(equal top ival)
      then None
      else
        let kind = Pair.kind octagon.variables in
        let ival = Arith.sub kind ival octagon.value in
        Some ival
    with Not_found -> None
end

(* -------------------------------------------------------------------------- *)
(*                                  Relations                                 *)
(* -------------------------------------------------------------------------- *)

(* Keep track of related variables in an octagon state. *)
module Relations = struct

  include Hptmap.Make (Variable) (VarSet) (Hptmap_Info)

  let inter =
    let cache = Hptmap_sig.PersistentCache "Octagons.Relations.inter" in
    let decide _pair x y =
      let r = VarSet.inter x y in
      if VarSet.is_empty r then None else Some r
    in
    inter ~cache ~symmetric:true ~idempotent:true ~decide

  let union =
    let cache = Hptmap_sig.PersistentCache "Octagons.Relations.union" in
    let decide _pair x y = VarSet.union x y in
    join ~cache ~symmetric:true ~idempotent:true ~decide

  (* Marks y as related to x. *)
  let relate_aux x y t =
    let related =
      try find x t
      with Not_found -> VarSet.empty
    in
    let updated = VarSet.add y related in
    add x updated t

  (* Marks x and y as mutually related. *)
  let relate pair t =
    let x, y = Pair.get pair in
    relate_aux y x (relate_aux x y t)

  let add variable set t =
    if VarSet.is_empty set
    then remove variable t
    else add variable set t
end

(* -------------------------------------------------------------------------- *)
(*                           Non-relational intervals                         *)
(* -------------------------------------------------------------------------- *)

module Intervals = struct

  include Hptmap.Make (Variable) (Ival) (Hptmap_Info)

  let internal_join = join

  let top = empty

  let is_included =
    let cache = Hptmap_sig.PersistentCache "Octagons.Intervals.is_included" in
    let decide_fst _ _ = true in
    let decide_snd _ _ = false in
    let decide_both _ x y = Ival.is_included x y in
    let decide_fast t1 t2 = decide_fast_inclusion t2 t1 in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  exception EBottom

  let narrow_exc =
    let cache = Hptmap_sig.NoCache in
    let decide _varinfo x y =
      let ival = Ival.narrow x y in
      if Ival.is_bottom ival then raise EBottom else ival
    in
    join ~cache ~symmetric:true ~idempotent:true ~decide

  let narrow x y = try `Value (narrow_exc x y) with EBottom -> `Bottom

  let join =
    let cache = Hptmap_sig.PersistentCache "Octagons.Intervals.join" in
    let decide _varinfo x y =
      let r = Ival.join x y in
      if Ival.(equal top r) then None else Some r
    in
    inter ~cache ~symmetric:true ~idempotent:true ~decide

  let widen =
    let cache = Hptmap_sig.PersistentCache "Octagons.Intervals.widen" in
    let decide _varinfo x y =
      let r = Arith.widen x y in
      if Ival.(equal top r) then None else Some r
    in
    inter ~cache ~symmetric:false ~idempotent:true ~decide
end


(* -------------------------------------------------------------------------- *)
(*                               Dependencies                                 *)
(* -------------------------------------------------------------------------- *)

module VariableToDeps =
struct
  let cache_prefix = "Eva.Octagons.VariableToDeps"

  include Hptmap.Make (Variable) (Deps) (Hptmap_Info)

  let is_included: t -> t -> bool =
    let cache_name = cache_prefix ^ ".is_included" in
    let decide_fst _b _v1 = true in
    let decide_snd _b _v2 = false in
    let decide_both _ v1 v2 = Deps.is_included v1 v2 in
    let decide_fast s t = if s == t then PTrue else PUnknown in
    binary_predicate
      (Hptmap_sig.PersistentCache cache_name) UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let narrow: t -> t -> t =
    let cache_name = cache_prefix ^ ".narrow" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = Deps.narrow v1 v2 in
    join ~cache ~symmetric ~idempotent ~decide

  let join: t -> t -> t =
    let cache_name = cache_prefix ^ ".join" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = Some (Deps.join v1 v2) in
    inter ~cache ~symmetric ~idempotent ~decide

  let find_opt (v: Variable.t) (m: t) =
    try Some (find v m) with Not_found -> None

  (* Return the subtrees of the left map whose keys are not in the right map. *)
  let only_in_left =
    let cache_name = cache_prefix ^ ".only_in_left" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = false in
    let idempotent = false in
    let decide_both _ _ _ = None in
    let decide_left = Neutral in
    let decide_right = Absorbing in
    merge ~cache ~symmetric ~idempotent ~decide_both ~decide_left ~decide_right

  let all_but_diff =
    let cache_name = cache_prefix ^ ".all_but_diff" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let decide_both _ d1 d2 = if Deps.equal d1 d2 then Some d1 else None in
    merge ~cache ~symmetric:true ~idempotent:true
      ~decide_left:Neutral ~decide_right:Neutral ~decide_both
end

module BaseToVariables = struct
  module VSet = VarSet

  (* [BaseToVariables] represents a map from bases to each symbolic variable
     used in the domain state that depends on this base.
     These variables are split into two (non necessary disjoint) sets:
     - direct dependency: variables whose value depends on the base;
     - indirect dependency: variables whose location depends on the base. *)
  module VSetPair =
  struct
    include Datatype.Pair
        (VSet) (* Directly dependent variables *)
        (VSet) (* Indirectly dependent variables *)
    let inter (s1,t1) (s2,t2) = VSet.inter s1 s2, VSet.inter t1 t2
    let union (s1,t1) (s2,t2) = VSet.union s1 s2, VSet.union t1 t2
    let is_empty (s, t) = VSet.is_empty s && VSet.is_empty t
  end

  include Hptmap.Make (Base.Base) (VSetPair) (Hptmap_Info)

  let cache_prefix = "Eva.Octagons.BaseToVariables"

  let _narrow =
    let cache_name = cache_prefix ^ ".narrow" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 =
      let inter = VSetPair.inter v1 v2 in
      if VSetPair.is_empty inter then None else Some inter
    in
    inter ~cache ~symmetric ~idempotent ~decide

  let join =
    let cache_name = cache_prefix ^ ".join" in
    let cache = Hptmap_sig.PersistentCache cache_name in
    let symmetric = true in
    let idempotent = true in
    let decide _ v1 v2 = VSetPair.union v1 v2 in
    join ~cache ~symmetric ~idempotent ~decide

  let find b m =
    try find b m
    with Not_found -> VSet.empty, VSet.empty

  let add_direct v =
    replace (function
        | None -> Some (VSet.singleton v, VSet.empty)
        | Some (direct, indirect) -> Some (VSet.add v direct, indirect))

  let add_indirect v =
    replace (function
        | None -> Some (VSet.empty, VSet.singleton v)
        | Some (direct, indirect) -> Some (direct, VSet.add v indirect))

  let add_deps var deps map =
    Memory_zone.fold_bases (add_direct var) deps.data map |>
    Memory_zone.fold_bases (add_indirect var) deps.indirect

  let remove_direct v =
    replace (function
        | None -> None
        | Some (direct, indirect) ->
          let direct = VSet.remove v direct in
          if VSet.is_empty direct && VSet.is_empty indirect
          then None
          else Some (direct, indirect))

  let remove_indirect v =
    replace (function
        | None -> None
        | Some (direct, indirect) ->
          let indirect = VSet.remove v indirect in
          if VSet.is_empty direct && VSet.is_empty indirect
          then None
          else Some (direct, indirect))

  let remove_deps var deps map =
    Memory_zone.fold_bases (remove_direct var) deps.data map |>
    Memory_zone.fold_bases (remove_indirect var) deps.indirect

  let all_variables m =
    fold (fun _b (dv, iv) acc -> VSet.(union (union dv iv) acc)) m VSet.empty
end

module Deps = struct
  module VSet = VarSet

  include Datatype.Pair (VariableToDeps) (BaseToVariables)

  (* [hash] and [compare] do not need to consider the [BaseToVariables] part as
     it can be fully deduced from VariableToDeps's one *)
  let hash (m, _i: t) =
    VariableToDeps.hash m

  let compare (m1, _i1: t) (m2, _i2: t) =
    VariableToDeps.compare m1 m2

  let empty = VariableToDeps.empty, BaseToVariables.empty

  let intersects_base (_m, i: t) base =
    let data, indirect = BaseToVariables.find base i in
    VSet.union data indirect |> VSet.elements

  let intersects_var (d: t) (vi: Cil_types.varinfo) =
    intersects_base d (Base.of_varinfo vi)

  let intersects_zone (m, i: t) zone =
    let filter ~direct zone v =
      try
        let deps = VariableToDeps.find v m in
        let v_zone = if direct then deps.data else deps.indirect in
        Memory_zone.intersects v_zone zone
      with Not_found -> false
    in
    let get_at_base b intervals (data_acc, indirect_acc) =
      let data, indirect = BaseToVariables.find b i in
      let zone = Memory_zone.inject b intervals in
      VSet.union data_acc (VSet.filter (filter ~direct:true zone) data),
      VSet.union indirect_acc (VSet.filter (filter ~direct:false zone) indirect)
    in
    let data, indirect =
      Memory_zone.fold_topset_ok get_at_base zone (VSet.empty, VSet.empty)
    in
    VSet.union data indirect |> VSet.elements

  (* Does [deps] contains at least the bases from [previous_deps]? *)
  let are_bases_increasing previous_deps deps =
    let get_bases = Memory_zone.get_bases in
    let is_increasing previous_zone zone =
      Base.SetLattice.is_included (get_bases previous_zone) (get_bases zone)
    in
    is_increasing previous_deps.data deps.data &&
    is_increasing previous_deps.indirect deps.indirect

  (* Exact but slower [add]: in the resulting maps (m, i), [i] is exactly the
     inverse of the map [m]. *)
  let _exact_add var deps (m, i: t): t =
    let add_to (m, i) =
      VariableToDeps.add var deps m,
      BaseToVariables.add_deps var deps i
    in
    match VariableToDeps.find_opt var m with
    | Some previous_deps when Deps.equal previous_deps deps ->
      m, i
    | Some previous_deps when not (are_bases_increasing previous_deps deps) ->
      (* If [var] already exists in the state and had bigger dependencies (a
         dependency can disappear by reduction), remove the previous deps
         from the inverse map to ensure consistency between both maps. *)
      add_to (m, BaseToVariables.remove_deps var previous_deps i)
    | _ -> add_to (m, i)

  (* Faster but approximating [add]: in the resulting maps (m, i), [i] may
     contain variables that do not appear in [m]. *)
  let add var deps (m, i: t): t =
    match VariableToDeps.find_opt var m with
    | Some d when Deps.equal d deps -> m, i
    | _ ->
      VariableToDeps.add var deps m,
      BaseToVariables.add_deps var deps i

  let remove (var: Variable.t) ((m, i): t): t option =
    match VariableToDeps.find_opt var m with
    | None -> None (* The variable was not registered *)
    | Some deps ->
      Some (VariableToDeps.remove var m,
            BaseToVariables.remove_deps var deps i)

  let filter (bases: Base.Hptset.t) (m, i : t): VSet.t * t =
    let i_inter, i_diff = BaseToVariables.partition_with_shape bases i in
    let removed_vars = BaseToVariables.all_variables i_diff in
    let m = VariableToDeps.diff_with_shape removed_vars m in
    let filter (s1, s2) =
      VSet.diff s1 removed_vars, VSet.diff s2 removed_vars
    in
    let i = BaseToVariables.map filter i_inter in
    removed_vars, (m, i)

  let get_var_bases (m, _: t) (var: Variable.t) =
    try
      let deps = VariableToDeps.find var m in
      let zone = Memory_zone.join deps.data deps.indirect in
      Memory_zone.get_bases zone
    with Not_found -> Base.SetLattice.empty

  (* Exact but slower [join]: in the resulting maps (m, i), [i] is exactly the
     inverse of the map [m]. *)
  let _exact_join (m1, i1: t) (m2, i2: t) =
    let m1_only = VariableToDeps.only_in_left m1 m2
    and m2_only = VariableToDeps.only_in_left m2 m1 in
    let i1 = VariableToDeps.fold BaseToVariables.remove_deps m1_only i1
    and i2 = VariableToDeps.fold BaseToVariables.remove_deps m2_only i2 in
    VariableToDeps.join m1 m2, BaseToVariables.join i1 i2

  (* Exact but slower [narrow]: in the resulting maps (m, i), [i] is exactly the
     inverse of the map [m]. *)
  let _exact_narrow (m1, i1: t) (m2, i2: t) =
    let all_but_diff = VariableToDeps.all_but_diff m1 m2 in
    let m1 = VariableToDeps.only_in_left m1 all_but_diff
    and m2 = VariableToDeps.only_in_left m2 all_but_diff in
    let m_narrow = VariableToDeps.narrow m1 m2
    and m_join = VariableToDeps.join m1 m2 in
    let m = VariableToDeps.narrow m_narrow all_but_diff in
    let i = BaseToVariables.join i1 i2 in
    let i = VariableToDeps.fold BaseToVariables.remove_deps m_join i in
    let i = VariableToDeps.fold BaseToVariables.add_deps m_narrow i in
    m, i

  (* Faster but approximating [join]: in the resulting maps (m, i), [i] may
     contain variables that do not appear in [m]. *)
  let join (m1, i1: t) (m2, i2: t) =
    VariableToDeps.join m1 m2, BaseToVariables.join i1 i2

  (* Faster but approximating [narrow]: in the resulting maps (m, i), [i] may
     contain variables that do not appear in [m]. *)
  let narrow (m1, i1: t) (m2, i2: t) =
    VariableToDeps.narrow m1 m2, BaseToVariables.join i1 i2

  let is_included (m1, _: t) (m2, _: t) =
    VariableToDeps.is_included m1 m2

  (* Check inverse dependencies. *)
  let check (abort: ('a, Format.formatter, unit) format -> 'a) (m, i: t) =
    let check_inverse var deps =
      let check_base fst_or_snd base =
        if not (VSet.mem var (fst_or_snd (BaseToVariables.find base i)))
        then
          abort "Missing inverse dependency %a of variable %a@"
            Base.pretty base Variable.pretty var
      in
      let check_zone fst_or_snd zone =
        let bases = Memory_zone.get_bases zone in
        Base.SetLattice.iter (check_base fst_or_snd) bases
      in
      check_zone fst deps.data;
      check_zone snd deps.indirect;
    in
    VariableToDeps.iter check_inverse m
end


(* -------------------------------------------------------------------------- *)
(*                               Octagon states                               *)
(* -------------------------------------------------------------------------- *)

module State = struct

  type state =
    { octagons: Octagons.t;       (* The intervals for X±Y. *)
      intervals: Intervals.t;     (* The intervals for the variables X,Y… *)
      relations: Relations.t;     (* The related variables in [octagons]. *)
      modified: Memory_zone.t; (* The memory zone modified by a function. *)
      deps: Deps.t;
    }

  let name = "octagon"

  include Datatype.Make_with_collections
      (struct
        type t = state
        include Datatype.Serializable_undefined

        let name = "Eva.Octagons.State"
        let structural_descr =
          Structural_descr.t_record
            [| Octagons.packed_descr;
               Intervals.packed_descr;
               Relations.packed_descr;
               Memory_zone.packed_descr;
               Deps.packed_descr |]
        let reprs =
          [ { octagons = Octagons.top;
              intervals = Intervals.empty;
              relations = Relations.empty;
              modified = Memory_zone.bottom;
              deps = Deps.empty; } ]

        let compare s1 s2 =
          let (<?>) c lcmp =
            if c <> 0 then c else Lazy.force lcmp
          in
          Octagons.compare s1.octagons s2.octagons <?>
          lazy (Intervals.compare s1.intervals s2.intervals) <?>
          lazy (Memory_zone.compare s1.modified s2.modified) <?>
          lazy (Deps.compare s1.deps s2.deps)

        let equal = Datatype.from_compare

        let hash t =
          Hashtbl.hash (Octagons.hash t.octagons,
                        Intervals.hash t.intervals,
                        Memory_zone.hash t.modified,
                        Deps.hash t.deps)

        let pretty fmt { octagons } =
          Format.fprintf fmt "@[%a@]" Octagons.pretty octagons
      end)

  let pretty_debug fmt { octagons; intervals; relations; deps } =
    Format.fprintf fmt
      "@[<v> Octagons: %a@; Intervals: %a@; Relations: %a@; Dependencies: %a@.]"
      Octagons.pretty octagons Intervals.pretty intervals
      Relations.pretty relations
      Deps.pretty deps

  (* Verify the internal structure of a state [t], depending on the boolean
     variable [debug]. *)
  let check =
    if not debug
    then fun _ t -> t
    else fun name t ->
      let abort format =
        Format.kasprintf
          (fun s ->
             Self.fatal
               "Incorrect octagon computed by function %s: %s@\n  State: %a"
               name s pretty_debug t)
          format
      in
      (* Checks that an octagon is properly registered in [t.relations]. This is
         mandatory for the soundness of the domain. On the other hand, two
         variables can be related in [t.relations] without an actual octagon
         between them. *)
      let check_relation x y =
        try VarSet.mem x (Relations.find y t.relations)
            && VarSet.mem y (Relations.find x t.relations)
        with Not_found -> false
      in
      let check_relation x y =
        if not (check_relation x y)
        then
          abort "missing relations between %a and %a"
            Variable.pretty x Variable.pretty y
      in
      (* Checks that dependencies of [var] are correctly registered. *)
      let check_deps var =
        if not (VariableToDeps.mem var (fst t.deps))
        then abort "missing dependencies of variable %a" Variable.pretty var
      in
      let check_octagon pair _ =
        let x, y = Pair.get pair in
        check_relation x y; check_deps x; check_deps y
      in
      Octagons.iter check_octagon t.octagons;
      Intervals.iter (fun var _ -> check_deps var) t.intervals;
      (* Check consistency of the dependency maps. *)
      Deps.check abort t.deps;
      t

  (* Is an octagon no more precise than the intervals inferred for the related
     variables? If so, do not save the octagon in the domain. *)
  let is_redundant intervals { variables; operation; value; } =
    if infer_intervals
    then
      try
        let v1, v2 = Pair.get variables in
        let i1 = Intervals.find v1 intervals
        and i2 = Intervals.find v2 intervals in
        let i = Arith.apply operation (Variable.kind v1) i1 i2 in
        Ival.is_included i value
      with Not_found -> false
    else false

  let is_redundant_diamond intervals variables diamond =
    is_redundant intervals {variables; operation = Add; value = diamond.add} &&
    is_redundant intervals {variables; operation = Sub; value = diamond.sub}

  (* Evaluates offset to an interval using the evaluate function for indexes *)
  let rec offset_to_coeff (eval : evaluator) base_type offset =
    let open Lattice_bounds.Top.Operators in
    match offset with
    | NoOffset -> `Value (Ival.zero)
    | Field (fi, sub) ->
      let* sub_coeff = offset_to_coeff eval fi.ftype sub in
      begin try
          let byte_offset = Z.of_int (fst (Cil.fieldBitsOffset fi) / 8) in
          let coeff = Ival.add_singleton_int byte_offset sub_coeff in
          `Value coeff
        with Cil.SizeOfError _ -> `Top
      end
    | Index (exp, sub) ->
      let elem_type = Ast_types.direct_element_type base_type in
      let* cvalue = eval exp in
      let* index =
        try
          `Value (Cvalue.V.project_ival cvalue)
        with Cvalue.V.Not_based_on_null -> `Top
      in
      let* sub_coeff = offset_to_coeff eval elem_type sub in
      let+ elem_size =
        try `Value (Cil.bytesSizeOf elem_type)
        with Cil.SizeOfError _ -> `Top
      in
      Ival.(add_int (scale (Z.of_int elem_size) index) sub_coeff)

  let mk_variable_builder (eval : evaluator) (_: t) =
    let (let*) x f = Option.bind f (Top.to_option x) in
    (* Is the interval computed for a variable a singleton? *)
    let is_singleton v =
      Top.map Cvalue.V.cardinal_zero_or_one v
      |> Top.value ~top:false
    in
    fun exp ->
      match exp.node with
      | Lval lval
        when Ast_types.is_integral_or_pointer lval.typ
          && not (Eva_ast.lval_contains_volatile lval)
          && not (is_singleton (eval exp)) ->
        Some (Variable.make_lval lval, Ival.zero)

      | CastE (typ, { node = Lval { node = Var vi, NoOffset } })
        when Ast_types.is_integral typ
          && Ast_types.is_float vi.vtype
          && not (Ast_types.has_qualifier "volatile" vi.vtype)
          && not (is_singleton (eval exp)) ->
        Some (Variable.make_int vi, Ival.zero)

      | StartOf { node = Var vi, offset } | AddrOf { node = Var vi, offset } ->
        let var = Variable.make_startof vi in
        let* coeff = offset_to_coeff eval vi.vtype offset in
        Some (var, coeff)

      | _ -> None


  (* ------------------------------ Lattice --------------------------------- *)

  let top =
    { octagons = Octagons.top;
      intervals = Intervals.top;
      relations = Relations.empty;
      modified = Memory_zone.top;
      deps = Deps.empty }

  let empty () =
    { octagons = Octagons.top;
      intervals = Intervals.top;
      relations = Relations.empty;
      modified = Memory_zone.bottom;
      deps = Deps.empty }

  let is_included t1 t2 =
    Octagons.is_included t1.octagons t2.octagons
    && Intervals.is_included t1.intervals t2.intervals
    && Memory_zone.is_included t1.modified t2.modified
    && Deps.is_included t1.deps t2.deps

  let join t1 t2 =
    let octagons =
      if not infer_intervals
      then Octagons.simple_join t1.octagons t2.octagons
      else
        let decide_empty intervals pair diamond =
          let v1, v2 = Pair.get pair in
          try
            let i1 = Intervals.find v1 intervals
            and i2 = Intervals.find v2 intervals in
            let kind = Variable.kind v1 in
            let add = Arith.add kind i1 i2
            and sub = Arith.sub kind i1 i2 in
            let diamond = Diamond.join diamond { add; sub } in
            if Diamond.is_top diamond then None else Some diamond
          with Not_found -> None
        in
        let decide_left = decide_empty t2.intervals
        and decide_right = decide_empty t1.intervals in
        Octagons.join ~decide_left ~decide_right t1.octagons t2.octagons
    in
    let relations =
      if infer_intervals
      then Relations.union t1.relations t2.relations
      else Relations.inter t1.relations t2.relations
    in
    let state =
      { octagons; relations;
        intervals = Intervals.join t1.intervals t2.intervals;
        modified = Memory_zone.join t1.modified t2.modified;
        deps = Deps.join t1.deps t2.deps;
      }
    in
    check "join" state

  let widen _kf _hints t1 t2 =
    let octagons =
      if not infer_intervals
      then Octagons.simple_widen t1.octagons t2.octagons
      else
        let decide_empty b intervals pair diamond =
          let v1, v2 = Pair.get pair in
          try
            let i1 = Intervals.find v1 intervals
            and i2 = Intervals.find v2 intervals in
            let kind = Variable.kind v1 in
            let add = Arith.add kind i1 i2
            and sub = Arith.sub kind i1 i2 in
            let diamond =
              if b
              then Diamond.widen { add; sub } diamond
              else Diamond.widen diamond { add; sub }
            in
            if Diamond.is_top diamond then None else Some diamond
          with Not_found -> None
        in
        let decide_left = decide_empty false t2.intervals
        and decide_right = decide_empty true t1.intervals in
        Octagons.widen ~decide_left ~decide_right t1.octagons t2.octagons
    in
    let relations =
      if infer_intervals
      then Relations.union t1.relations t2.relations
      else Relations.inter t1.relations t2.relations
    in
    let state =
      { octagons; relations;
        intervals = Intervals.widen t1.intervals t2.intervals;
        modified = Memory_zone.join t1.modified t2.modified;
        deps = Deps.join t1.deps t2.deps }
    in
    check "widen" state

  let narrow t1 t2 =
    Octagons.narrow t1.octagons t2.octagons >>- fun octagons ->
    Intervals.narrow t1.intervals t2.intervals >>- fun intervals ->
    let relations = Relations.union t1.relations t2.relations in
    let modified = Memory_zone.narrow t1.modified t2.modified in
    let deps = Deps.narrow t1.deps t2.deps in
    `Value { octagons; intervals; relations; modified; deps }

  (* -------------- Transitive closure when adding an octagon --------------- *)

  type relation =
    { vars: Variable.t * Variable.t;
      diamond: diamond; }

  let add_diamond state variables diamond =
    if is_redundant_diamond state.intervals variables diamond
    then `Value state
    else
      Octagons.add variables diamond state.octagons >>-: fun octagons ->
      let relations = Relations.relate variables state.relations in
      { state with octagons; relations }

  let inverse { vars; diamond } =
    let var1, var2 = vars in
    { vars = var2, var1; diamond = Diamond.reverse_variables true diamond }

  let transitive_relation y rel1 rel2 =
    let rel1 =
      if Variable.equal y (snd rel1.vars) then rel1 else inverse rel1
    and rel2 =
      if Variable.equal y (fst rel2.vars) then rel2 else inverse rel2
    in
    (* rel1 is about X±Y, rel2 is about Y±Z. *)
    let kind = Variable.kind y in
    (* X+Z = (X+Y) - (Y-Z) and X+Y = (X-Y) + (Y+Z) *)
    let add =
      Ival.narrow
        (Arith.sub kind rel1.diamond.add rel2.diamond.sub)
        (Arith.add kind rel1.diamond.sub rel2.diamond.add)
    (* X-Z = (X+Y) - (Y+Z) and X-Z = (X-Y) + (Y-Z) *)
    and sub =
      Ival.narrow
        (Arith.sub kind rel1.diamond.add rel2.diamond.add)
        (Arith.add kind rel1.diamond.sub rel2.diamond.sub)
    in
    let diamond = {add; sub} in
    let pair, swap = Pair.make (fst rel1.vars) (snd rel2.vars) in
    let diamond = Diamond.reverse_variables swap diamond in
    pair, diamond

  let saturate state x y rel1 =
    try
      let y_related = Relations.find y state.relations in
      let y_related = VarSet.remove x y_related in
      let aux z state =
        state >>- fun state ->
        try
          let pair, _ = Pair.make y z in
          let diamond = Octagons.find pair state.octagons in
          let vars = Pair.get pair in
          let rel2 = { vars; diamond } in
          let pair, diamond = transitive_relation y rel1 rel2 in
          add_diamond state pair diamond
        with Not_found -> `Value state
      in
      VarSet.fold aux y_related (`Value state)
    with Not_found -> `Value state

  (* Adds dependencies to the state only if [eval_deps] is not None. *)
  let add_octagon eval_deps state octagon =
    if Ival.(equal top octagon.value) || is_redundant state.intervals octagon
    then `Value state
    else
      let state =
        if saturate_octagons
        then
          let x, y = Pair.get octagon.variables in
          let diamond = match octagon.operation with
            | Add -> { add = octagon.value; sub = Ival.top }
            | Sub -> { add = Ival.top; sub = octagon.value }
          in
          let relation = { vars = x, y; diamond } in
          saturate state y x relation >>- fun state ->
          saturate state x y relation
        else `Value state
      in
      state >>- fun state ->
      Octagons.add_octagon octagon state.octagons >>-: fun octagons ->
      let relations = Relations.relate octagon.variables state.relations in
      let variable_list = Pair.variable_list octagon.variables in
      let deps =
        match eval_deps with
        | None -> state.deps
        | Some eval_deps ->
          let add_var_deps deps v =
            Deps.add v (eval_deps v) deps
          in
          List.fold_left add_var_deps state.deps variable_list
      in
      { state with octagons; relations; deps }

  let add_diamond eval_deps state variables diamond =
    add_octagon eval_deps state { variables; operation = Add; value = diamond.add }
    >>- fun state ->
    add_octagon eval_deps state { variables; operation = Sub; value = diamond.sub }

  let remove state x =
    match Deps.remove x state.deps with
    | None -> state
    | Some deps ->
      let intervals = Intervals.remove x state.intervals in
      let state = { state with intervals; deps } in
      try
        let relations = Relations.find x state.relations in
        let remove_one y state =
          try
            let yrelations = Relations.find y state.relations in
            let yrelations = VarSet.remove x yrelations in
            let relations = Relations.add y yrelations state.relations in
            let pair, _ = Pair.make x y in
            let octagons = Octagons.remove pair state.octagons in
            { state with octagons; relations }
          with Not_found -> state
        in
        let state = VarSet.fold remove_one relations state in
        let relations = Relations.remove x state.relations in
        { state with relations }
      with Not_found -> state

  let related_octagons state x =
    try
      let related = Relations.find x state.relations in
      let aux y acc =
        let pair, swap = Pair.make x y in
        try
          let diamond = Octagons.find pair state.octagons in
          let diamond = Diamond.reverse_variables swap diamond in
          (y, diamond) :: acc
        with Not_found -> acc
      in
      VarSet.fold aux related []
    with Not_found -> []

  (* x' = ±x - delta *)
  let sub_delta ~inverse state x delta =
    let intervals = Intervals.remove x state.intervals in
    let state = { state with intervals } in
    let x_related = Relations.find x state.relations in
    let aux y state =
      let pair, swap = Pair.make x y in
      try
        let diamond = Octagons.find pair state.octagons in
        let diamond =
          if inverse
          then
            let op = if swap then fun x -> x else Arith.neg in
            { add = op diamond.sub;
              sub = op diamond.add }
          else diamond
        in
        let kind = Variable.kind x in
        let op = if swap then Arith.add else Arith.sub in
        let add =
          if Ival.(equal top diamond.add)
          then diamond.add
          else Arith.sub kind diamond.add delta
        and sub =
          if Ival.(equal top diamond.sub)
          then diamond.sub
          else op kind diamond.sub delta
        in
        let diamond' = { add; sub } in
        let octagons = Octagons.unsafe_add pair diamond' state.octagons in
        { state with octagons }
      with Not_found -> state
    in
    VarSet.fold aux x_related state
end


(* -------------------------------------------------------------------------- *)
(*                               Octagon domain                               *)
(* -------------------------------------------------------------------------- *)

module Domain = struct

  include State
  include Domain_builder.Complete (State)

  type value = Cvalue.V.t
  type location = Precise_locs.precise_location
  type origin
  let value_dependencies = Main_values.cval
  let location_dependencies = Main_locations.ploc

  let top_value = `Value (Cvalue.V.top, None), Alarmset.all

  (* Evaluator building. *)

  let evaluator_from_oracle oracle = fun expr ->
    match fst (oracle expr) with
    | `Bottom -> `Top (* should not happen *)
    | `Value (Cvalue.V.Top _) -> `Top
    | `Value cvalue -> `Value cvalue

  let evaluator_from_valuation valuation =
    let eval_exp expr =
      match valuation.Abstract_domain.find expr with
      | `Top -> `Top
      | `Value record ->
        match record.Eval.value.v with
        | `Bottom -> `Top (* TODO: why this keeps happening? *)
        | `Value (Cvalue.V.Top _) -> `Top
        | `Value cvalue -> `Value cvalue
    in
    let eval_loc lval = valuation.Abstract_domain.find_loc_def lval in
    let eval_deps var =
      Variable.deps ~eval_loc var
    in
    eval_exp, eval_deps

  (* Domain functions *)

  let extract_expr ~oracle _context state expr =
    let eval = evaluator_from_oracle oracle in
    let evaluate_octagon octagon = Octagons.evaluate octagon state.octagons in
    let env = mk_variable_builder eval state in
    let ival, alarms =
      Rewriting.evaluate_through_octagons eval evaluate_octagon env expr
    in
    if Ival.(equal ival top)
    then top_value
    else if Ival.is_bottom ival
    then `Bottom, Alarmset.all
    else `Value (Cvalue.V.inject_ival ival, None), alarms

  let extract_lval ~oracle:_ _context _t _lval _loc = top_value

  let reduce_further state expr value =
    match expr.node with
    | Lval lval when Ast_types.is_integral_or_pointer lval.typ ->
      begin
        try
          let x_ival = Cvalue.V.project_ival value in
          let var = Variable.make_lval lval in
          let kind = Variable.kind var in
          let octagons = State.related_octagons state var in
          let reduce acc (y, octagons) =
            match Variable.lval y with
            | None -> acc
            | Some lval ->
              let y_ival1 =
                if Ival.(equal top octagons.add)
                then Ival.top
                else Arith.sub kind octagons.add x_ival
              in
              let y_ival2 =
                if Ival.(equal top octagons.sub)
                then Ival.top
                else Arith.sub kind x_ival octagons.sub
              in
              let y_ival = Ival.narrow y_ival1 y_ival2 in
              if Ival.(equal top y_ival) then acc else
                let y_expr = Eva_ast.Build.lval lval in
                let y_cvalue = Cvalue.V.inject_ival y_ival in
                (y_expr, y_cvalue) :: acc
          in
          List.fold_left reduce [] octagons
        with Cvalue.V.Not_based_on_null -> []
      end
    | _ -> []

  let kill zone state =
    if Memory_zone.(equal zone top)
    then top
    else
      let modified = Memory_zone.join state.modified zone in
      let vars = Deps.intersects_zone state.deps zone in
      let state = List.fold_left State.remove state vars in
      { state with modified }


  exception EBottom

  let infer_octagons valuation expr ival state =
    let eval_exp, eval_deps = evaluator_from_valuation valuation in
    let env = mk_variable_builder eval_exp state in
    let octagons = Rewriting.make_octagons eval_exp env expr ival in
    let add_octagon state octagon =
      match State.add_octagon (Some eval_deps) state octagon with
      | `Bottom -> raise EBottom
      | `Value state -> state
    in
    List.fold_left add_octagon state octagons

  let infer_interval eval_deps expr ival state =
    if not infer_intervals
    then state
    else
      match expr.node with
      | Lval lval when Ast_types.is_integral lval.typ
                    && not (Eva_ast.lval_contains_volatile lval) ->
        let var = Variable.make_lval lval in
        let deps = Deps.add var (eval_deps var) state.deps in
        let intervals = Intervals.add var ival state.intervals in
        { state with intervals; deps }
      | _ -> state

  let update valuation state =
    let _eval_exp, eval_deps = evaluator_from_valuation valuation in
    let aux expr record state =
      let value = record.Eval.value in
      match record.reductness, value.v, value.initialized, value.escaping with
      | (Created | Reduced), `Value cvalue, true, false ->
        begin
          try
            let ival = Cvalue.V.project_ival cvalue in
            let state = infer_octagons valuation expr ival state in
            infer_interval eval_deps expr ival state
          with Cvalue.V.Not_based_on_null -> state
        end
      | _ -> state
    in
    try `Value (check "update" (valuation.Abstract_domain.fold aux state))
    with EBottom -> `Bottom

  let assign_interval eval_deps variable assigned state =
    if not infer_intervals
    then state
    else
      match assigned with
      | Assign v
      | Copy (_, { v = `Value v; initialized = true; escaping = false }) ->
        begin
          try
            let ival = Cvalue.V.project_ival v in
            let intervals = Intervals.add variable ival state.intervals in
            let deps = Deps.add variable (eval_deps variable) state.deps in
            { state with intervals; deps }
          with Cvalue.V.Not_based_on_null -> state
        end
      | _ -> state

  (* Assigns [lvalue] to [expr]. *)
  let assign_variable lvalue expr assigned valuation state =
    let eval_exp, eval_deps = evaluator_from_valuation valuation in
    let variable = Variable.make_lval lvalue in
    (* Remove lvals referring to the variable *)
    let lvalue_zone = (eval_deps variable).data in
    let modified = Memory_zone.join state.modified lvalue_zone in
    let state = { state with modified } in
    let vars = Deps.intersects_zone state.deps lvalue_zone in
    let vars = List.filter (Fun.negate (Variable.equal variable)) vars in
    let state = List.fold_left State.remove state vars in
    (* Interpret inversible assignment if possible *)
    (* TODO: redundant with rewrite_binop below. *)
    let env = mk_variable_builder eval_exp state in
    let vars = Rewriting.rewrite eval_exp env expr in
    let equal_varinfo v = Variable.equal variable v.Rewriting.var in
    let state =
      try
        let var = List.find equal_varinfo vars in
        let inverse = not var.Rewriting.sign in
        State.sub_delta ~inverse state variable var.Rewriting.coeff
      with Not_found -> State.remove state variable
    in
    let state = assign_interval eval_deps variable assigned state in
    let left_expr = Eva_ast.Build.lval lvalue in
    (* On the assignment X = E; if X-E can be rewritten as ±(X±Y-v),
       then the octagonal constraint [X±Y ∈ v] holds. *)
    let octagons = Rewriting.rewrite_binop eval_exp env left_expr Sub expr in
    let state =
      List.fold_left
        (fun acc (_sign, octagon) ->
           acc >>- fun state ->
           State.add_octagon (Some eval_deps) state octagon)
        (`Value state) octagons
    in
    state >>-: check "precise assign"

  let assign ~pos left_value expr assigned valuation state =
    if Position.is_local pos
    && Ast_types.is_integral_or_pointer left_value.lval.typ
    && not (Eva_ast.lval_contains_volatile left_value.lval)
    then assign_variable left_value.lval expr assigned valuation state
    else
      let written_loc = Precise_locs.imprecise_location left_value.lloc in
      let written_zone =
        Locations.(enumerate_valid_bits Write written_loc)
      in
      let state = kill written_zone state in
      `Value (check "imprecise assign" state)

  let assume ~pos:_ _exp _bool = update

  let start_recursive_call recursion state =
    let vars = List.map fst recursion.substitution @ recursion.withdrawal in
    let var_deps v = Deps.intersects_var state.deps v in
    let vars = List.flatten (List.map var_deps vars) in
    List.fold_left State.remove state vars

  let start_call ~pos:_ call recursion valuation state =
    if intraprocedural ()
    then `Value (empty ())
    else
      let state = { state with modified = Memory_zone.bottom } in
      match recursion with
      | Some recursion ->
        (* No relation inferred from the assignment of formal parameters
           for recursive calls, because the valuation cannot be used safely
           as the substitution of local and formals variables has not been
           applied to it. *)
        `Value (start_recursive_call recursion state)
      | None ->
        let assign_formal state { formal; concrete; avalue } =
          let lval = Eva_ast.Build.var formal in
          if Ast_types.is_integral_or_pointer formal.vtype
          then state >>- assign_variable lval concrete avalue valuation
          else state
        in
        List.fold_left assign_formal (`Value state) call.arguments

  let finalize_call ~pos:_ _call _recursion ~pre ~post =
    if intraprocedural ()
    then `Value (kill post.modified pre)
    else
      let modified = Memory_zone.join post.modified pre.modified in
      `Value { post with modified }

  let logic_assign _logic_assign location state =
    let loc = Precise_locs.imprecise_location location in
    let zone = Locations.(enumerate_valid_bits Write loc) in
    let state = kill zone state in
    check "logic_assign" state

  let enter_scope _kind _varinfos state = state
  let leave_scope _kf varinfos state =
    let var_deps v = Deps.intersects_var state.deps v in
    let vars = List.concat_map var_deps varinfos in
    let state = List.fold_left State.remove state vars in
    check "leave_scope" state

  let initialize_variable _lval _location ~initialized:_ _value state = state
  let initialize_variable_using_type _kind _varinfo state = state

  let relate bases state =
    if intraprocedural ()
    then Base.SetLattice.empty
    else
      let add_related_bases acc var =
        try
          let related = Relations.find var state.relations in
          VarSet.elements related |>
          List.map (Deps.get_var_bases state.deps) |>
          List.fold_left Base.SetLattice.join acc
        with Not_found -> acc
      in
      let aux base acc =
        let variables = Deps.intersects_base state.deps base in
        List.fold_left add_related_bases acc variables
      in
      Base.Hptset.fold aux bases Base.SetLattice.empty

  let filter bases state =
    if intraprocedural ()
    then state
    else
      let removed_vars, deps = Deps.filter bases state.deps in
      let mem_var v = not (VarSet.mem v removed_vars) in
      let mem_pair pair =
        let x, y = Pair.get pair in
        mem_var x && mem_var y
      in
      let octagons = Octagons.filter mem_pair state.octagons in
      let intervals = Intervals.diff_with_shape removed_vars state.intervals in
      let relations = Relations.diff_with_shape removed_vars state.relations in
      { state with octagons; intervals; relations; deps }

  let interprocedural_reuse =
    let cache = Hptmap_sig.PersistentCache "Octagons.reuse"
    and symmetric = false
    and idempotent = true
    and decide _key left _right = left in
    let join_oct = Octagons.internal_join ~cache ~symmetric ~idempotent ~decide
    and join_itv = Intervals.internal_join ~cache ~symmetric ~idempotent ~decide
    and join_rel = Relations.union in
    fun ~current_input ~previous_output ->
      let current_input = kill previous_output.modified current_input in
      let intervals = join_itv previous_output.intervals current_input.intervals
      and deps = Deps.narrow previous_output.deps current_input.deps in
      (* We use [add_diamond] to add each relation from the previous output
         into the current input, in order to benefit from the (partial)
         saturation of octagons performed by this function. For a maximum
         precision, a complete saturation would be required.
         Otherwise, we use instead a more efficient (but less precise) merge
         of the maps from the previous output and the current input. *)
      if saturate_octagons
      then
        let state = { current_input with intervals; deps } in
        let add_diamond variables diamond acc =
          match add_diamond None acc variables diamond with
          | `Value state -> state
          | `Bottom ->
            Self.failure ~current:true ~once:true
              "Octagon domain: the use of the memexec cache unexpectedly \
               led to bottom, which could be a bug. The analysis continues \
               by ignoring the relations leading to bottom.";
            acc
        in
        Octagons.fold add_diamond previous_output.octagons state
      else
        { octagons = join_oct previous_output.octagons current_input.octagons;
          relations = join_rel previous_output.relations current_input.relations;
          modified = current_input.modified;
          intervals; deps; }

  let reuse _bases ~current_input ~previous_output =
    if intraprocedural ()
    then previous_output
    else
      let t = interprocedural_reuse ~current_input ~previous_output in
      check "reuse result" t

  (* Could be optimized by sharing code with [filter] below. *)
  let overwrite bases ~on:state ~by:_ =
    let base_deps base = Deps.intersects_base state.deps base in
    let vars = List.concat_map base_deps (Base.Hptset.elements bases) in
    List.fold_left remove state vars
end

include Domain

let registered =
  let name = "octagon"
  and descr =
    Format.asprintf
      "Infers integer relations of the form b < %tX %t Y < e, \
       where X, Y are program lvalues and b, e are constants."
      Unicode.pp_plus_minus Unicode.pp_plus_minus
  in
  Abstractions.Domain.register ~name ~descr ~priority:8 (module Domain)
