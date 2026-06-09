(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type lval = Eva_ast.lval
type lhost = Eva_ast.lhost
type exp = Eva_ast.exp

(** *)

(* -------------------------------------------------------------------------- *)
(**                       {2 Lattice structure }                              *)
(* -------------------------------------------------------------------------- *)

include Lattice_bounds
include Bottom.Operators


(* -------------------------------------------------------------------------- *)
(**                     {2 Types for the evaluations }                        *)
(* -------------------------------------------------------------------------- *)

(* Forward evaluation. *)
type 't with_alarms = 't * Alarmset.t
type 't evaluated = 't or_bottom with_alarms

(* This monad propagates the `Bottom value if needed and join the potential
   alarms returned during the evaluation. *)
let (>>=) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t -> let t', a' = f t in t', Alarmset.combine a a'

(* Use this monad if the following function returns a simple value. *)
let (>>=:) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t -> let t' = f t in `Value t', a

(* Use this monad if the following function returns no alarms. *)
let (>>=.) (t, a) f = match t with
  | `Bottom  -> `Bottom, a
  | `Value t -> let t' = f t in t', a

module Evaluated = struct
  type 'a t = 'a evaluated
  module Operators = struct
    let ( let* ) evaluated f = evaluated >>= f
    let ( let+ ) evaluated f = evaluated >>=: f
    let ( let& ) evaluated f = evaluated >>=. f
  end
end

(* Backward evaluation. *)
type 'a reduced = [ `Bottom | `Unreduced | `Value of 'a ]

(* -------------------------------------------------------------------------- *)
(**                     {2 Cache for the evaluations }                        *)
(* -------------------------------------------------------------------------- *)

(* State of the reduction of an abstract value. *)
type reductness =
  | Unreduced  (* No reduction. *)
  | Reduced    (* A reduction has been performed for this expression. *)
  | Created    (* The abstract value has been created. *)
  | Dull       (* Reduction is pointless for this expression. *)

(* Right values with 'undefined' and 'escaping addresses' flags. *)
type 'a flagged_value = {
  v: 'a or_bottom;
  initialized: bool;
  escaping: bool;
}

let pretty_flags fmt value = match value.initialized, value.escaping with
  | false, true  -> Format.pp_print_string fmt "UNINITIALIZED or ESCAPINGADDR"
  | false, false -> Format.pp_print_string fmt "UNINITIALIZED"
  | true, true   -> Format.pp_print_string fmt "ESCAPINGADDR"
  | true, false  -> Format.pp_print_string fmt "BOTTOM"

let pretty_value_with_flags pp fmt value = match value.v with
  | `Bottom -> pretty_flags fmt value
  | `Value v ->
    if value.initialized && not value.escaping
    then pp fmt v
    else Format.fprintf fmt "%a or %a" pp v pretty_flags value

(* Data record associated to each evaluated expression. *)
type ('a, 'origin) record_val = {
  value : 'a flagged_value;  (* The resulting abstract value *)
  origin: 'origin option;   (* The origin of the abstract value *)
  reductness : reductness;  (* The state of reduction. *)
  val_alarms : Alarmset.t   (* The emitted alarms during the evaluation. *)
}

(* Data record associated to each evaluated left-value. *)
type 'a record_loc = {
  loc: 'a;                  (* The location of the left-value. *)
  loc_alarms: Alarmset.t    (* The emitted alarms during the evaluation. *)
}

(* Results of an evaluation: the results of all intermediate calculation (the
    value of each expression and the location of each lvalue) are cached in a
    map. *)
module type Valuation = sig
  type t
  type value  (* Abstract value. *)
  type origin (* Origin of values. *)
  type loc    (* Abstract memory location. *)

  val empty : t
  val find : t -> exp -> (value, origin) record_val or_top
  val add : t -> exp -> (value, origin) record_val -> t
  val fold : (exp -> (value, origin) record_val -> 'a -> 'a) -> t -> 'a -> 'a
  val find_loc : t -> lval -> loc record_loc or_top
  val find_loc_def : t -> lval -> loc
  val remove : t -> exp -> t
  val remove_loc : t -> lval -> t
end

(* Returns the list of the subexpressions of [expr] that contain [subexpr],
   without [subexpr] itself. *)
let compute_englobing_subexpr ~subexpr ~expr =
  let merge = Option.merge (@) in
  (* Returns [Some] of the list of subexpressions of [expr] that contain
     [subexpr], apart [subexpr] itself, or [None] if [subexpr] does not appear
     in [expr]. *)
  let rec compute expr =
    if Eva_ast.Exp.equal expr subexpr
    then Some []
    else
      let sublist = match expr.node with
        | UnOp (_, e, _)
        | CastE (_, e) ->
          compute e
        | BinOp (_, e1, e2, _) ->
          merge (compute e1) (compute e2)
        | Lval {node = (host, offset)} ->
          merge (compute_host host) (compute_offset offset)
        | _ -> None
      in
      Option.map (fun l -> expr :: l) sublist
  and compute_host = function
    | Var _ -> None
    | Mem e -> compute e
  and compute_offset offset = match offset with
    | NoOffset -> None
    | Field (_, offset) -> compute_offset offset
    | Index (index, offset) ->
      merge (compute index) (compute_offset offset)
  in
  Option.value ~default:[] (compute expr)

module Englobing = Datatype.Pair_with_collections (Eva_ast.Exp) (Eva_ast.Exp)
module SubExprs = Datatype.List (Eva_ast.Exp)

module EnglobingSubexpr =
  State_builder.Hashtbl (Englobing.Hashtbl) (SubExprs)
    (struct
      let name = "Value.Eval.Englobing_subexpressions"
      let size = 32
      let dependencies = [ Ast.self ]
    end)

let compute_englobing_subexpr ~subexpr ~expr=
  EnglobingSubexpr.memo
    (fun (expr, subexpr) -> compute_englobing_subexpr ~subexpr ~expr)
    (expr, subexpr)

module Clear_Valuation (Valuation : Valuation) = struct
  let clear_englobing_exprs valuation ~expr ~subexpr =
    let englobing = compute_englobing_subexpr ~subexpr ~expr in
    let remove valuation expr =
      let valuation = Valuation.remove valuation expr in
      match expr.node with
      | Lval lval -> Valuation.remove_loc valuation lval
      | _ -> valuation
    in
    List.fold_left remove valuation englobing
end


(* -------------------------------------------------------------------------- *)
(**                        {2 Types of assignments }                          *)
(* -------------------------------------------------------------------------- *)

type 'loc left_value = {
  lval: lval;
  lloc: 'loc;
}

(* Assigned values. *)
type ('loc, 'value) assigned =
  | Assign of 'value
  | Copy of 'loc left_value * 'value flagged_value

let value_assigned = function
  | Assign v -> `Value v
  | Copy (_, copied) -> copied.v

type 'location location_or_address =
  | Location of 'location
  | Address of varinfo

type 'location logic_dependency =
  { term: identified_term;
    direct: bool;
    location: 'location location_or_address; }

type 'location logic_assign =
  | Assigns of identified_term * 'location logic_dependency list
  | Allocates of identified_term
  | Frees of identified_term

(* -------------------------------------------------------------------------- *)
(**                       {2 Interprocedural Analysis }                       *)
(* -------------------------------------------------------------------------- *)

type ('loc, 'value) argument = {
  formal: varinfo;
  concrete: exp;
  avalue: ('loc, 'value) assigned;
}

type ('loc, 'value) call = {
  kf: kernel_function;
  callstack: Callstack.t;
  arguments: ('loc, 'value) argument list;
  rest: (exp * ('loc, 'value) assigned) list;
  return: varinfo option;
}

type recursion = {
  depth: int;
  substitution: (varinfo * varinfo) list;
  base_substitution: Base.substitution;
  withdrawal: varinfo list;
  base_withdrawal: Base.Hptset.t;
}

type cacheable = Cacheable | NoCache | NoCacheCallers

(** This function cannot be in {!Position} as it would create a dependency cycle
    [Position] -> [Eval] -> [Alarmset] -> [Position] *)
let position_of_call call =
  let kf, lloc = Callstack.pop_call call.callstack in
  assert (Kernel_function.equal kf call.kf);
  match lloc, call.return with
  | None, Some vi ->
    Position.global_init vi
  | None, None ->
    Position.root_call ~thread:call.callstack.thread ~entry_point:call.kf
  | Some (stmt, caller_stack), _ ->
    Position.local stmt caller_stack
