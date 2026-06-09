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

(** Types and functions related to evaluations.
    Heavily used by abstract values and domains, evaluation of expressions,
    transfer functions of instructions and the dataflow analysis. *)


(* -------------------------------------------------------------------------- *)
(**                      {2 Lattice structure }                               *)
(* -------------------------------------------------------------------------- *)

include module type of Lattice_bounds
include module type of Bottom.Operators


(* -------------------------------------------------------------------------- *)
(**                    {2 Types for the evaluations }                         *)
(* -------------------------------------------------------------------------- *)

(** A type and a set of alarms. *)
type 't with_alarms = 't * Alarmset.t

(** Most forward evaluation functions return the set of alarms resulting from
    the operations, and a result which can be `Bottom, if the evaluation fails,
    or the expected value. *)
type 't evaluated = 't or_bottom with_alarms


(** This monad propagates the `Bottom value if needed, and join the alarms
    of each evaluation. *)
val (>>=) : 'a evaluated -> ('a -> 'b evaluated) -> 'b evaluated

(** Use this monad of the following function returns no alarms. *)
val (>>=.) : 'a evaluated -> ('a -> 'b or_bottom) -> 'b evaluated

(** Use this monad if the following function returns a simple value. *)
val (>>=:) : 'a evaluated -> ('a -> 'b) -> 'b evaluated


module Evaluated : sig
  type 'a t = 'a evaluated
  module Operators : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let& ) : 'a t -> ('a -> 'b or_bottom) -> 'b t
  end
end

(** Most backward evaluation function returns `Bottom if the reduction leads to
    an invalid state, `Unreduced if no reduction can be performed, or the
    reduced value. *)
type 'a reduced = [ `Bottom | `Unreduced | `Value of 'a ]

(* -------------------------------------------------------------------------- *)
(**                    {2 Cache for the evaluations }                         *)
(* -------------------------------------------------------------------------- *)


(** The evaluation of an expression stores in a cache the result of all
    intermediate computation. This cache is the outcome of the evaluation,
    and is used by abstract domains for transfer functions.
    It contains
    - the abstract value of each sub-expression, as well as its origin (see
      below), its reduction (see below), and the alarms produced by its
      evaluation.
    - the abstract location of each lvalue of the expression, as well as
      its type, and the alarms produced by its dereference.

    The evaluation queries the abstract domain the value of some sub-expressions.

    The origin of an abstract value is then provided by the abstract domain, and
    kept in the cache. The origin is None if the value has been internally
    computed without calling the domain.

    Also, a value provided by the domain may be reduced by the internal
    computation of the forward and backward evaluation. Such a reduction is
    tracked by the evaluator and reported to the domain, in the cache.
    States of reduction are:
    - Unreduced: the value provided by the domain could not be reduced;
    - Reduced: the value provided by the domain has been reduced;
    - Created: the domain has returned `Top for the given expression;
    - Dull: the domain was not queried for the given expression.
*)

(** State of reduction of an abstract value. *)
type reductness =
  | Unreduced  (** No reduction. *)
  | Reduced    (** A reduction has been performed for this expression. *)
  | Created    (** The abstract value has been created. *)
  | Dull       (** Reduction is pointless for this expression. *)

(** Right values with 'undefined' and 'escaping addresses' flags. *)
(* TODO: find a better name. *)
type 'a flagged_value = {
  v: 'a or_bottom;
  initialized: bool;
  escaping: bool;
}

val pretty_value_with_flags:
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a flagged_value -> unit

(** Data record associated to each evaluated expression. *)
type ('a, 'origin) record_val = {
  value: 'a flagged_value;  (** The resulting abstract value *)
  origin: 'origin option;   (** The origin of the abstract value *)
  reductness : reductness;  (** The state of reduction. *)
  val_alarms : Alarmset.t   (** The emitted alarms during the evaluation. *)
}

(** Data record associated to each evaluated left-value. *)
type 'a record_loc = {
  loc: 'a;                  (** The location of the left-value. *)
  loc_alarms: Alarmset.t    (** The emitted alarms during the evaluation. *)
}

(** Results of an evaluation: the results of all intermediate calculation (the
    value of each expression and the location of each lvalue) are cached in a
    map. *)
module type Valuation = sig
  type t

  (** Abstract value. *)
  type value

  (** Origin of values. *)
  type origin

  (** Abstract memory location. *)
  type loc

  val empty : t
  val find : t -> exp -> (value, origin) record_val or_top
  val add : t -> exp -> (value, origin) record_val -> t
  val fold : (exp -> (value, origin) record_val -> 'a -> 'a) -> t -> 'a -> 'a
  val find_loc : t -> lval -> loc record_loc or_top
  val find_loc_def : t -> lval -> loc
  val remove : t -> exp -> t
  val remove_loc : t -> lval -> t
end

module Clear_Valuation (Valuation: Valuation) : sig
  (** Removes from the valuation all the subexpressions of [expr] that contain
      [subexpr], except [subexpr] itself. *)
  val clear_englobing_exprs :
    Valuation.t -> expr:exp -> subexpr:exp -> Valuation.t
end


(* -------------------------------------------------------------------------- *)
(**                       {2 Types of assignments }                           *)
(* -------------------------------------------------------------------------- *)

(** Lvalue with its location and type. *)
type 'loc left_value = {
  lval: lval;
  lloc: 'loc;
}

(** Assigned values. *)
type ('loc, 'value) assigned =
  | Assign of 'value
  (** Default assignment of a value. *)
  | Copy of 'loc left_value * 'value flagged_value
  (** Copy of the location of a lvalue, that contains the given flagged value.
      The value is copied exactly, with possible indeterminateness. *)

(* Extract the assigned value from a [value assigned]. *)
val value_assigned : ('loc, 'value) assigned -> 'value or_bottom

(** The location of a logic dependency:
    - the memory location of <lval> for a clause "\from lval" (default case).
    - the address of x in case of a clause "\from &x". *)
type 'location location_or_address =
  | Location of 'location
  | Address of varinfo

(** The logic dependency of an ACSL assigns clause. *)
type 'location logic_dependency =
  { term: identified_term;
    (** The ACSL term of the dependency, expressed in a \from clause. *)
    direct: bool;
    (** Whether the dependency is direct (default case), or has been declared
        as "indirect", meaning that its value is only used in a conditional or
        to compute an address. *)
    location: 'location location_or_address;
    (** The memory location of the dependency. *)
  }

type 'location logic_assign =
  | Assigns of identified_term * 'location logic_dependency list
  (** assigns clause, with the dependencies of the \from clause.
      An empty list means \nothing. *)
  | Allocates of identified_term
  | Frees of identified_term

(* -------------------------------------------------------------------------- *)
(**                       {2 Interprocedural Analysis }                       *)
(* -------------------------------------------------------------------------- *)

(** Argument of a function call. *)
type ('loc, 'value) argument = {
  formal: varinfo;          (** The formal argument of the called function. *)
  concrete: exp;            (** The concrete argument at the call site *)
  avalue: ('loc, 'value) assigned;  (** The value of the concrete argument. *)
}

(** A function call. *)
type ('loc, 'value) call = {
  kf: kernel_function;                        (** The called function. *)
  callstack: Callstack.t;                     (** The current callstack
                                                  (with this call on top). *)
  arguments: ('loc, 'value) argument list;    (** The arguments of the call. *)
  rest: (exp * ('loc, 'value) assigned) list; (** Extra-arguments. *)
  return: varinfo option;                     (** Fake varinfo to store the
                                                  return value of the call.
                                                  Same varinfo for every
                                                  call to a given function. *)
}

(** [position_of_call call] creates the global or local position from the
    call information [call]. If this is the entry point of the analysis (i.e.
    the callsite is [Kglobal]) then the position will be a root call. If there
    is a statement callsite then the position will be local. *)
val position_of_call  : ('a, 'b) call -> Position.t

(** Information needed to interpret a recursive call.
    The local variables and formal parameters of different recursive calls
    should not be mixed up. Those of the current call must be temporary withdraw
    or replaced from the domain states before starting the new recursive call,
    and the inverse transformation must be made at the end of the call. *)
type recursion = {
  depth: int;
  (** Depth of the recursive call, i.e. the number of previous call to the
      called function in the current callstack. *)
  substitution: (varinfo * varinfo) list;
  (** List of variables substitutions to be performed by the domains: for each
      pair, the first variable must be replaced by the second one in the domain
      state. *)
  base_substitution: Base.substitution;
  (** Same substitution as the previous field, for bases. *)
  withdrawal: varinfo list;
  (** List of variables to be temporary removed from the state at the start of a
      new recursive call (by the function [start_call] of the abstract domains),
      or to be put back in the state at the end of a recursive call (by the
      function [finalize_call] of the abstract domains).  *)
  base_withdrawal: Base.Hptset.t;
  (** Same withdrawal as the previous field, for bases. *)
}

(** Can the results of a function call be cached with memexec? *)
type cacheable =
  | Cacheable      (** Functions whose result can be safely cached. *)
  | NoCache        (** Functions whose result should not be cached, but for
                       which the caller can still be cached. Typically,
                       functions printing something during the analysis. *)
  | NoCacheCallers (** Functions for which neither the call, neither the
                       callers, can be cached. *)
