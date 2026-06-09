(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module transforms inductive predicate definitions into "direct"
    predicate definitions (introduced by [LBpred]), a form that can then be
    translated into Cil.
    It is in general not clear how inductive definitions can be translated into
    an executable form. However for a restricted set of inductive definitions
    this can be achieved. This subset is constituted of generalized Horn
    clauses, described in the reference manual under the subsection Inductive
    predicates. *)

open Cil_types

exception Unsupported
(** This exception is raised if an inductive definition is not of a form that
    can be transformed into an executable form. *)

module Derived_functions : sig
  val iter : (logic_info -> logic_info -> unit) -> unit
end
(** [extract_predicate] may generate auxiliary logic functions. Such a logic
    function f is stored in an hash table of this module table with the
    predicate p (from which f has been derived) as a key and f as a value.
    There may be multiple logic functions associated with a predicate. *)

val predicate_is_unsound_if_false : logic_info -> bool
(** Predicates extracted in incomplete mode rely on function extraction.
    This extraction may be wrong to assume that the predicate is actually
    functional w.r.t. to the argument chosen as a result.
    In the future extracted logic functions should return (lazy) lists instead
    of just one result. In the meantime whenever such a predicate is false
    it might be due to its logic function having missed a case that was being
    shadowed by a preceding case. *)

val extract_predicate : logic_info -> logic_info
(** transform a [logic_info] containing an inductively defined predicate
    ([LBinductive]) into a "directly" defined predicated ([LBpred]).

    @raise Unsupported *)

val clear : unit -> unit
(** clear memoization of inductives *)

val is_inductive : logic_info -> bool
(** @return [true] if [logic_info] contains an inductive definition *)

val is_fallthrough_term: term -> bool
(** For incomplete inductive definitions, it may happen that none of the
    constructors applies. In this case a fallthrough term is generated which is
    to be translated into a failing assertion. This function tests for terms
    being fallthrough terms. *)
