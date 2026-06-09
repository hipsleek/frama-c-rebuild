(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val get_preprocessed_quantifier:
  predicate -> (term * logic_var * term) list * predicate
(** @return the preprocessed of a quantified predicate the
    [(term * logic_var * term) list] is the list of all the quantified variables
    along with their syntactic guards, and the [predicate] is the goal: the
    original predicate with all the quantifiers removed.
    @raises Typing_error if predicate cannot be typed.
    @raises Abort.fatal if predicate has not been preprocessed. *)

val add_guard_for_small_type : logic_var -> predicate -> unit
(** Adds an optional additional guard condition that comes from the typing *)

val get_guard_for_small_type : logic_var -> predicate option
(** @return the optional additional guard for a quantified logic variables. It
    may happen that the syntactic guard of the variable can be refined with the
    type of the variable, this additional predicate translates this refinement *)

val replace : predicate -> (term * logic_var * term) list -> predicate -> unit
(** Replace the computed guards. This is because the typing sometimes simplifies
    the computed bounds, so we allow for storing new bounds *)

val clear_guards : unit -> unit
(** Clear the table of guard conditions for quantified variables *)

val preprocess : file -> unit
(** Preprocess all the quantified predicates in the ast and store the result
    in an hashtable *)

val preprocess_annot : code_annotation -> unit
(** Preprocess the quantified predicate in a given code annotation *)

val preprocess_predicate : predicate -> unit
(** Preprocess the quantified predicate in a given predicate *)
