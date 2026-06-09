(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module is dedicated to some preprocessing on the predicates:
    - It guards all the [Pvalid] and [Pvalid_read] clauses with
      an adequate [Pinitialized] clause;
    - It replaces all the applications [Papp] by a corresponding
      term obtained as an application [Tapp]
      The predicates that have undergone these changed are
      called the preprocessed predicates.
*)

open Cil_types
open Cil_datatype

val preprocess : file -> unit
(** Preprocess all the predicates of the ast and store the results *)

val preprocess_annot : code_annotation -> unit
(** Preprocess of the predicate of a single code annotation and store
    the results *)

val preprocess_predicate : predicate -> unit
(** Preprocess a predicate and its children and store the results  *)

(** The analyses in [Logic_normalizer] may:
    - create new auxiliary predicates and logic functions
    - transform inductively and axiomatically defined logic functions and
      predicates into recursively defined ones
      This module provides functions to inquire about their status. *)
module Logic_infos : sig
  val generated_of : logic_info -> Logic_info.Set.t
  (** auxiliary [logic_info]s generated from the given [logic_info]. *)

  val origin_of_lv : logic_var -> logic_var
  (** Identify the [logic_info] from which the [logic_info] identified by the
      given argument stems from. This is required in order to create meaningful
      feedback messages for the user who should not be confronted with the
      names of generated logic functions. *)
end

val predicate_is_unsound_if_false : logic_info -> bool

val get_pred : predicate -> predicate
(** Retrieve the preprocessed form of a predicate *)

val get_orig_pred : predicate -> predicate
(** Retrieve the original form of the given predicate *)

val get_term : term -> term
(** Retrieve the preprocessed form of a term *)

val get_orig_term : term -> term
(** Retrieve the original form of the given term *)

val clear: unit -> unit
(** clear the table of normalized predicates *)
