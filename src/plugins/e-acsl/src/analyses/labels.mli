(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Pre-analysis for Labeled terms and predicates.

    This pre-analysis records, for each labeled term or predicate, the place
    where the translation must happen.

    The list of labeled terms or predicates to be translated for a given
    statement is provided by [Labels.at_for_stmt]. *)

open Cil_types
open Analyses_datatype

val get_first_inner_stmt: stmt -> stmt
(** If the given statement has a label, return the first statement of the block.
    Otherwise return the given statement. *)

val at_for_stmt: stmt -> At_data.t list
(** @return the set of labeled predicates and terms to be translated on the
    given statement. *)

val at_for_pot : Pred_or_term.t -> At_data.t
(** @return the set of labeled predicates and terms to be translated on the
    given predicate/term. *)

val preprocess: file -> unit
(** Analyse sources to find the statements where a labeled predicate or term
    should be translated. *)

val reset: unit -> unit
(** Reset the results of the pre-analysis. *)

val _debug: unit -> unit
(** Print internal state of labels translation. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

val has_empty_quantif_ref: ((term * logic_var * term) list -> bool) ref
