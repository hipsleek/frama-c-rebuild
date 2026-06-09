(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val dkey : Options.category

val preprocess : file -> unit
(** Compute the RTE table for a given file. *)

val iter_on_guards : term -> (predicate -> unit) -> unit
(** retrieve the list of guards for a given term and, if it exists, iterate over
    all elements applying a given function to them. *)

val fold_guards_il : default:'a -> term -> (predicate -> 'a -> 'a) -> 'a
(* retrieve the list of guards for a given term, if it exists, and apply folding
   operation to it using a given function. *)

val fold_guards_old : default:'a -> term -> (predicate -> 'a -> 'a) -> 'a
(* retrieve the list of guards for a given term and there sub-terms, if they
   exist, and applies folding operation to it using a given function. *)

val clear : unit -> unit
(** Clear the table. *)
