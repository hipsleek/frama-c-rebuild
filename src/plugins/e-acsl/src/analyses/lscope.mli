(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Analyses_types

(* Handle the logic scope of a term.
   We define the logic scope of a term [t] to be the set of PURELY logic
   variables that are bound in [t] in case of use. *)

type t = lscope

module D: Datatype.S with type t = lscope

val empty: t
(* Create an empty logic scope. *)

val add: lscope_var -> t -> t
(* Return a new logic scope in which the given [lscope_var] has been added. *)

val remove: lscope_var -> t -> t
(** @return a new logic scope in which the given [lscope_var] has been removed
    if it was present. Use physical equality to check if the [lscope_var] is
    present. *)

val get_all: t -> lscope_var list
(* Return the list of [lscope_var] of the given logic scope.
   The first element is the last [lscope_var] that was added to [t], the
   second element is the second to last [lscope_var] that was added to [t], and
   so on. *)

val is_used: t -> pred_or_term -> bool
(* [is_used lscope pot] returns [true] iff [pot] uses a variable from
   [lscope]. *)
