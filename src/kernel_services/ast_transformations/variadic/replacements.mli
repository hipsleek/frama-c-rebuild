(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val add: varinfo -> varinfo -> unit
(** [add replaced original] stores the association of the original and the
    replaced functions in a project state. *)

val find: varinfo -> varinfo
(* [find fct] returns the original function for [fct] from the project state if
   it has been replaced. Raise [Not_found] if no original function exists. *)

val mem: varinfo -> bool
(* [mem fct] returns true if an original function exists for [fct], false
   otherwise. *)
