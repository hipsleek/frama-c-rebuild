(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Variable name generator wrt a lexical scope. *)

type scope =
  | Global
  | Function
  | Block

val get: scope:scope -> string -> string
(** @return a fresh, sanitized version of the given name for a given scope. *)

val clear_locals: unit -> unit
(** Reset the generator for variables that are local to a block or a
    function. *)

val of_exp : Cil_types.exp -> string
(** Generate a reasonable variable name element from the given expression.
    Note that in order to obtain a fresh name the result still needs to be
    piped through [get]. *)
