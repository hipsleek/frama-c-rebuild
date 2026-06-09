(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Handling of recursion cycles in the callgraph *)

open Cil_types
open Eval

(* Returns the specification for a recursive call to the given function. Fails
   if the function has no specification. Marks the preconditions of the call
   as unknowns. *)
val check_spec: kinstr -> kernel_function -> unit

(** Creates the information about a recursive call. *)
val make: ('v, 'loc) call -> recursion option

(** Changes the information about a recursive call to be used at the end
    of the call. *)
val revert: recursion -> recursion
