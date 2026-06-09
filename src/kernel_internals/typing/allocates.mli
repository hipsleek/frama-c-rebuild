(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Generation of default [allocates \nothing] clauses.

    Automatic generation of [allocates \nothing] and [loop allocates \nothing]
    clauses has been removed until a plugin supports them.
    To force generation, the following functions can be used.
*)

val add_allocates_nothing_funspec: Cil_types.kernel_function -> unit
(** Adds [allocates \nothing] to the default behavior of the function
    if it does not have and allocation clause yet. *)

class vis_add_loop_allocates: Visitor.frama_c_inplace
(** This class adds [loop allocates] clauses to all the statements it visits. *)

val add_allocates_nothing: unit -> unit
(** Add default [allocates \nothing] clauses to all functions and loops. *)
