(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** E-ACSL built-in database. *)

val mem: string -> bool
(** @return true iff the given function name is an E-ACSL built-in *)

val find: string -> Cil_types.varinfo
(** Get the varinfo corresponding to the given E-ACSL built-in name.
    @raise Not_found if it is not a built-in *)

val update: string -> Cil_types.varinfo -> unit
(** If the given name is an E-ACSL built-in, change its old varinfo by the given
    new one. *)
