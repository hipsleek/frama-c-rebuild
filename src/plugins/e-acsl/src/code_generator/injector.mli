(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** The E-ACSL main instrumentation step. *)

val inject: unit -> unit
(** Inject all the necessary pieces of code for monitoring the program
    annotations. *)
