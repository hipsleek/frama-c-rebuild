(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** General module for E-ACSL analyses *)

val check_integrity : unit -> unit

val preprocess: unit -> unit
(** Analyses to run before starting the translation *)

val reset: unit -> unit
(** Clear the results of the analyses *)
