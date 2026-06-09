(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Dump calls to Mcfg into DOT graphs *)

open Cil_types

include Mcfg.S

val fopen : kernel_function -> string option -> unit
val flush : unit -> unit

(**************************************************************************)
