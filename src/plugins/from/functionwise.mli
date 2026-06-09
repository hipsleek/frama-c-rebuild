(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Computation of functional dependencies. In this module, the results are
    computed from the synthetic results of the value analysis. *)

open Cil_types

val self : State.t
val compute : kernel_function -> unit
val compute_all : unit -> unit
val is_computed : kernel_function -> bool
val get : Cil_types.kernel_function -> Eva.Assigns.t
val pretty : Format.formatter -> kernel_function -> unit
