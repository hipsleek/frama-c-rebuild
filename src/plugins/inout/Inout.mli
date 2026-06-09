(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Returns the memory zone needed to evaluate the given expression at the
    given statement. *)
val expr_inputs: stmt -> exp -> Memory_zone.t

(** Returns the memory zone read by the given statement. *)
val stmt_inputs: stmt -> Memory_zone.t

(** Returns the memory zone modified by the given statement. *)
val stmt_outputs: stmt -> Memory_zone.t

(** Returns the memory zone read by the given function, including its
    local and formal variables. *)
val kf_inputs: kernel_function -> Memory_zone.t

(** Returns the memory zone read by the given function, without its
    local and formal variables. *)
val kf_external_inputs: kernel_function -> Memory_zone.t

(** Returns the memory zone modified by the given function, including its
    local and formal variables. *)
val kf_outputs: kernel_function -> Memory_zone.t

(** Returns the memory zone modified by the given function, without its
    local and formal variables. *)
val kf_external_outputs: kernel_function -> Memory_zone.t

(** Returns the inputs/outputs computed for the given function.
    If [stmt] is specified and is a possible call to the given function,
    returns the inputs/outputs for this call specifically. *)
val get_precise_inout: ?stmt:stmt -> kernel_function -> Inout_type.t

val self: State.t
