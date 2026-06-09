(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val compute: kernel_function -> unit

val get_external: kernel_function -> Inout_type.t
val get_external_aux: ?stmt:stmt -> kernel_function -> Inout_type.t

val get_internal_precise: ?stmt:stmt -> kernel_function -> Inout_type.t

val pretty_operational_inputs_internal:
  Format.formatter -> kernel_function -> unit
val pretty_operational_inputs_external:
  Format.formatter -> kernel_function -> unit
val pretty_operational_inputs_external_with_formals:
  Format.formatter -> kernel_function -> unit

val pretty: Format.formatter -> Inout_type.t -> unit
