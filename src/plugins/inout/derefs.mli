(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


val pretty_external: Format.formatter -> Cil_types.kernel_function -> unit
val compute_external: Cil_types.kernel_function -> unit
