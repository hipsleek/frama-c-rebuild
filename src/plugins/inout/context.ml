(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  val pretty_internal: Format.formatter -> Cil_types.kernel_function -> unit
  val pretty_external_with_formals: Format.formatter -> Cil_types.kernel_function -> unit
end
