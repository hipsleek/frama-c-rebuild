(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val name: string

include Plugin.S

module ForceAccessPath: Parameter_sig.Bool
module ForceOut: Parameter_sig.Bool
module ForceExternalOut: Parameter_sig.Bool
module ForceInput: Parameter_sig.Bool
module ForceInputWithFormals: Parameter_sig.Bool
module ForceInout: Parameter_sig.Bool
module ForceInoutExternalWithFormals: Parameter_sig.Bool
module ForceDeref: Parameter_sig.Bool

module Output: Parameter_sig.Bool
