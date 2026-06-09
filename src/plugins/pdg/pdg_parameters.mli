(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

module BuildAll: Parameter_sig.Bool

module BuildFct: Parameter_sig.Kernel_function_set

module PrintBw: Parameter_sig.Bool

module DotBasename: Parameter_sig.String
