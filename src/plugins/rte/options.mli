(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

module Enabled: Parameter_sig.Bool

module DoShift : Parameter_sig.Bool
module DoDivMod : Parameter_sig.Bool
module DoFloatToInt : Parameter_sig.Bool
module DoInitialized : Parameter_sig.Kernel_function_set
module DoMemAccess : Parameter_sig.Bool
module DoPointerCall : Parameter_sig.Bool

module Trivial : Parameter_sig.Bool
module Warn : Parameter_sig.Bool
module FunctionSelection: Parameter_sig.Kernel_function_set

val use_eva_results: unit -> bool

val dkey_annot: category
