(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

module type Varinfo_set = Parameter_sig.Set
  with type elt = Cil_types.varinfo
   and type t = Cil_datatype.Varinfo.Set.t

module OutputDot : Parameter_sig.Filepath
module OutputJson : Parameter_sig.Filepath
module DepthLimit : Parameter_sig.Int
module FromFunctionAlarms : Parameter_sig.Kernel_function_set
module FromBases : Varinfo_set
module UnfoldedBases : Varinfo_set
module HiddenBases : Varinfo_set
