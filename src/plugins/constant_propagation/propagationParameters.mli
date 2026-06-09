(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module SemanticConstFolding: Parameter_sig.Bool
module SemanticConstFold: Parameter_sig.Fundec_set
module CastIntro: Parameter_sig.Bool
module ExpandLogicContext: Parameter_sig.Bool
module Project_name: Parameter_sig.String

include Log.Messages
