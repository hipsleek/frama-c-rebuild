(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

module Analysis: Parameter_sig.Bool
(** Whether to perform spare code detection or not. *)

module Annot : Parameter_sig.Bool
(** keep more things to keep all reachable annotations. *)

module GlobDecl : Parameter_sig.Bool
(** remove unused global types and variables *)
