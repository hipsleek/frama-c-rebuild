(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Analyses_types
open Analyses_datatype

val widen : ?arg:bool -> logic_info -> ival -> ival -> ival
val widen_profile : logic_info -> Profile.t -> Profile.t -> Profile.t
