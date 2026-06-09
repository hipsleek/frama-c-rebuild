(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

type t_zones = Memory_zone.t Stmt.Hashtbl.t
val build_zones :
  kernel_function -> stmt -> lval -> Stmt.Hptset.t * t_zones
val pretty_zones : Format.formatter -> t_zones -> unit
val get_zones : t_zones ->  Cil_types.stmt -> Memory_zone.t
