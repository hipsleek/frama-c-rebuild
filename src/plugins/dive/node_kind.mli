(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Datatype.S with type t = Dive_types.node_kind

val get_base : t -> Cil_types.varinfo option
val to_lval : t -> Cil_types.lval option
