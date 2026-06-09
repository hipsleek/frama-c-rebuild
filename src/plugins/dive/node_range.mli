(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type t = Dive_types.node_range

val evaluate : Cvalue.V.t -> Cil_types.typ -> t
val upper_bound : t -> t -> t
