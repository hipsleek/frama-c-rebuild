(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Typ : sig
  val params : typ -> (string * typ * attributes) list
  val ghost_partitioned_params : typ ->
    (string * typ * attributes) list *
    (string * typ * attributes) list
  val params_types : typ -> typ list
  val params_count : typ -> int
end
