(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Security slicing. *)

open Cil_types

val get_direct_component: stmt -> stmt list
val get_indirect_backward_component: stmt -> stmt list
val get_forward_component: stmt -> stmt list
val impact_analysis: Kernel_function.t -> stmt -> stmt list
(*
val slice: bool -> Project.t
*)
