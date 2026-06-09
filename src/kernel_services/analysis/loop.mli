(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Operations on (natural) loops. *)

open Cil_types

val is_natural : kernel_function -> stmt -> bool
val get_naturals : kernel_function -> stmt list Cil_datatype.Stmt.Map.t

val is_non_natural: kernel_function -> stmt -> bool
val get_non_naturals: kernel_function -> Cil_datatype.Stmt.Set.t

val back_edges : kernel_function -> stmt -> stmt list
(** Statements that are the origin of a back-edge to a natural loop. *)
