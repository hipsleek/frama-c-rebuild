(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

val get_data_scope_at_stmt :
  Kernel_function.t -> stmt -> lval ->
  Stmt.Hptset.t * (Stmt.Hptset.t * Stmt.Hptset.t)

val get_prop_scope_at_stmt :
  kernel_function -> stmt -> code_annotation ->
  Stmt.Hptset.t * code_annotation list

val check_asserts : unit -> code_annotation list

val rm_asserts : unit -> unit

(** for internal use *)
module R: Plugin.General_services
