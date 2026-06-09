(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val get_defs :
  Kernel_function.t -> Cil_datatype.Stmt.t -> Cil_datatype.Lval.t ->
  (Cil_datatype.Stmt.Hptset.t * Memory_zone.t option) option

val get_defs_with_type :
  Kernel_function.t -> Cil_datatype.Stmt.t -> Cil_datatype.Lval.t ->
  ((bool * bool) Cil_datatype.Stmt.Map.t * Memory_zone.t option) option


(* internal use *)
val compute_with_def_type_zone:
  Cil_types.kernel_function -> Cil_types.stmt -> Memory_zone.t ->
  ((bool * bool) Cil_datatype.Stmt.Map.t * Memory_zone.t option) option
(** This function is similar to {!get_defs_with_type}, except
    that it receives a zone as argument, instead of an l-value *)
