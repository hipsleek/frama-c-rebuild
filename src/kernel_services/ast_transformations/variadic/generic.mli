(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* The vpar formal parameter *)
val vpar : string * Cil_types.typ * Cil_types.attributes

(* Shallow translation of variadic types *)
val translate_type : Cil_types.typ -> Cil_types.typ

(* Adds the vpar parameter to variadic functions *)
val add_vpar : Cil_types.varinfo -> unit

(* Translation of va_* builtins *)
val translate_va_builtin :
  Cil_types.fundec -> Cil_types.instr -> Cil_types.instr list

(* Generic translation of calls *)
val translate_call :
  builder:Builder.t -> Cil_types.lhost -> Cil_types.exp list -> unit
