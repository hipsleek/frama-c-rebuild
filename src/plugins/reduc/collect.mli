(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type 'a alarm_component = Emitter.t ->
  kernel_function ->
  stmt ->
  rank:int -> Alarms.alarm -> code_annotation -> 'a -> 'a

type env

type annoth = AnnotAll | AnnotInout

val empty_env: annoth -> env

val get_relevant: env alarm_component (* Set(loc) * Set(exp) ? *)

val should_annotate_stmt: env -> stmt -> bool
val get_relevant_vars_stmt: env -> kernel_function -> stmt -> lval list
