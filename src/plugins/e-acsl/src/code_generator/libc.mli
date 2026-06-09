(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Code generation for libc functions *)

open Cil_types

val is_writing_memory: varinfo -> bool
(** @return true if the function is a libc function that writes memory. *)

val update_memory_model:
  loc:location ->
  ?result:lval ->
  Env.t ->
  kernel_function ->
  varinfo ->
  exp list ->
  lval option * Env.t
(** [update_memory_model ~loc env kf ?result caller args] generates code in
    [env] to update the memory model after executing the libc function [caller]
    with the given [args].
    @return a tuple [result_opt, env] where [result_opt] is an option with
    the lvalue for the result of the function and [env] is the updated
    environment. *)
