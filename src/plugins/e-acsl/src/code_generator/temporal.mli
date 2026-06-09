(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Transformations to detect temporal memory errors (e.g., dereference of
    stale pointers). *)

open Cil_types

(* [TODO ARCHI]: change the call convention in this module *)

val enable: bool -> unit
(** Enable/disable temporal transformations *)

val handle_function_parameters: kernel_function -> Env.t -> Env.t
(** [handle_function_parameters kf env] updates the local environment [env],
    according to the parameters of [kf], with statements allowing to track
    referent numbers across function calls. *)

val handle_stmt: stmt -> Env.t -> kernel_function -> Env.t
(** Update local environment ([Env.t]) with statements tracking temporal
    properties of memory blocks *)

val generate_global_init: varinfo -> offset -> init -> stmt option
(** Generate [Some s], where [s] is a statement tracking global initializer
    or [None] if there is no need to track it *)
