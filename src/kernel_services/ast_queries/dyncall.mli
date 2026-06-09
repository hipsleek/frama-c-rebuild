(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val pp_calls : Format.formatter -> kernel_function list -> unit

val get : ?bhv:string -> stmt -> (Property.t * kernel_function list) option
(** Returns [None] if there is no specified dynamic call. *)

val compute : unit -> unit
(** Forces computation of dynamic calls.
    Otherwise, they are computed lazily on [get].
    Requires [-wp-dynamic]. *)
