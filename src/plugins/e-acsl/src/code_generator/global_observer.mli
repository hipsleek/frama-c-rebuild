(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Observation of global variables. *)

open Cil_types

val function_init_name: string
(** Name of the function in which [mk_init_function] (see below) generates the
    code. *)

val function_clean_name: string
(** Name of the function in which [mk_clean_function] (see below) generates the
    code. *)

val reset: unit -> unit
val is_empty: unit -> bool

val add: varinfo -> unit
(** Observe the given variable if necessary. *)

val add_initializer: varinfo -> offset -> init -> unit
(** Add the initializer for the given observed variable. *)

val mk_init_function: unit -> varinfo * fundec
(** Generate a new C function containing the observers for global variable
    declarations and initializations. *)

val mk_clean_function: unit -> (varinfo * fundec) option
(** Generate a new C function containing the observers for global variable
    de-allocations if there are global variables. *)
