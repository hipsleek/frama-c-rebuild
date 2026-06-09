(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Dive_types
open Context

val add_lval : t -> stmt -> lval -> node
val add_var : t -> varinfo -> node
val add_alarm : t -> stmt -> Alarms.alarm -> node
val add_annotation : t -> stmt -> code_annotation -> node option
val add_stmt : t -> stmt -> node option
val add_property : t -> Property.t -> node option
val add_localizable : t -> Printer_tag.localizable -> node option

val explore_forward : depth:int -> t -> node -> unit
val explore_backward : depth:int -> t -> node -> unit

val show : t -> node -> unit
val hide : t -> node -> unit

val reduce_to_horizon : t -> range -> node -> unit
