(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Associate literal strings to fresh varinfo. *)

open Cil_types

val reset: unit -> unit
(** Must be called to redo the analysis *)

val is_empty: unit -> bool

val add: string -> varinfo -> unit
val find: string -> varinfo

val fold: (string -> varinfo -> 'a -> 'a) -> 'a -> 'a
