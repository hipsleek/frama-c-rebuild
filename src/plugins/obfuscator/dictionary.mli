(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val fresh: Obfuscator_kind.t -> string -> string
(** Generate a fresh name of the given kind *)

val id_of_string_literal: string -> string
(** @return the generated name for a literal string.
    @raise Not_found if no name has already been generated. *)

val pretty_kind: Format.formatter -> Obfuscator_kind.t -> unit
val pretty: Format.formatter -> unit

val mark_as_computed: unit -> unit
val is_computed: unit -> bool
