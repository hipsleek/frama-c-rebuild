(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module provides pretty printing utilities. Same as {!Pretty_utils}
    but without dependencies to {!Fclib.List} or {!Fclib.Array}.
    @since 33.0-Arsenic *)

(** Formatter for ["%a"] format *)
type 'a aformatter = Format.formatter -> 'a -> unit

(** Formatter for ["%t"] format *)
type tformatter = Format.formatter -> unit

(** Formats used with [fprintf] and alike. *)
type nonrec 'a format = ('a,Format.formatter,unit) format

(** Pretty prints a sequence.
    @param format defines the format used to print the collection, e.g. "@[%t@]"
    @param item defines the format for an item of the collection, e.g. "@[%a@]"
    @param sep defines the format for the separator between items, e.g "@;,"
    @param last defines the format for the last separator, defaults to [sep]
    @param empty defines the format for empty sequences, defaults to [format] *)
val pretty_seq:
  format:(tformatter -> unit) format ->
  item:('a aformatter -> 'a -> unit) format ->
  sep:unit format ->
  ?last:unit format ->
  ?empty:unit format ->
  'a aformatter -> ('a Seq.t) aformatter
