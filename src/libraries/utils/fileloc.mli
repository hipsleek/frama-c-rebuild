(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module handle locations in a source file. [Fileloc.t] is a Frama-C
    datatype, and comes with usual [compare], [equal], [hash] and [pretty]
    functions.
    - [compare] first compares the starting position of the location
      then - if they are equal - compares the ending position.
    - [hash] only hashes the first position.

    @before 33.0-Arsenic This module was {!Cil_datatype.Location}.
    @since Frama-C+dev
*)

type t = Filepos.t * Filepos.t [@@deriving show]

include Datatype.S_with_collections with type t := t

(** Special representation of an unknown location. *)
val unknown : t


(** {2 Pretty printing } *)

(** Pretty prints a position in the format [<file>:<line>-<line1>-<line2>] or,
    if on one line and the column number is available, in the format
    [<file>:<line>:<char1>-<char2>]. *)
val pretty : Format.formatter -> t -> unit

(** Pretty prints a position in the format ["<file>", line <line>-<line>] or,
    if on one line and the column number is available, in the format
    ["<file>", line <line>, character <char1>-<char2>]. *)
val pretty_long : t Pretty_utils.formatter

(** Same as {!pretty_long} but also prints the list of inclusion. *)
val pretty_long_with_inclusions: t Pretty_utils.formatter

(** Pretty-prints the ocaml internal representation of a location, for debug
    purposes.

    @since 22.0-Titanium
*)
val pretty_debug: t Pretty_utils.formatter


(** {2 Conversion from/to Lexing.position } *)

(** Convert a pair of [Lexing.position] to a [Fileloc.t]. *)
val of_lexing_loc : Lexing.position * Lexing.position -> t

(** Convert a pair of [Fileloc.t] to a [Lexing.position]. *)
val to_lexing_loc : t -> Lexing.position * Lexing.position

(** [is_known loc] returns true if the location is neither unknown nor
    generated.
    @since Frama-C+dev *)
val is_known : t -> bool


(** {2 Accessors } *)

(** Get the first line of the location. *)
val line : t -> int

(** Get the file path of the location. *)
val path : t -> Filepath.t


(** {2 Datatype with comparison/hash on original source positions} *)

(** This module provides an alternative datatype where only original location
    are considered for [compare], [equal] and [hash].
    See {!Filepos.Original}. *)
module Original : Datatype.S_with_collections with type t = t
