(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module handle positions in a source file. [Filepos.t] is a Frama-C
    datatype, and comes with usual [compare], [equal], [hash] and [pretty]
    functions.
    - [compare] orders location first by file path, then by offset
      in the file. This means the position order will be compatible with the
      apparition order in each file. If no offset information is available in
      both compared positions, the line then column will be used.
    - [hash] only hashes the path and the line of the position.

    @before 33.0-Arsenic This module was split between {!Filepath} and
    {!Cil_datatype.Position}.
*)

type t [@@deriving show]

type origin =
  | Unknown
  (** Unknown position. This constructor should be avoided. *)
  | Original
  (** The position is in one of the user input files. *)
  | Generated of string
  (** The position is in generated input. The string is a name identifying the
      generator. *)
  | Preprocessed of t
  (** The position is in file that have been produced from a preprocessing at
      the given position. *)
  | Included of t
  (** The position is in a file included from the given position *)
[@@deriving show]

include Datatype.S_with_collections with type t := t


(** {2 Pretty printing } *)

(** Pretty prints a position in the format [<file>:<line>] or, if the column
    number is available, in the format [<file>:<line>:<char>]. *)
val pretty : Format.formatter -> t -> unit

(** Pretty prints a position in the format ["<file>", line <line>] or, if the
    column number is available, in the format
    ["<file>", line <line>, character <char>] *)
val pretty_long : Format.formatter -> t -> unit

(** Debug printer. Prints the internal representation of locations. *)
val pretty_debug : Format.formatter -> t -> unit


(** {2 Construction } *)

(** Make a new position. The default for [origin] is [Original]. *)
val make :
  ?path:Filepath.t ->
  ?offset:int ->
  ?line:int ->
  ?column:int ->
  ?origin:origin ->
  unit -> t


(** {2 Special positions } *)

(** Make a new position for a generated input. The generator name is given
    as a string. The position is memoized such that two position for the same
    generator are necessarily physically equal. *)
val generated : string -> t

(** Special representation of an unknown position. *)
val unknown : t

(** Return true if the given position is neither unkwnown nor generated. *)
val is_known : t -> bool


(** {2 Conversion from/to Lexing.position } *)

(** Convert a [Lexing.position] to a [Filepos.t]. *)
val of_lexing_pos : ?origin:origin -> Lexing.position -> t

(** Convert a [Filepos.t] to a [Lexing.position] *)
val to_lexing_pos : t -> Lexing.position


(** {2 Position tracking } *)

(** Update the current line of the position. Tries to keep track the file
    inclusions; recursive inclusion is unsupported. *)
val update_line : ?path:Filepath.t -> line:int -> t -> t

(** Increment the line number in the position. *)
val incr_line : t -> t

(** Update the column of the position. *)
val update_column : column:int -> t -> t


(** {2 Accessors } *)

(** Get the original position. If the position is in a preprocessed code,
    returns the source of the preprocessing otherwise this function is the
    identity. *)
val original : t -> t

(** Get the path of a position. If the position is in a preprocessed code,
    returns the original file. *)
val path : t -> Filepath.t

(** Get the line of a position, starting at 1. If the position is in a
    preprocessed code, returns the line in the original file. *)
val line : t -> int

(** Get the column of the position, starting at 1. If the position is in a
    preprocessed code and as Frama-C cannot track the column in the original
    file, this function will likely return 0. *)
val column : t -> int

(** Get the origin of a position. *)
val origin : t -> origin

(** Get the path of the input file. Unlike {!path}, if the position is in a
    preprocessed code, it returns the preprocessed output path. *)
val input_path : t -> Filepath.t

(** Get the line in the input file, starting at 1. Unlike {!line}, if the
    position is in a preprocessed code, it returns the line in the
    preprocessed output. *)
val input_line : t -> int

(** Get the column in the input file, starting at 1. Unlike {!column}, if the
    position is in a preprocessed code, it returns the column in the
    preprocessed output. *)
val input_column : t -> int

(** Get the offset in the input file, starting at 0. Unlike {!offset}, if the
    position is in a preprocessed code, it returns the offset in the
    preprocessed output. *)
val input_offset : t -> int

(** Returns whether the location is an preprocessed file. If [true] is returned
    then {!original} will likely not be the identity {!path} and {!line} will
    likely to return different results than {!input_path} and {!input_line}. *)
val is_preprocessed : t -> bool

(** Returns the list of inclusion positions when the position is in preprocessed
    code; returns nothing if the position is not in an included file. *)
val inclusions : t -> t list


(** {2 Datatype with comparison/hash on original source positions} *)

(** This module provides an alternative datatype where only original positions
    are considered for [compare], [equal] and [hash]. This is intended for
    preprocessed code where the same file can be included several times leading
    to tokens having different position in the preprocessing output but the
    same original position.

    The comparison, equality and hash functions only
    consider the path and the line, since the original column is usually not
    available. *)
module Original : Datatype.S_with_collections with type t = t
