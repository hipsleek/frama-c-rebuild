(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** {2 Text enriched with semantic tags}                                      *)
(* -------------------------------------------------------------------------- *)

(** This module provides a rich text type that represents a string of characters
    enriched by semantic tags. It is intended to provide a way to store texts
    that can be output in several format, for instance in html or using ANSI
    sequences in terminals. When the text is pretty printed, the semantic
    tags are output in the usual way on the formatter (which can translate the
    semantic tags to html markup for instance). See {!Format} for more details
    about semantic tags.

    @before 32.0-Germanium the buffer functions were used to build plain strings
    instead of rich text *)

type t (** Text with tags *)

(** [empty] is the empty text, containing neither plain text nor tags. *)
val empty : t

(** [is_empty text] returns true if [text] is empty. A rich text containing
    only semantic tags is not considered empty. *)
val is_empty : t -> bool

(** [of_string s] returns a plain text equal to [s] with no semantic tags. *)
val of_string : string -> t

(** [length text] returns the number of characters in the text. *)
val size : t -> int

(** [plain text] returns the plain string in the text (without any tag). *)
val plain : t -> string

(** [contains text c] returns whether [text] contains the character [c]. *)
val contains : t -> char -> bool

(** [index text c] finds the first index of character [c].
    @raises Not_found if not found *)
val index : t -> char -> int

(** [truncate ~start_pos ~end_pos text] truncate the text [text] to the range
    from [start_pos] (included, default to 0) to [end_pos] (excluded, default
    to the text size). All tags outside this range are removed. *)
val sub : ?start_pos:int -> ?end_pos:int -> t -> t

(** Indicates the total available space and the position of the truncation *)
type truncation = [ `None | `Left of int | `Middle of int | `Right of int ]

(** [pretty fmt text] pretty-prints the text onto the given formatter
    [fmt], with the semantic tags.
    The original text has been {i already} laid out with respect to
    horizontal and vertical boxes, and this layout will be output as-it-is
    into the formatter.
    @param truncate defines the maximum size of the printed text and the
    position of the truncation if the text exceed this size
    @param ellipsis when [truncate] is given and the text length is bigger than
    the specified size, then [ellipsis] is printed instead of the truncated
    part. *)
val pretty :
  ?truncate:truncation ->
  ?ellipsis:string ->
  Format.formatter ->
  t ->
  unit

(** Pretty prints the text into a string
    @param prefix a pretty printing function called at the beginning of the
    print
    @param suffix a pretty printing function called at the end of the print
    @param truncate defines the maximum size of the string and the position of
    the truncation if the text exceed this size
    @param ellipsis when [truncate] is given and the text length is bigger than
    the specified size, then [ellipsis] is printed instead of the truncated
    part. *)
val to_string :
  ?prefix:(Format.formatter -> unit) ->
  ?suffix:(Format.formatter -> unit) ->
  ?truncate:truncation ->
  ?ellipsis:string ->
  t -> string

(** [need_truncation ?truncate text] returns whether {!to_string}, {!pretty} or
    {!sprintf} will truncate the text. *)
val need_truncation : ?truncate:truncation -> t -> bool

(* -------------------------------------------------------------------------- *)
(** {2 Buffers for building rich text}                                        *)
(* -------------------------------------------------------------------------- *)

(** Buffer for creating rich text.

    The buffer grows on demand, but is protected against huge messages.
    Maximal size is around 2 billions ASCII characters, which should be enough
    to store more than 25kloc source text. *)
type buffer

module Buffer :
sig
  (** Create a buffer.

      The right-margin is set to [~margin] and
      maximum indentation to [~indent].
      Default values are those of [Format.make_formatter], which are
      [~indent:68] and [~margin:78] in OCaml 4.05.
  *)
  val create : ?indent:int -> ?margin:int -> unit -> buffer

  (** Reset the buffer to its initial empty state. *)
  val reset : buffer -> unit

  (** Buffer contents, with its semantic tags.
      @param trim if set to true, remove leading and trailing whitespaces
      (including tabulations, line feed and carriage returns) *)
  val contents : ?trim:bool -> buffer -> t

  val add_char : buffer -> char -> unit (** Buffer-like *)
  val add_string : buffer -> string -> unit (** Buffer-like *)
  val add_substring : buffer -> string -> int -> int -> unit (** Buffer-like *)

  (** Pretty printing into the buffer. Similar to {!Format.fprintf}. *)
  val bprintf : buffer -> ('a,Format.formatter,unit) format -> 'a

  (** Same as [bprintf] above, but instead of returning immediately,
      passes a formatter to the continuation given as first argument at the end
      of printing. *)
  val kbprintf :
    (Format.formatter -> 'a) ->
    buffer ->
    ('b,Format.formatter,unit,'a) format4 ->
    'b
end


(* -------------------------------------------------------------------------- *)
(** {2 Direct formatting}                                                     *)
(* -------------------------------------------------------------------------- *)

(** Pretty prints to a string.
    @param indent defines the maximum indentation as in {!create}, defaults
    to 20
    @param margin defines the right-margin as in {!create}, defaults to 40
    @param trim if set to true, remove leading and trailing whitespace
    (including tabulations, line feed and carriage returns)
    @param truncate defines the maximum size of the printed text and the
    position of the truncation if the (trimmed) text exceed this size
    @param ellipsis when [truncate] is given and the (trimmed) text length is
    bigger than the specified size, then [ellipsis] is printed instead of the
    truncated part. *)
val sprintf  :
  ?indent:int ->
  ?margin:int ->
  ?trim:bool ->
  ?truncate:truncation ->
  ?ellipsis:string ->
  ('a, Format.formatter,unit,string) format4 ->
  'a

(** Pretty prints to a rich text.
    @param indent defines the maximum indentation as in {!create}
    @param margin defines the right-margin as in {!create}
    @param trim if set to true, remove leading and trailing whitespace
    (including tabulations, line feed and carriage returns) *)
val mprintf  :
  ?indent:int ->
  ?margin:int ->
  ?trim:bool ->
  ('a, Format.formatter,unit,t) format4 ->
  'a
