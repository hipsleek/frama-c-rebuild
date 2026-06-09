(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Extension of OCaml's {!Stdlib.String} module.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since 33.0-Arsenic
*)

include module type of Stdlib.String

(** Case-insensitive string comparison. Only ISO-8859-1 accents are handled. *)
val compare_ignore_case: string -> string -> int

(** Same as {!String.hash} but made available here until the minimal supported
    version of OCaml is 5.0. *)
val hash: string -> int

(** [remove_prefix ~strict prefix s] returns [None] if [prefix] is not a prefix
    of [s] and [Some s'] iff [s=prefix^s']. *)
val remove_prefix: ?strict:bool -> string -> string -> string option

(** [remove_suffix ~strict suffix s] returns [None] if [suffix] is not a suffix
    of [s] and [Some s'] iff [s=s'^suffix]. *)
val remove_suffix: ?strict:bool -> string -> string -> string option

(** Same as {!String.length} but counts utf8 characters instead of bytes. *)
val utf8_length: string -> int

(** Same as {!String.escaped} but for utf8 encoded strings. Unicode non ASCII
    characters are preserved unescaped. *)
val utf8_escaped: string -> string

(** remove underscores at the beginning and end of a string. If a string
    is composed solely of underscores, return the empty string *)
val trim_underscores: string -> string

(** Escape string for use in HTML tag. *)
val html_escape: string -> string

(** [percent_encode s] returns the string [s] encoded so that it can be used
    as a path component in a HTML URL. All characters not on the list of
    unreserved characters in RFC3986 are percent-encoded. For instance the space
    character is converted to [%20].

    Cf. {{:https://datatracker.ietf.org/doc/html/rfc3986#section-2.3}} for the
    list of unreserved characters. *)
val percent_encode: string -> string

(** Return [true] if the string is ["yes"], ["true"] or ["1"] (ignore case).
    @since 33.0-Arsenic
*)
val means_yes: string -> bool
