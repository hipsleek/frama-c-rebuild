(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This modules provides utilities to use semantic tags to output color
    and style information on capable terminals.

    Standard tags can be used in format strings as in the following example.

    {[
      Format.printf "@{<bold>Bold Text} @{<red>Red Text}"
    ]}

    The name [s] of the string tags inside ["@{<s>}"] should match the
    corresponding style or color constructor. The comparison is
    case-insensitive. For colors, the color name may be prefixed by an optional
    "fg" for foreground color and by "bg" for background colors. Multiple tags
    can be given at once by separating them with a comma.

    {[
      Format.printf "@{<red,bold>Red & Bold Text}"
    ]}

    Alternatively, style tags may be output using the new [Style_tag] :

    {[
      Format.open_stag (Style_tag (Color Red))
    ]}

    For both versions, the semantic tags handlers have to be activated using
    the [enable] or [enable_on] functions below.

    See {!Format.stag} for details about semantic tags.
    @since 32.0-Germanium *)

(** [is_supported ()] returns whether the current terminal supports ansi
    escape sequence, i.e. if it exports a [TERM] environment
    variable that is not assigned "DUMB" *)
val is_supported : unit -> bool

(** Enable the style output on the given formatter. No support test is
    performed.
    @param fallback is set to [true], unhandled styles are delegated
    to the underlying formatter. Default is [false].
    @return a reset function that can be called to reset styles. *)
val enable_on : ?fallback:bool -> Format.formatter -> (unit -> unit)

(** Output colors. The associated string semantic tag is documented for each
    constructor. Note that there exists variants prefixed with "fg:" and "bg:"
    for each colors, for foreground and background. When no prefix is used,
    it means the foreground color. *)
type color =
  | Black   (** ["black"] *)
  | Red     (** ["red"]   *)
  | Green   (** ["green"] *)
  | Yellow  (** ["yellow"] *)
  | Blue    (** ["blue"] *)
  | Magenta (** ["magenta"] *)
  | Cyan    (** ["cyan"] *)
  | White   (** ["white"] *)
  | Orange  (** ["orange"] *)

(** Output Styles. The associated string semantic tag is documented for each
    constructor. *)
type style =
  | Bold                (** ["bold"] *)
  | Faint               (** ["faint"] *)
  | Italic              (** ["italic"] *)
  | Underline           (** ["underline"] *)
  | Blink               (** ["blink"] *)
  | Strike              (** ["strike"] *)
  | Foreground of color (** ["fg:xxxx"] where ["xxxx"] is the color tag *)
  | Background of color (** ["bg:xxxx"] where ["xxxx"] is the color tag *)

(** Associates a style to format tag ["@{<tag>"]. *)
val add_style : string -> style -> unit

(** Find the style associated to format tag ["@{<tag>"], if any.
    @raises Not_found *)
val find_style : string -> style

(** Remove a style. Previous style definition is restored, if any. *)
val remove_style : string -> unit

(** Reset styles to predefined ones.
    Removes {i all} the previously added styles. *)
val reset_styles : unit -> unit

(** Extension of semantic tags for style information *)
type Format.stag += Style_tag of style
