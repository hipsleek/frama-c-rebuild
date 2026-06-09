(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Float kinds}
    @since 33.0-Arsenic
*)
type fkind =
  | FFloat      (** [float] *)
  | FFloat32    (** [binary32] *)
  | FFloat64    (** [binary64] *)
  | FDouble     (** [double] *)
  | FLongDouble (** [long double] *)

(** {2 pretty modes} *)

(** Determine how floats will be printed.
    @since 32.0-Germanium *)
type float_display =
  | Default (* Printed normally *)
  | NormDec (* Normalized in decimals *)
  | NormHex (* Normalized in hexadecimals *)

(** Change the printer mode for floats.
    @since 32.0-Germanium *)
val set_float_display : float_display -> unit

(** {2 Rounding modes} *)

type rounding =
  | Nearest_even (** Rounding to nearest, tie to even. *)
  | Upward       (** Rounding toward +∞. *)
  | Downward     (** Rounding toward -∞. *)
  | Toward_zero  (** Rounding toward zero. *)

(** Set the current rounding mode. *)
val set_rounding_mode : rounding -> unit

(** Get the current rounding mode. *)
val get_rounding_mode : unit -> rounding

(** Rounds the given float to a single precision float. *)
val round_to_single_precision : float -> float

(** Rounds the given float to a single precision float if
    [fkind = FFloat] or [fkind = FFloat32]. *)
val round_if_single_precision : fkind -> float -> float


(** {2 Floating-point operations} *)

type truncated_to_integer =
  | Integer of Z.t (** The given float has been successfully truncated. *)
  | Underflow      (** The given float would underflow if truncated. *)
  | Overflow       (** The given float would overflow if truncated. *)

(** Truncates a given floating point number, i.e returns the closest
    integer rounded toward zero, regardless of the current rounding mode.
    The [truncate_to_integer] function converts the given number into an
    64 bits integer it it fits or a token specifying if the conversion is
    impossible due to an underflow or an overflow. *)
val truncate_to_integer : float -> truncated_to_integer


(** True if the given float is finite (not infinite or a NaN). *)
val is_finite : float -> bool

(** True if the given float is infinite (but not a NaN). *)
val is_infinite : float -> bool

(** True if the given float is a NaN. *)
val is_nan : float -> bool

(** {2 Pretty printers}

    The function [pretty_normal] implements a custom printer. The function
    [pretty] relies on internal state {!float_display} to decide between using
    the OCaml printer or the [pretty_normal] one. This mode can be changed
    using {!set_float_display}. *)

val pretty_normal : use_hex:bool -> Format.formatter -> float -> unit
val pretty : Format.formatter -> float -> unit

(** True if the given string's suffix corresponds to the given
    format, i.e "F" for single precision (or "F32" for _Float32),
    "D" (or "F64") for double precision (_Float64), and
    "L" for extended precision.

    @before 33.0-Arsenic "f32" and "f64" were not accepted.
*)
val has_suffix : fkind -> string -> bool

(** [extract_suffix s] tries to find a floating-point suffix
    ("f", "F", "d", "l", "f32", etc) in [s] and returns
    [Some (s_without_suffix, suffix, fkind)]. If [s] has no suffix,
    returns [Some (s, "", FDouble)]. If the suffix is unknown, returns
    [None].

    @since 33.0-Arsenic
*)
val extract_suffix : string -> (string * string * fkind) option

(** {2 Format based constants} *)

type format = Single | Double

(** The significant size of a given format is denoted {m m}.
    The exponent size of a given format is denoted {m e}. *)

(** Returns the largest positive finite floating point number of a given format.
    It is computed as :
    {math \left({2 - 2 ^ {1 - m}}\right) ^ {2 ^ {e - 1} - 1}} *)
val largest_finite_float_of : format-> float

(** Returns the bounds of the finite range of a given format, i.e the largest
    negative finite floating point number and the largest positive finite
    floating point number of a given format. *)
val finite_range_of : format -> float * float

(** Returns the smallest positive normalized floating point number of a given
    format. It is computed as :
    {math  {2} ^ {2 - {2} ^ {e - 1}} } *)
val smallest_normal_float_of : format -> float

(** Returns the smallest positive denormalized floating point number of a
    given format. It is simply computed by setting the least significant bit
    to one, as this bit corresponds to the last bit of the significant. *)
val smallest_denormal_float_of : format -> float

(** Returns the unit in the last place of a given format, i.e the value of
    the least significant bit of a floating point number with an exponent
    set at zero. It is primally used to overapproximate rounding errors.
    It is computed as {m 2 ^ {-m}}. *)
val unit_in_the_last_place_of : format -> float

val minimal_exponent_of : format -> int
val maximal_exponent_of : format -> int
