(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Objectives and limitations}

    The goal of this module is to provide a representation of floating-point
    numbers that statically encode the format of the represented number. As
    for now, the numbers are represented using the OCaml [float] type (which
    are in the [binary64] format) and thus, the [Long] format (or [binary80])
    is not yet supported.

    The main feature of this module currently used by Frama-C is the [parse]
    function, which returns a witness that specifies the format one would like
    to use and the one actually used to encode the parsed floating point number.

    @since 31.0-Gallium *)



(** {2 Floating point formats}

    Supported floating point formats are declared at the type level to
    enforce a strict manipulation of floating point numbers. *)

type single = Format_Single
type float32 = Format_Float32
type float64 = Format_Float64
type double = Format_Double
type long   = Format_Long

type 'f format =
  | Single : single format (** 32-bits single precision IEEE-754 format. *)
  | Float32 : float32 format (** C23's _Float32 (identical to single) *)
  | Float64 : float64 format (** C23's _Float64 (identical to double) *)
  | Double : double format (** 64-bits double precision IEEE-754 format. *)

(** Returns the [fkind] corresponding to the given format. *)
val fkind_of_format : 'f format -> Floating_point.fkind

(** Pretty printer for floating point formats. *)
val pretty_format : 'f format Pretty_utils.formatter

(** Type used to return a proof that two formats are in fact the same. *)
type ('l, 'r) same_format =
  | No  : ('l, 'r) same_format
  | Yes : 'f format -> ('f, 'f) same_format

(** Check if two formats are in fact the same. *)
val same_format : 'l format -> 'r format -> ('l, 'r) same_format

(** Compare formats. *)
val compare_format : 'l format -> 'r format -> int

(** Hash of a format. *)
val hash_format : 'f format -> int



(** {2 Typed floating point numbers} *)

(** Type of statically formatted floating point numbers. The parameter
    specifies the number's format. *)
type 'f t

(** Returns a typed single precision float, performing rounding if needed *)
val single : float -> single t

(** Returns a typed double precision float. *)
val double : float -> double t

(** Build a typed float of the given format given an OCaml 64-bits
    floating point number. *)
val represents : float:float -> in_format:'f format -> 'f t

(** Returns the OCaml float represented by a given typed float. *)
val to_float : 'f t -> float

(** Returns the format of a given typed float. *)
val format : 'f t -> 'f format

(** True if the given typed float is finite (not infinite or a NaN). *)
val is_finite : 'f t -> bool

(** True if the given typed float is infinite (but not a NaN). *)
val is_infinite : 'f t -> bool

(** True if the given typed float is a NaN. *)
val is_nan : 'f t -> bool

(** Cast a typed floating point number to a new given format.
    Rounding operations are performed when needed. *)
val change_format : _ t -> 'f format -> 'f t

(** [equal f1 f2] returns true if [f1] and [f2] have the same format and are
    equal, false otherwise. *)
val equal : 'l t -> 'r t -> bool

(** Total order on two typed floats with possibly different formats. *)
val compare : 'l t -> 'r t -> int

(** Hash of a typed float. *)
val hash : 'f t -> int



(** {2 Pretty printers}

    The function [pretty_normal] implements a custom printer. The function
    [pretty] relies on internal state {!Floating_point.float_display} to decide
    between using the OCaml printer or the [pretty_normal] one. This mode can be
    changed using {!Floating_point.set_float_display}. *)

val pretty_normal : use_hex:bool -> Format.formatter -> 'f t -> unit
val pretty : Format.formatter -> 'f t -> unit



(** {2 Parser}

    Parses a typed float from a string. As the represented float may not be
    exact, the parser actually returns three typed floats. The [lower] one
    corresponds to the floating point number computed using the [Downward]
    rounding mode. The [upper] one is based on the [Upward] rounding mode.
    Finally, the [nearest] corresponds to the [Nearest_even] rounding mode.
    As each one of them is typed, the parser must actually returns an
    existantially quantified record, using the [parsed] wrapper type. *)

type 'f parsed_float =
  { lower   : 'f t
  ; nearest : 'f t
  ; upper   : 'f t
  ; format  : 'f format
  }

(** A witness encoding the floating-point format represented by the string,
    and the format actually used to parse it. For now, Long format is not
    supported by this module, and is thus approximated using double precision. *)
type ('format, 'encoded_as) parsed_format =
  | Single_supported  : (single,   single) parsed_format
  | Float32_supported : (float32, float32) parsed_format
  | Float64_supported : (float64, float64) parsed_format
  | Double_supported  : (double,   double) parsed_format
  | Long_unsupported  : (long  ,   double) parsed_format

type parsed_result =
  | Parsed : ('f, 'k) parsed_format * 'k parsed_float -> parsed_result

(** Parses the given string and returns [Ok Parsed (kind, float)] with the
    parsed [float] and its [kind] (single, double or long) according to its
    suffix, if any. Strings with no suffix are parsed as double. Returns
    [Error msg] if the parsing fails, where [msg] is the error message. *)
val parse : string -> (parsed_result, string) result

(** Calls {!parse} and evaluates the result type.
    @raise Failure if the parsing fails.
    @before 33.0-Arsenic emitted a {!Kernel.failure} instead
*)
val parse_exn : string -> parsed_result
[@@deprecated "Use parse function instead and handle the error if any."]

(** Returns the floating-point kind parsed by [parse]. *)
val parsed_fkind : ('k, 'f) parsed_format -> Floating_point.fkind



(** {2 Format based constants}

    See documentation in [Floating_point]. *)

val largest_finite_float_of : format:'f format -> 'f t
val finite_range_of : format:'f format -> 'f t * 'f t
val smallest_normal_float_of : format:'f format -> 'f t
val smallest_denormal_float_of : format:'f format -> 'f t
val unit_in_the_last_place_of : format:'f format -> 'f t



(** {2 Comparisons} *)

val ( =  ) : 'f t -> 'f t -> bool
val ( <> ) : 'f t -> 'f t -> bool
val ( <  ) : 'f t -> 'f t -> bool
val ( <= ) : 'f t -> 'f t -> bool
val ( >  ) : 'f t -> 'f t -> bool
val ( >= ) : 'f t -> 'f t -> bool



(** {2 Correctly rounded arithmetic operations}

    Correctly rounded arithmetic operations. As stated by the IEEE-754 norm,
    computing a correctly rounded operation is exactly equivalent to first
    compute the operation in real arithmetic over the operands and then
    perform a rounding into the floating point format. *)

val neg   : 'f t -> 'f t
val sqrt  : 'f t -> 'f t
val ( + ) : 'f t -> 'f t -> 'f t
val ( - ) : 'f t -> 'f t -> 'f t
val ( * ) : 'f t -> 'f t -> 'f t
val ( / ) : 'f t -> 'f t -> 'f t
val ( mod ) : 'f t -> 'f t -> 'f t



(** {2 Mathematic functions over floating point numbers}

    None of those functions are correctly rounded. They behave exactly as their
    libc equivalents. For each format, the corresponding libc function is
    actually called. For example, computing the exponential of a floating point
    number in single precision uses the libc [expf] function. *)

val exp   : 'f t -> 'f t
val log   : 'f t -> 'f t
val log10 : 'f t -> 'f t
val pow   : 'f t -> 'f t -> 'f t
val fmod  : 'f t -> 'f t -> 'f t
val cos   : 'f t -> 'f t
val sin   : 'f t -> 'f t
val tan   : 'f t -> 'f t
val acos  : 'f t -> 'f t
val asin  : 'f t -> 'f t
val atan  : 'f t -> 'f t
val atan2 : 'f t -> 'f t -> 'f t



(** {2 Floating point numbers as integers} *)

(** Truncate a given typed floating point number, i.e returns the closest
    integer rounded toward zero, regardless of the current rounding mode.
    The [trunc] function is based on the libc equivalent and never fails. *)
val trunc : 'f t -> 'f t

(** Rounds a given typed floating point number to the closest integer. Ties
    are rounded away from zero, regardless of the current rounding mode. *)
val round : 'f t -> 'f t

(** Returns the bits encoding of a given typed floating point number,
    represented as a {!Z.t}. *)
val bits_encoding : 'f t -> Z.t
