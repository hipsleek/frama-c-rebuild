(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module is deprecated, use {!Z} instead.
    Extension of [Zarith] module {!Z}.
    @since Nitrogen-20111001 *)

type 'a formatter = Format.formatter -> 'a -> unit
[@@deprecated "Use Pretty_utils.formatter instead."]

type t = Z.t
[@@deprecated "Use Z.t instead."]

(** This type is deprecated but we want to be able to use it here. *)
[@@@alert "-deprecated"]

val equal : t -> t -> bool
[@@deprecated "Use Z.equal instead."]
[@@migrate { repl = Z.equal } ]

val compare : t -> t -> int
[@@deprecated "Use Z.compare instead."]
[@@migrate { repl = Z.compare } ]

val le : t -> t -> bool
[@@deprecated "Use Z.leq instead."]
[@@migrate { repl = Z.leq } ]

val ge : t -> t -> bool
[@@deprecated "Use Z.geq instead."]
[@@migrate { repl = Z.geq } ]

val lt : t -> t -> bool
[@@deprecated "Use Z.leq instead."]
[@@migrate { repl = Z.leq } ]

val gt : t -> t -> bool
[@@deprecated "Use Z.gt instead."]
[@@migrate { repl = Z.gt } ]

val add : t -> t -> t
[@@deprecated "Use Z.add instead."]
[@@migrate { repl = Z.add } ]

val sub : t -> t -> t
[@@deprecated "Use Z.sub instead."]
[@@migrate { repl = Z.sub } ]

val mul : t -> t -> t
[@@deprecated "Use Z.mul instead."]
[@@migrate { repl = Z.mul } ]

val shift_left : t -> t -> t
(** @raise Invalid_argument if second argument (count) is negative *)
[@@deprecated "Use Z.shift_left_z instead."]
[@@migrate { repl = Z.shift_left_z } ]

val shift_right : t -> t -> t
(** @raise Invalid_argument if second argument (count) is negative *)
[@@deprecated "Use Z.shift_right_z instead."]
[@@migrate { repl = Z.shift_right_z } ]

val shift_right_logical : t -> t -> t
(** @raise Invalid_argument if any argument is negative *)
[@@deprecated "Use Z.shift_right_logical instead."]
[@@migrate { repl = Z.shift_right_logical } ]

val logand : t -> t -> t
[@@deprecated "Use Z.logand instead."]
[@@migrate { repl = Z.logand } ]

val logor : t -> t -> t
[@@deprecated "Use Z.logor instead."]
[@@migrate { repl = Z.logor } ]

val logxor : t -> t -> t
[@@deprecated "Use Z.logxor instead."]
[@@migrate { repl = Z.logxor } ]

val lognot : t -> t
[@@deprecated "Use Z.lognot instead."]
[@@migrate { repl = Z.lognot } ]

val min : t -> t -> t
[@@deprecated "Use Z.min instead."]
[@@migrate { repl = Z.min } ]

val e_div : t -> t -> t
(** Euclidean division (that returns a positive rem).
    Implemented by [Z.ediv]

    Equivalent to C division if both operands are positive.
    Equivalent to a floored division if b > 0 (rounds downwards),
    otherwise rounds upwards.
    Note: it is possible that e_div (-a) b <> e_div a (-b).
*)
[@@deprecated "Use Z.ediv instead."]
[@@migrate { repl = Z.ediv } ]

val e_rem : t -> t -> t
[@@deprecated "Use Z.erem instead."]
[@@migrate { repl = Z.erem } ]
(** Remainder of the Euclidean division (always positive).
    Implemented by [Z.erem] *)

val e_div_rem : t -> t -> (t * t)
(** [e_div_rem a b] returns [(e_div a b, e_rem a b)].
    Implemented by [Z.ediv_rem] *)
[@@deprecated "Use Z.ediv_rem instead."]
[@@migrate { repl = Z.ediv_rem } ]

val c_div : t -> t -> t
[@@deprecated "Use Z.div instead."]
[@@migrate { repl = Z.div } ]
(** Truncated division towards 0 (like in C99).
    Implemented by [Z.div] *)

val c_rem : t -> t -> t
[@@deprecated "Use Z.rem instead."]
[@@migrate { repl = Z.rem } ]
(** Remainder of the truncated division towards 0 (like in C99).
    Implemented by [Z.rem] *)

val c_div_rem : t -> t -> t * t
[@@deprecated "Use Z.div_rem instead."]
[@@migrate { repl = Z.div_rem } ]
(** [c_div_rem a b] returns [(c_div a b, c_rem a b)].
    Implemented by [Z.div_rem] *)

val pgcd : t -> t -> t
[@@deprecated "Use Z.gcd instead."]
[@@migrate { repl = Z.gcd } ]
(** [pgcd v 0 == pgcd 0 v == abs v]. Result is always positive *)

val ppcm : t -> t -> t
[@@deprecated "Use Z.lcm instead."]
[@@migrate { repl = Z.lcm } ]
(** [ppcm v 0 == ppcm 0 v == 0]. Result is always positive *)

val cast : size:t -> signed:bool -> value:t -> t
[@@deprecated "Use Z.cast instead."]
[@@migrate { repl = Z.cast } ]

val abs : t -> t
[@@deprecated "Use Z.abs instead."]
[@@migrate { repl = Z.abs } ]

val neg : t -> t
[@@deprecated "Use Z.neg instead."]
[@@migrate { repl = Z.neg } ]

val succ : t -> t
[@@deprecated "Use Z.succ instead."]
[@@migrate { repl = Z.succ } ]

val pred : t -> t
[@@deprecated "Use Z.pred instead."]
[@@migrate { repl = Z.pred } ]

val is_zero : t -> bool
[@@deprecated "Use Z.is_zero instead."]
[@@migrate { repl = Z.is_zero } ]

val is_one : t -> bool
[@@deprecated "Use Z.is_one instead."]
[@@migrate { repl = Z.is_one } ]

val is_even : t -> bool
[@@deprecated "Use Z.is_even instead."]
[@@migrate { repl = Z.is_even } ]

val zero : t
[@@deprecated "Use Z.zero instead."]
[@@migrate { repl = Z.zero } ]

val one : t
[@@deprecated "Use Z.one instead."]
[@@migrate { repl = Z.one } ]

val two : t
[@@deprecated "Use Z.of_int 2 or ppx_z_literals '2z'."]
[@@migrate { repl = Z.of_int 2 } ]

val four : t
[@@deprecated "Use Z.of_int 4 or ppx_z_literals '4z'."]
[@@migrate { repl = Z.of_int 4 } ]

val eight : t
[@@deprecated "Use Z.of_int 8 or ppx_z_literals '8z'."]
[@@migrate { repl = Z.of_int 8 } ]

val sixteen : t
[@@deprecated "Use Z.of_int 16 or ppx_z_literals '16z'."]
[@@migrate { repl = Z.of_int 16 } ]

val thirtytwo : t
[@@deprecated "Use Z.of_int 32 or ppx_z_literals '32z'."]
[@@migrate { repl = Z.of_int 32 } ]

val onethousand : t
[@@deprecated "Use Z.of_int 1000 or ppx_z_literals '1000z'."]
[@@migrate { repl = Z.of_int 1000 } ]

val billion_one : t
[@@deprecated "Use Z.of_int 1_000_000_001 or ppx_z_literals '1_000_000_001_z'."]
[@@migrate { repl = Z.of_int 1_000_000_001 } ]

val minus_one : t
[@@deprecated "Use Z.minus_one instead."]
[@@migrate { repl = Z.minus_one } ]

val max_int64 : t
[@@deprecated "Use Z.of_int64 Int64.max_int instead."]
[@@migrate { repl = Z.of_int64 Int64.max_int } ]

val min_int64 : t
[@@deprecated "Use Z.of_int64 Int64.min_int instead."]
[@@migrate { repl = Z.of_int64 Int64.min_int } ]

val two_power_32 : t
[@@deprecated "Use Z.two_power_of_int 32 instead."]
[@@migrate { repl = Rel.two_power_of_int 32 } ]

val two_power_64 : t
[@@deprecated "Use Z.two_power_of_int 64 instead."]
[@@migrate { repl = Rel.two_power_of_int 64 } ]

val length : t -> t -> t (** b - a + 1 *)
[@@deprecated "Use Z.length instead."]
[@@migrate { repl = Z.length } ]

val of_int : int -> t
[@@deprecated "Use Z.of_int instead."]
[@@migrate { repl = Z.of_int } ]

val of_int64 : Int64.t -> t
[@@deprecated "Use Z.of_int64 instead."]
[@@migrate { repl = Z.of_int64 } ]

val of_int32 : Int32.t -> t
[@@deprecated "Use Z.of_int32 instead."]
[@@migrate { repl = Z.of_int32 } ]

(**
   @raise Z.Overflow if too big
   @since 24.0-Chromium
*)
val to_int_exn : t -> int
[@@deprecated "Use Z.to_int instead."]
[@@migrate { repl = Z.to_int } ]

(**
   @raise Z.Overflow if too big
   @since 24.0-Chromium
*)
val to_int64_exn : t -> int64
[@@deprecated "Use Z.to_int64 instead."]
[@@migrate { repl = Z.to_int64 } ]

(**
   @raise Z.Overflow if too big
   @since 24.0-Chromium
*)
val to_int32_exn : t -> int32
[@@deprecated "Use Z.to_int32 instead."]
[@@migrate { repl = Z.to_int32 } ]

(**
   Returns [Some i] if the number can be converted to an [int],
   or [None] otherwise.
   @since 24.0-Chromium
*)
val to_int_opt : t -> int option
[@@deprecated "Use Z.to_int_opt instead."]
[@@migrate { repl = Z.to_int_opt } ]

(**
   Returns [Some i] if the number can be converted to an [int64],
   or [None] otherwise.
   @since 24.0-Chromium
*)
val to_int64_opt : t -> int64 option
[@@deprecated "Use Z.to_int64_opt instead."]
[@@migrate { repl = Z.to_int64_opt } ]

(**
   Returns [Some i] if the number can be converted to an [int32],
   or [None] otherwise.
   @since 24.0-Chromium
*)
val to_int32_opt : t -> int32 option
[@@deprecated "Use Z.to_int32_opt instead."]
[@@migrate { repl = Z.to_int32_opt } ]


val to_float : t -> float
[@@deprecated "Use Z.to_float instead."]
[@@migrate { repl = Z.to_float } ]

val of_float : float -> t
[@@deprecated "Use Z.of_float instead."]
[@@migrate { repl = Z.of_float } ]

val round_up_to_r : min:t -> r:t -> modu:t -> t
(** [round_up_to_r m r modu] is the smallest number [n] such that
    [n]>=[m] and [n] = [r] modulo [modu] *)
[@@deprecated "Use Z.round_up_to_r instead."]
[@@migrate { repl = Z.round_up_to_r } ]

val round_down_to_r : max:t -> r:t -> modu:t -> t
(** [round_down_to_r m r modu] is the largest number [n] such that
    [n]<=[m] and [n] = [r] modulo [modu] *)
[@@deprecated "Use Z.round_down_to_r instead."]
[@@migrate { repl = Z.round_down_to_r } ]

val two_power : t -> t
(** Computes [2^n]
    @raise Z.Overflow for exponents greater than 1024 *)
[@@deprecated "Use Z.two_power instead."]
[@@migrate { repl = Z.two_power } ]

val two_power_of_int : int -> t
(** Computes [2^n] *)
[@@deprecated "Use Z.two_power_of_int instead."]
[@@migrate { repl = Z.two_power_of_int } ]

val power_int_positive_int_opt : int -> int -> t option
(** Exponentiation *)
[@@deprecated "Use Big_int_Z.power_int_positive_int instead."]
[@@migrate { repl = (fun x y ->
    try Some (Big_int_Z.power_int_positive_int x y)
    with Invalid_argument _ -> None)
  } ]

val extract_bits : start:t -> stop:t -> t -> t
[@@deprecated "Use Z.extract_bits instead."]
[@@migrate { repl = Z.extract_bits } ]

val popcount: t -> int
[@@deprecated "Use Z.popcount instead."]
[@@migrate { repl = Z.popcount } ]

val hash : t -> int
[@@deprecated "Use Z.hash instead."]
[@@migrate { repl = Z.hash } ]

val to_string : t -> string
[@@deprecated "Use Z.to_string instead."]
[@@migrate { repl = Z.to_string } ]

val of_string : string -> t
[@@deprecated "Use Z.of_string instead."]
[@@migrate { repl = Z.of_string } ]

(** @raise Invalid_argument when the string cannot be parsed. *)

(** Prints the integer in decimal format. See also {!pretty_hex}.

    @before 25.0-Manganese there was an optional [hexa] argument. *)
val pretty : t Pretty_utils.formatter
[@@deprecated "Use Z.pretty instead."]
[@@migrate { repl = Z.pretty } ]

(** Prints the integer in hexadecimal format (replaces [hexa] optional
    argument of {!pretty} from older versions).

    @since 25.0-Manganese *)
val pretty_hex : t Pretty_utils.formatter
[@@deprecated "Use Z.pretty_hex instead."]
[@@migrate { repl = Z.pretty_hex } ]

val pp_bin : ?nbits:int -> ?sep:string -> t Pretty_utils.formatter
(** Print binary format. Digits are output by blocs of 4 bits
    separated by [~sep] with at least [~nbits] total bits. If [nbits] is
    non positive, it will be ignored.

    Positive values are prefixed with ["0b"] and negative values
    are printed as their 2-complement ([lnot]) with prefix ["1b"]. *)
[@@deprecated "Use Z.pp_bin instead."]
[@@migrate { repl = Z.pp_bin } ]

val pp_hex : ?nbits:int -> ?sep:string -> t Pretty_utils.formatter
(** Print hexadecimal format. Digits are output by blocs of 16 bits
    (4 hex digits) separated by [~sep] with at least [~nbits] total bits.
    If [nbits] is non positive, it will be ignored.

    Positive values are prefixed with ["0x"] and negative values
    are printed as their 2-complement ([lnot]) with prefix ["1x"]. *)
[@@deprecated "Use Z.pp_hex instead."]
[@@migrate { repl = Z.pp_hex } ]
