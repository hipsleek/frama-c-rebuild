(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Integer values abstractions: an abstraction represents a set of integers.
    Provided with a lattice structure and over-approximations of arithmetic
    operations. *)

(** Abstractions do not represent the empty set. Instead, operations creating
    empty sets return `Bottom. *)
open Lattice_bounds

include Datatype.S_with_collections

include Eva_lattice_type.Full_AI_Lattice_with_cardinality with type t := t

(** Hints for the widening: set of relevant thresholds. *)
type widen_hint = Z.Set.t

(** [widen ~size ~hint t1 t2] is an over-approximation of [join t1 t2].
    [size] is the size (in bits) of the widened value, and [hint] is a set of
    relevant thresholds. *)
val widen: ?size:Z.t -> ?hint:widen_hint -> t -> t -> t

val zero: t
val one: t
val minus_one: t
val zero_or_one: t

val positive_integers: t
(** All positive integers, including 0. *)

val negative_integers: t
(** All negative integers, including 0. *)

(** {2 Building.} *)

(** Returns an exact abstraction of the given integer. *)
val inject_singleton: Z.t -> t

(** [inject_range min max] returns an abstraction of all integers between
    [min] and [max] included. [None] means that the abstraction is unbounded. *)
val inject_range: Z.t option -> Z.t option -> t

(** Builds an abstraction of all integers between [min] and [max] included and
    congruent to [rem] modulo [modu]. For [min] and [max], [None] is the
    corresponding infinity. Checks that [min] <= [max], [modu] > 0 and
    0 <= [rest] < [modu], and fails otherwise. If possible, reduces [min] and
    [max] according to the modulo. *)
val inject_interval:
  min: Z.t option -> max: Z.t option ->
  rem: Z.t -> modu: Z.t ->
  t

(** As [inject_interval], but also checks that [min] and [max] are congruent to
    [rem] modulo [modu]. *)
val make:
  min: Z.t option -> max: Z.t option ->
  rem: Z.t -> modu: Z.t ->
  t

(** {2 Accessors.} *)

(** Returns the smallest integer represented by an abstraction.
    Returns None if it does not exist, i.e. if the abstraction is unbounded. *)
val min_int: t -> Z.t option

(** Returns the highest integer represented by an abstraction.
    Returns None if it does not exist, i.e. if the abstraction is unbouded. *)
val max_int: t -> Z.t option

(** Returns the smallest and highest integers represented by an abstraction. *)
val min_and_max: t -> Z.t option * Z.t option

(** Returns [min, max, rem, modu] such that for all integers [i] represented by
    the given abstraction, [i] satisfies min ≤ i ≤ max and i ≅ rem [modu]. *)
val min_max_rem_modu:
  t -> Z.t option * Z.t option * Z.t * Z.t

exception Not_Singleton

(** Projects a singleton abstraction into an integer.
    @raise Not_Singleton if the cardinal of the argument is not 1. *)
val project_int: t -> Z.t

(** Is an abstraction internally represented as a small integer set? *)
val is_small_set: t -> bool
val project_small_set: t -> Z.t list option

(** {2 Cardinality.} *)

val is_singleton: t -> bool
val cardinal_zero_or_one: t -> bool
val cardinal: t -> Z.t option
val cardinal_estimate: size:Z.t -> t -> Z.t
val cardinal_less_than: t -> int -> int
val cardinal_is_less_than: t -> int -> bool

val is_zero: t -> bool
val contains_zero: t -> bool
val contains_non_zero: t -> bool

(** {2 Arithmetics.} *)

val add: t -> t -> t
(** Addition of two integer abstractions. *)

val add_under: t -> t -> t or_bottom
(** Under-approximation of the addition of two integer abstractions *)

val add_singleton: Z.t -> t -> t
(** Addition of an integer abstraction with a singleton integer.
    Exact operation. *)

val neg: t -> t
(** Negation of an integer abstraction. *)

val abs: t -> t
(** Absolute value of an integer abstraction. *)

val scale: Z.t -> t -> t
(** [scale f v] returns an abstraction of the integers [f * x]
    for all [x] in [v]. This operation is exact. *)

val scale_div: pos:bool -> Z.t -> t -> t
(** [scale_div f v] is an over-approximation of the elements [x / f] for all
    elements [x] in [v]. Uses the computer division (like in C99) if [pos] is
    false, and the euclidean division if [pos] is true. *)

val scale_div_under: pos:bool -> Z.t -> t -> t or_bottom
(** Under-approximation of the division. *)

val scale_rem: pos:bool -> Z.t -> t -> t
(** Over-approximation of the remainder of the division. Uses the computer
    division (like in C99) if [pos] is false, and the euclidean division if
    [pos] is true. *)

val mul: t -> t -> t
val div: t -> t -> t or_bottom
val c_rem: t -> t -> t or_bottom
val shift_left:  t -> t -> t or_bottom
val shift_right: t -> t -> t or_bottom

val bitwise_and: t -> t -> t
val bitwise_or: t -> t -> t
val bitwise_xor: t -> t -> t
val bitwise_signed_not: t -> t
val bitwise_unsigned_not: size:int -> t -> t

(** {2 Comparisons} *)

(** [forward_comp op l r] returns the result of the comparison [l op r]. *)
val forward_comp:
  Abstract_interp.Comp.t -> t -> t -> Abstract_interp.Comp.result

(** [backward_comp_left op l r] reduces [l] by assuming [l op r] holds. *)
val backward_comp_left: Abstract_interp.Comp.t -> t -> t -> t or_bottom


(** {2 Misc} *)

val cast_int_to_int: size:Z.t -> signed:bool -> t -> t

(** Splits an abstraction into two abstractions. *)
val subdivide: t -> t * t

(** Extracts bits from [start] to [stop] from the given integer abstraction,
    [start] and [stop] being included. *)
val extract_bits: start:Z.t -> stop:Z.t -> t -> t

(** Builds an abstraction of all integers in a C integer type. *)
val create_all_values: signed:bool -> size:int -> t

(** [all_values ~size v] returns true iff v contains all integer values
    representable in [size] bits. *)
val all_values: size:Z.t -> t -> bool

val complement_under: size:int -> signed:bool -> t -> t or_bottom
(** Returns an under-approximation of the integers of the given size and
    signedness that are *not* represented by the given value. *)

(** Iterates on all integers represented by an abstraction, in increasing order
    by default. If [increasing] is set to false, iterate by decreasing order.
    @raise Abstract_interp.Error_Top if the abstraction is unbounded. *)
val fold_int: ?increasing:bool -> (Z.t -> 'a -> 'a) -> t -> 'a -> 'a

(** Builds a sequence of all integers represented by an abstraction, in
    increasing order (resp. decreasing order if [increasing] is set to false).
    The sequence might be infinite.
    @raise Abstract_interp.Error_Top if the abstraction has no lower bound
    (or no upper bound in decreasing order). *)
val to_seq: ?increasing:bool -> t -> Z.t Seq.t
