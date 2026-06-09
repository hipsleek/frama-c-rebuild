(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Integer intervals with congruence.
    An interval defined by [min, max, rem, modu] represents all integers
    between the bounds [min] and [max] and congruent to [rem] modulo [modu].
    A value of [None] for [min] (resp. [max]) represents -infinity
    (resp. +infinity). [modu] is > 0, and [0 <= rem < modu]. *)

open Lattice_bounds

include Datatype.S_with_collections

include Eva_lattice_type.Full_AI_Lattice_with_cardinality with type t := t

(** Hints for the widening: set of relevant thresholds. *)
type widen_hint = Z.Set.t

(** [widen ~size ~hint t1 t2] is an over-approximation of [join t1 t2].
    [size] is the size (in bits) of the widened value, [hint] is a set of
    relevant thresholds for the widened interval bounds. *)
val widen: ?size:Z.t -> ?hint:widen_hint -> t -> t -> t

(** Checks that the interval defined by [min, max, rem, modu] is well formed. *)
val check:
  min:Z.t option -> max:Z.t option ->
  rem:Z.t -> modu:Z.t -> unit

(** Makes the interval of all integers between [min] and [max] and congruent
    to [rem] modulo [modu]. Fails if these conditions does not hold:
    - min ≤ max
    - 0 ≤ rem < modu
    - min ≅ rem [modu] ∧ max ≅ rem [modu] *)
val make:
  min:Z.t option -> max:Z.t option ->
  rem:Z.t -> modu:Z.t -> t

(** Makes the interval of all integers between [min] and [max]. *)
val inject_range: Z.t option -> Z.t option -> t

(** Returns the bounds of the given interval. [None] means infinity. *)
val min_and_max: t -> Z.t option * Z.t option

(** Returns the bounds and the modulo of the given interval. *)
val min_max_rem_modu:
  t -> Z.t option * Z.t option * Z.t * Z.t

(** [mem i t] returns true iff the integer [i] is in the interval [t]. *)
val mem: Z.t -> t -> bool

(** Returns the number of integers represented by the given interval.
    Returns [None] if the interval represents an infinite number of integers. *)
val cardinal: t -> Z.t option

val complement_under: min:Z.t -> max:Z.t -> t -> t or_bottom
(** Returns an under-approximation of the integers between [min] and [max]
    that are *not* represented by the given interval. *)

(** {2 Interval semantics.} *)

(** See {!Int_val} for more details. *)

val add_singleton_int: Z.t -> t -> t
val add: t -> t -> t
val add_under: t -> t -> t or_bottom
val neg: t -> t
val abs: t -> t

val scale: Z.t -> t -> t
val scale_div: pos:bool -> Z.t -> t -> t
val scale_div_under: pos:bool -> Z.t -> t -> t or_bottom
val scale_rem: pos:bool -> Z.t -> t -> t

val mul: t -> t -> t
val div: t -> t -> t or_bottom
val c_rem: t -> t -> t or_bottom

val cast: size:Z.t -> signed:bool -> t -> t

(** {2 Misc.} *)

val subdivide: t -> t * t

val reduce_sign: t -> bool -> t or_bottom
val reduce_bit: int -> t -> bool -> t or_bottom

val fold_int: ?increasing:bool -> (Z.t -> 'a -> 'a) -> t -> 'a -> 'a
val to_seq: ?increasing:bool -> t -> Z.t Seq.t
