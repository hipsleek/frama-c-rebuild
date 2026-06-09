(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Sets of intervals with a lattice structure. Consecutive intervals are
    automatically fused. *)

type itv = Z.t * Z.t

module type S = sig

  include Lattice_type.Full_Lattice

  val is_top: t -> bool

  val inject_bounds: Z.t -> Z.t -> t
  val inject_itv: itv -> t
  val inject: itv list -> t
  val from_ival_size: Ival.t -> Z_or_top.t -> t
  (** Conversion from an ival, which represents the beginning of
      each interval. The size if taken from the [Z_or_top.t] argument.
      If the result contains more than [-plevel] arguments, it is
      automatically over-approximated. *)

  val from_ival_size_under: Ival.t -> Z_or_top.t -> t
  (** Same as [from_ival_size], except that the result is an under-approximation
      if the ival points to too many locations *)

  val project_set: t -> itv list
  (** May raise [Error_Top].
      As intervals are not represented as lists, this function has an overhead.
      Use iterators whenever possible instead. *)

  val project_singleton: t -> itv option

  (** Iterators *)

  val fold: (itv -> 'a -> 'a) -> t -> 'a -> 'a
  (** May raise [Error_Top] *)

  val iter: (itv -> unit) -> t -> unit
  (** May raise [Error_Top] *)

  val pretty_typ: Cil_types.typ option -> t Pretty_utils.formatter
  (** Pretty-printer that supposes the intervals are subranges of
      a C type, and use the type to print nice offsets *)

  val range_covers_whole_type: Cil_types.typ -> t -> bool
  (** Does the interval cover the entire range of bits that are valid
      for the given type. *)


  (**/**)

  val pretty_debug: t Pretty_utils.formatter

end
