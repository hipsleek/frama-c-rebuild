(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module provides transient datastructures that may be more precise
    than an {!Ival.t}, {!Addresses.Bits.t} and {!Locations.t}
    respectively, typically for l-values such as [t[i][j]], [p->t[i]], etc.
    Those structures do not have a lattice structure, and cannot be stored
    as an abstract domain. However, they can be use to model more precisely
    read or write accesses to semi-imprecise l-values. *)


(** {2 Precise offsets} *)

type precise_offset
val pretty_offset : Format.formatter -> precise_offset -> unit

val equal_offset: precise_offset -> precise_offset -> bool

val offset_zero : precise_offset
val offset_bottom : precise_offset
val offset_top : precise_offset
val inject_ival : Ival.t -> precise_offset

val is_bottom_offset : precise_offset -> bool

val imprecise_offset : precise_offset -> Ival.t

(*val _scale_offset : Z.t -> precise_offset -> precise_offset*)
val shift_offset_by_singleton : Z.t -> precise_offset -> precise_offset
val shift_offset : Ival.t -> precise_offset -> precise_offset


(** {2 Precise Address_set.Bits} *)

type precise_addr_bits
val pretty_addr_bits : Format.formatter -> precise_addr_bits -> unit
val bottom_addr_bits : precise_addr_bits

val inject_addr_bits : Addresses.Bits.t -> precise_addr_bits
val combine_base_precise_offset : Base.t -> precise_offset -> precise_addr_bits
val combine_addr_precise_offset :
  Addresses.Bits.t -> precise_offset -> precise_addr_bits

val imprecise_addr_bits : precise_addr_bits -> Addresses.Bits.t


(** {2 Precise locations} *)

type precise_location

val equal_loc: precise_location -> precise_location -> bool

val loc_size: precise_location -> Z_or_top.t

val make_precise_loc :
  precise_addr_bits -> size:Z_or_top.t -> precise_location

val imprecise_location : precise_location -> Locations.t

val loc_bottom : precise_location
val is_bottom_loc: precise_location -> bool

val loc_top : precise_location
val is_top_loc: precise_location -> bool

val replace_base: Base.substitution -> precise_location -> precise_location

val fold: (Locations.t -> 'a -> 'a) -> precise_location -> 'a -> 'a

val enumerate_valid_bits:
  Locations.access -> precise_location -> Memory_zone.t

val valid_cardinal_zero_or_one: for_writing:bool -> precise_location -> bool
(** Is the restriction of the given location to its valid part precise enough
    to perform a strong read, or a strong update. *)

val cardinal_zero_or_one: precise_location -> bool
(** Should not be used, {!valid_cardinal_zero_or_one} is almost always more
    useful *)

val pretty_loc: precise_location Pretty_utils.formatter

val valid_part:
  Locations.access -> bitfield:bool -> precise_location -> precise_location
(** Overapproximation of the valid part of the given location (without any
    access, or for a read or write access).
    [bitfield] indicates whether the location may be the one of a bitfield, and
    is true by default. If it is set to false, the location is assumed to be
    byte aligned, and its offset (expressed in bits) is reduced to be congruent
    to 0 modulo 8. *)


(** {2 Deprecated} *)

type precise_location_bits
[@@deprecated "Use precise_addr_bits instead"]

val pretty_loc_bits : Format.formatter -> precise_addr_bits -> unit
[@@deprecated "Use pretty_addr_bits instead"]
[@@migrate { repl = Rel.pretty_addr_bits }]

val bottom_location_bits : precise_addr_bits
[@@deprecated "Use bottom_addr_bits instead"]
[@@migrate { repl = Rel.bottom_addr_bits }]

val inject_location_bits : Addresses.Bits.t -> precise_addr_bits
[@@deprecated "Use inject_addr_bits instead"]
[@@migrate { repl = Rel.inject_addr_bits }]

val combine_loc_precise_offset :
  Addresses.Bits.t -> precise_offset -> precise_addr_bits
[@@deprecated "Use combine_addr_precise_offset instead"]
[@@migrate { repl = Rel.combine_addr_precise_offset }]

val imprecise_location_bits :
  precise_addr_bits -> Addresses.Bits.t
[@@deprecated "Use imprecise_addr_bits instead"]
[@@migrate { repl = Rel.imprecise_addr_bits }]
