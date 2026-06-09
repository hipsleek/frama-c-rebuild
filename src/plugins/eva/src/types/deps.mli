(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Memory dependencies of an expression. *)
type t = {
  data: Memory_zone.t;
  (** Memory zone directly required to evaluate the given expression. *)
  indirect: Memory_zone.t;
  (** Memory zone read to compute data addresses. *)
}

include Datatype.S with type t := t

val pretty_precise: Format.formatter -> t -> unit

(* Constructors *)

val top : t
val bottom : t
val data : Memory_zone.t -> t
val indirect : Memory_zone.t -> t

(* Conversion *)

val to_zone : t -> Memory_zone.t

(* Mutators *)

val add_data : t -> Memory_zone.t -> t
val add_indirect : t -> Memory_zone.t -> t

(* Map *)

val map : (Memory_zone.t -> Memory_zone.t) -> t -> t

(* Lattice operators *)

val is_included : t -> t -> bool
val join : t -> t -> t
val narrow : t -> t -> t
