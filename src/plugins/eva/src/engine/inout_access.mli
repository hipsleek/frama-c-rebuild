(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Read and written memory zones at some given [Position.t] point.

    The data is projectified and depends on the analysis state. *)

(** Represents a read and write access. *)
type t = private {
  read : Memory_zone.t;
  write : Memory_zone.t;
}

module Access : sig
  include Lattice_type.Bottom_Bounded_Join_Semi_Lattice with type t := t

  val make : ?read:Memory_zone.t -> ?write:Memory_zone.t -> unit -> t
  (** [make ?read ?write ()] creates an [access] with the given [read] and
      [write] as read and written memory locations. *)

  val add_read : Memory_zone.t -> t -> t
  (** [add_read zone access] adds [zone] to the read memory locations in
      [access]. *)

  val add_write : Memory_zone.t -> t -> t
  (** [add_write zone access] adds [zone] to the written memory locations in
      [access]. *)
end

val register_read : Position.t -> Memory_zone.t -> unit
(** [register_read pos zone] adds the given [zone] as a "read" memory location
    at the given [pos]. *)

val register_write : Position.t -> Memory_zone.t -> unit
(** [register_write pos zone] adds the given [zone] as a "written" memory
    location at the given [pos]. *)

val register : Position.t -> t -> unit
(** [register pos access] adds the given [access] to the accesses at the
    given [pos]. *)

val mk_filter : filter_base:(Base.base -> bool) -> (t -> t)
(** [mk_filter ~filter_base] creates a filter function for the functions below
    from a function that filter bases. *)

val keep_globals_only : t -> t
(** [keep_globals_only access] filters the given memory locations to only keep
    those coming from global bases (cf. {!Base.is_global}). *)

val at : ?filter:(t -> t) -> Position.t -> t
(** [at ?filter pos] returns the read and written zones for the given [pos],
    filtered by [filter]. *)

val iter : ?filter:(t -> t) -> (Position.t -> t -> unit) -> unit
(** [iter ?filter f] iterates over all positions where a read or write
    access occurs and applies [f] on that access. The access is filtered by
    [filter] before being passed to [f]. *)

val fold : ?filter:(t -> t) ->
  (Position.t -> t -> 'acc -> 'acc) ->
  'acc ->
  'acc
(** [fold ?filter f acc] folds over all positions where a read or write
    access occurs and applies [f] on that access. The access is filtered by
    [filter] before being passed to [f]. *)

val dump : ?filter:(t -> t) -> Format.formatter -> unit
(** Dump the internal state regarding the read and written memory zones. The
    zones are being filtered by [filter] before being dumped. *)
