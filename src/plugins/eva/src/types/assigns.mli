(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Datastructures and common operations for the results of the From plugin. *)

module DepsOrUnassigned : sig

  type t =
    | Unassigned (** Location has never been assigned *)
    | AssignedFrom of Deps.t (** Location guaranteed to have been overwritten,
                                 its contents depend on the [Deps.t] value *)
    | MaybeAssignedFrom of Deps.t  (** Location may or may not have been
                                       overwritten *)

  (** The lattice is [DepsBottom <= Unassigned], [DepsBottom <= AssignedFrom z],
      [Unassigned <= MaybeAssignedFrom] and
      [AssignedFrom z <= MaybeAssignedFrom z]. *)

  val top : t
  val equal : t -> t -> bool
  val may_be_unassigned : t -> bool
  val to_zone : t -> Memory_zone.t
end

module Memory : sig
  include Lmap_bitwise.Location_map_bitwise with type v = DepsOrUnassigned.t

  val find : t -> Memory_zone.t -> Memory_zone.t
  (** Imprecise version of find, in which data and indirect dependencies are
      not distinguished *)

  val find_precise : t -> Memory_zone.t -> Deps.t
  (** Precise version of find *)

  val find_precise_loffset : LOffset.t -> Base.t -> Int_Intervals.t -> Deps.t

  val add_binding : exact:bool -> t -> Memory_zone.t -> Deps.t -> t
  val add_binding_loc : exact:bool -> t -> Locations.t -> Deps.t -> t
  val add_binding_precise_loc :
    exact:bool -> Locations.access -> t ->
    Precise_locs.precise_location -> Deps.t -> t
end

type t = {
  return : Deps.t
(** Dependencies for the returned value *);
  memory : Memory.t
(** Dependencies on all the zones modified by the function *);
}

include Datatype.S with type t := t

val top : t
val join : t -> t -> t

(** Extract the left part of a from result, ie. the zones that are written *)
val outputs : t -> Memory_zone.t
