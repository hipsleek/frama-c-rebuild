(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A partitioning index is a collection of states optimized to determine
    if a new state is included in one of the states it contains — in a more
    efficient way than to test the inclusion with all stored states.
    Such an index is used to keep track of all the states already propagated
    through a control point, and to rule out new incoming states included in
    previous ones.

    Partitioning index relies on an heuristics on the cvalue domain,
    and is very inefficient without it. *)

module Make (Domain: Engine_abstractions_sig.Domain) : sig
  type t

  (** Creates an empty index. *)
  val empty: unit -> t

  (** Adds a state into an index. Returns true if the state did not belong to
      the index (and has indeed been added), and false if the index already
      contained the state. *)
  val add : Domain.t -> t -> bool

  val pretty : Format.formatter -> t -> unit
end
