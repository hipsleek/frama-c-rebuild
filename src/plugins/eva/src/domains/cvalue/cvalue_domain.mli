(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Main domain of the Value Analysis. *)

module State : Abstract_domain.Leaf
  with type value = Main_values.CVal.t
   and type location = Main_locations.PLoc.location
   and type state = Cvalue.Model.t * Locals_scoping.clobbered_set

val registered: Abstractions.Domain.registered

(** Specific functions for partitioning optimizations.  *)

type prefix
module Subpart : Hashtbl.HashedType
val distinct_subpart :
  State.t -> State.t -> (prefix * Subpart.t * Subpart.t) option
val find_subpart : State.t -> prefix -> Subpart.t option

(** Special getters. *)

module type Getters = sig
  type t
  val get_cvalue : (t -> Cvalue.Model.t) option
  val get_cvalue_or_top : t -> Cvalue.Model.t
  val get_cvalue_or_bottom : t Lattice_bounds.or_bottom -> Cvalue.Model.t
end

module Getters (Dom : Abstract.Domain.External) : Getters with type t := Dom.t
