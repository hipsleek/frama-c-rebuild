(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Domain for a taint analysis. *)

include Abstract_domain.Leaf
  with type value = Cvalue.V.t
   and type location = Precise_locs.precise_location

val registered: Abstractions.Domain.registered

type taint = | Direct | Indirect | Untainted

(** Is a memory zone tainted according to a given state?
    Only consider the taints of the given [names], if any. Otherwise,
    a memory zone is tainted as soon as it is tainted for at least one taint. *)
val is_tainted: ?names:string list -> state -> Memory_zone.t -> taint

(** Returns the list of taint names encountered by the taint analysis. *)
val taint_names: unit -> string list

(** Sets of taint names classified by kind of dependency. *)
type taint_names_by_kind =
  { direct_taint_names: Datatype.String.Set.t;
    (** Taint names for which the given zone has a direct data dependency. *)
    indirect_taint_names: Datatype.String.Set.t;
    (** Taint names for which the given zone has an indirect (control)
        dependency. *)
  }

(** Returns the sets of taint names whose tainted locations intersect the given
    memory zone, classified by kind of dependency (direct or indirect). *)
val taint_names_by_kind:
  state -> Memory_zone.t -> taint_names_by_kind Lattice_bounds.or_top
