(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Main memory locations of Eva that can be used by abstract domains. *)

(** Abstract locations built over Precise_locs. *)
module PLoc : sig
  include Abstract_location.Leaf
    with type value = Cvalue.V.t
     and type location = Precise_locs.precise_location

  val make: Locations.t -> location
end

val ploc: PLoc.location Abstract_location.dependencies
