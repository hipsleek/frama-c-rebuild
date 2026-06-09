(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** An abstract domain built on top of the Simpler_domains.Simple_Cvalue
    interface that just prints the transfer functions called by the engine
    during an analysis. *)
include Abstract_domain.Leaf with type value = Cvalue.V.t
                              and type location = Precise_locs.precise_location

val registered: Abstractions.Domain.registered
