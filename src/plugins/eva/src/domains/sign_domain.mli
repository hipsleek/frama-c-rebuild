(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Abstraction of the sign of integer variables. *)

include Abstract_domain.Leaf with type value = Sign_value.t
                              and type location = Precise_locs.precise_location

val registered: Abstractions.Domain.registered
