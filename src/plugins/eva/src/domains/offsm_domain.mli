(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module D : Abstract_domain.Leaf
  with type value = Offsm_value.offsm_or_top
   and type location = Precise_locs.precise_location

val registered: Abstractions.Domain.registered
