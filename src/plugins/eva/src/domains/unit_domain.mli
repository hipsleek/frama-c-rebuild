(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Make
    (Value: Abstract_value.S)
    (Loc: Abstract_location.S)
  : Abstract.Domain.Internal with type context = unit
                              and type state = unit
                              and type value = Value.t
                              and type location = Loc.location
