(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Make
    (Value: Abstract_value.S)
    (Left: Abstract.Location.Internal with type value = Value.t)
    (Right: Abstract.Location.Internal with type value = Value.t)
  : Abstract.Location.Internal
    with type value = Value.t
     and type location = Left.location * Right.location
     and type offset = Left.offset * Right.offset
