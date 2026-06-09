(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val product_category: Self.category

module Make
    (Context  : Abstract_context.S)
    (Value    : Abstract_value.S with type context = Context.t)
    (Location : Abstract_location.S with type value = Value.t)
    (Left  : Abstract.Domain.Internal
     with type context = Context.t
      and type value = Value.t
      and type location = Location.location)
    (Right : Abstract.Domain.Internal
     with type context = Context.t
      and type value = Value.t
      and type location = Location.location)
  : Abstract.Domain.Internal
    with type context = Context.t
     and type value = Value.t
     and type location = Location.location
     and type state = Left.state * Right.state
