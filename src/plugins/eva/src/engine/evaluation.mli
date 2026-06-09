(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type Value = sig
  include Abstract.Value.External

  (** Inter-reduction of values. Useful when the value module is a reduced
      product of several abstraction.
      The value computed by the forward evaluation for each sub-expression or
      lvalue is reduced by this function. *)
  val reduce : t -> t
end

module type Queries = sig
  include Abstract_domain.Queries
  include Datatype.S with type t = state
end

(** Generic functor. *)
module Make
    (Context : Abstract_context.S)
    (Value : Value with type context = Context.t)
    (Loc : Abstract_location.S with type value = Value.t)
    (Domain : Queries with type context = Context.t
                       and type value = Value.t
                       and type location = Loc.location)
  : Evaluation_sig.S with type state = Domain.state
                      and type context = Context.t
                      and type value = Value.t
                      and type origin = Domain.origin
                      and type loc = Loc.location
