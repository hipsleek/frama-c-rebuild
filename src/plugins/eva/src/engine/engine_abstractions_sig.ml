(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Signature of abstractions used in the Eva engine. *)

(** Signature of the context abstractions used in the engine. *)
module type Context = Abstract.Context.External

(** Signature of the value abstractions used in the engine, with the reduction
    function of the reduced product. *)
module type Value = sig
  include Abstract.Value.External
  val reduce : t -> t
end

(** Signature of abstract location used in the engine. *)
module type Location = Abstract.Location.External

(** Signature of the abstract domain used in the engine. *)
module type Domain = sig
  include Abstract.Domain.External

  (** Direct access to the cvalue component of the abstract domain. *)
  include Cvalue_domain.Getters with type t := state

  (** Function used during the analysis to register computed states.
      Built by [Engine.Make]. *)
  module Store : sig
    val register_state: Callstack.t -> Domain_store.control_point -> t -> unit
  end
end


(* The four abstractions used in Eva (Context, Value, Location, Domain),
   plus an evaluation engine for these abstractions. *)
module type S = sig

  module Ctx : Context

  module Val : Value with type context = Ctx.t

  module Loc : Location with type value = Val.t

  module Dom : Domain with type value = Val.t
                       and type location = Loc.location
                       and type context = Ctx.t

  module Eval : Evaluation_sig.S
    with type state = Dom.t
     and type context = Ctx.t
     and type value = Val.t
     and type loc = Loc.location
     and type origin = Dom.origin
end
