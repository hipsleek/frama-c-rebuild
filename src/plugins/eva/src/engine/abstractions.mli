(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Registration of abstract domains.} *)

module Domain : sig

  (** Witness of the registration of an abstract domain, it can be used to
      programmatically enable the domain. *)
  type registered

  (** Registers a leaf abstract domain.
      - [name] must be unique. The domain is enabled by -eva-domains [name].
      - [descr] is a description printed in the help message of -eva-domains.
      - [experimental] is false by default. If set to true, a warning is emitted
        when the domain is enabled.
      - [priority] can be any integer; domains with higher priority are always
        processed first, and help messages list domains by decreasing priority.
        Current domains have priorities ranging from 1 to 10, so a priority of 0
        (respectively 11) ensures that a domain is processed after (respectively
        before) the classic Eva domains. Default priority is 0.
      - [auto_enable] is called during domain configuration; if it returns true,
        the domain is automatically enabled. Always [false] by default. *)
  val register :
    name:string -> descr:string ->
    ?experimental:bool -> ?priority:int -> ?auto_enable:(unit -> bool) ->
    (module Abstract_domain.Leaf) -> registered

  (** Registers a dynamic domain, which is built at the start of an analysis
      analysis using the function given as last argument.
      See function {!register} for more details. *)
  val dynamic_register :
    name:string -> descr:string ->
    ?experimental:bool -> ?priority:int -> ?auto_enable:(unit -> bool) ->
    (unit -> (module Abstract_domain.Leaf)) -> registered

  module type Context = Abstract.Context.External
  module type Value = Abstract.Value.External

  (** Functor domain which can be built over any value abstractions, but with
      fixed locations dependencies. *)
  module type Functor = sig
    type location
    val location_dependencies: location Abstract_location.dependencies
    module Make (C : Context) (V : Value with type context = C.t) : sig
      include Abstract_domain.S
        with type context = C.t
         and type value = V.t
         and type location = location
      val key : state Abstract_domain.key
    end
  end

  (** Registers a functor domain. See function {!register} for more details. *)
  val register_functor:
    name:string -> descr:string ->
    ?experimental:bool -> ?priority:int -> ?auto_enable:(unit -> bool) ->
    (module Functor) -> registered

end


(** {2 Reduced product between value abstractions.} *)

(** Value reduced product registration. Registering a value reduced product
    requires the keys of each value abstractions involved along with a reducer,
    i.e. a function that perform the reduction. *)
module Reducer : sig
  type ('a, 'b) reducer = 'a -> 'b -> 'a * 'b
  val register :
    'a Abstract_value.key -> 'b Abstract_value.key -> ('a, 'b) reducer -> unit
end


(** {2 Configuration of an analysis.} *)

(** Configuration defining the abstractions to be used in an analysis.
    A configuration is a set of registered abstract domains. Each domain comes
    with an optional analysis mode. None is the default mode: the domain is
    enabled for the whole analysis. See {!Domain_mode} for more details. *)
module Config : sig
  type t

  (** Creates the configuration according to the analysis parameters. *)
  val configure : unit -> t

  (** Creates a custom configuration from a list of registered abstract domains,
      associated with optional analysis modes. [None] is the default mode: the
      domain is enabled for the whole analysis. See {!Domain_mode} for more
      details. *)
  val of_list : (Domain.registered * Domain_mode.t option) list -> t

  (** Are two configurations identical? *)
  val equal : t -> t -> bool
end



(** {2 Types and functions used in the engine.} *)

(** The value abstractions signature used in the engine, with the reduction
    function of the reduced product. *)
module type Value_with_reduction = sig
  include Abstract.Value.External
  val reduce : t -> t
end

(** The four abstractions used in an Eva analysis. *)
module type S = sig
  module Ctx : Abstract.Context.External
  module Val : Value_with_reduction with type context = Ctx.t
  module Loc : Abstract.Location.External with type value = Val.t
  module Dom : Abstract.Domain.External
    with type value = Val.t
     and type location = Loc.location
     and type context = Ctx.t
end

(** Builds the abstractions according to a configuration. *)
val make : Config.t -> (module S)



(** {2 Analysis low level modifications.} *)

(** Registration of a hook, i.e. a function that modifies directly the three
    abstractions after their building by the engine and before the start of
    each analysis. *)
module Hooks : sig
  type hook = (module S) -> (module S)
  val register : hook -> unit
end
