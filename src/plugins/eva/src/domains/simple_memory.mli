(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Simple memory abstraction for scalar non-volatile variables, built upon a
    value abstraction. Basically a map from variable to values. *)

(** A builtin is an ocaml function for the interpretation of a whole C function:
    it takes the list of value arguments, and returns the result (that can be
    bottom). *)
type 'value builtin = 'value list -> 'value Eval.or_bottom

(** Set of relevant thresholds for the widening. *)
type widen_hint = Z.Set.t * Datatype.Float.Set.t

(** Abstraction of the values variables are mapped to. *)
module type Value = sig
  include Abstract_value.Leaf

  (** Widening operation to ensure convergence. *)
  val widen : widen_hint -> t -> t -> t

  (** This function must return [true] if the given variable should be
      tracked by the domain. All untracked variables are implicitly
      mapped to [V.top]. *)
  val track_variable: Cil_types.varinfo -> bool

  (** A list of builtins for the domain: each builtin is associated with the
      name of the C function it interprets. *)
  val builtins: (string * t builtin) list
end

(** Signature of a simple memory abstraction for scalar variables.  *)
module type S = sig
  type t
  type value

  (** [add loc typ v state] binds [loc] to [v] in state. If [typ] does
      not match the effective type of the location pointed, [V.top] is
      bound instead. This function automatically handles the case where
      [loc] abstracts multiple locations, or when some locations are not
      tracked by the domain. *)
  val add: Precise_locs.precise_location -> Cil_types.typ -> value -> t -> t

  (** [find loc typ state] returns the join of the abstract values stored
      in the locations abstracted to by [loc] in [state], assuming the
      result has type [typ]. When [loc] includes untracked locations, or when
      [typ] does not match the type of the locations in [loc], the
      result is approximated. *)
  val find: Precise_locs.precise_location -> Cil_types.typ -> t -> value

  (** [remove loc state] drops all information on the locations pointed to
      by [loc] from [state]. *)
  val remove: Precise_locs.precise_location -> t -> t

  (** [remove_variables list state] drops all information about the variables
      in [list] from state. *)
  val remove_variables: Cil_types.varinfo list -> t -> t

  (** Fold on base value pairs. *)
  val fold: (Base.t -> value -> 'a -> 'a) -> t -> 'a -> 'a

end

(* Builds a memory from a value abstraction. *)
module Make_Memory (_: sig val name: string end) (Value: Value) : sig
  include Datatype.S_with_collections
  include S with type t := t
             and type value := Value.t

  val top: t
  (** The top abstraction, which maps all variables to {!V.top}. *)

  val join: t -> t -> t
  val widen: Cil_types.kernel_function -> Cil_types.stmt -> t -> t -> t
  val is_included: t -> t -> bool
end


(* Builds a complete Eva domain from a value abstraction. *)
module Make_Domain
    (_: sig val name: string end)
    (Value: Value)
  : sig

    include Abstract_domain.Leaf with type value = Value.t
                                  and type location = Precise_locs.precise_location

    include S with type t := t
               and type value := Value.t

  end
