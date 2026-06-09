(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Automatic builders to complete abstract domains from different
    simplified interfaces. *)

open Cil_types
open Eval

module type InputDomain = Domain_store.InputDomain

(** Part of an abstract domain signature automatically built by the
    {!Complete} functor. These functions can be redefined to achieve
    better precision or performance. See {!Abstract_domain} for more details. *)
module type LeafDomain = sig
  type t

  val name: string

  type context = unit
  val context_dependencies: context Abstract_context.dependencies
  val build_context: t -> context or_bottom

  val backward_location: t -> lval -> 'loc -> 'v -> ('loc * 'v) or_bottom
  val reduce_further: t -> exp -> 'v -> (exp * 'v) list

  val evaluate_predicate:
    t Abstract_domain.logic_environment -> t -> predicate -> Alarmset.status
  val reduce_by_predicate:
    t Abstract_domain.logic_environment -> t -> predicate -> bool -> t or_bottom
  val interpret_acsl_extension:
    acsl_extension -> t Abstract_domain.logic_environment -> t -> t

  val enter_loop: stmt -> t -> t
  val incr_loop_counter: stmt -> t -> t
  val leave_loop: stmt -> t -> t

  val project: Base.Hptset.t -> t -> t
  val filter: Base.Hptset.t -> t -> t
  val reuse: Base.Hptset.t -> current_input:t -> previous_output:t -> t

  val show_expr: 'a -> t -> Format.formatter -> exp -> unit
  val post_analysis: t Lattice_bounds.or_bottom -> unit

  module Store: Domain_store.S with type t := t

  val log_category: Self.category

  val key: t Abstract_domain.key
end

(** Automatically builds some functions of an abstract domain. *)
module Complete (Domain: InputDomain) : LeafDomain with type t := Domain.t

module Complete_Minimal
    (Value: Abstract_value.Leaf)
    (Location: Abstract_location.Leaf)
    (Domain: Simpler_domains.Minimal)
  : Abstract_domain.Leaf with type context = unit
                          and type value = Value.t
                          and type location = Location.location
                          and type state = Domain.t

module Complete_Minimal_with_datatype
    (Value: Abstract_value.Leaf)
    (Location: Abstract_location.Leaf)
    (Domain: Simpler_domains.Minimal_with_datatype)
  : Abstract_domain.Leaf with type context = unit
                          and type value = Value.t
                          and type location = Location.location
                          and type state = Domain.t

module Complete_Simple_Cvalue
    (Domain: Simpler_domains.Simple_Cvalue)
  : Abstract_domain.Leaf with type context = unit
                          and type value = Cvalue.V.t
                          and type location = Precise_locs.precise_location
                          and type state = Domain.t

(* Restricts an abstract domain on specific functions. The domain will only be
   enabled on the given functions. Moreover, a mode is associated to each of
   these functions, allowing (or not) the domain to infer or use properties
   in the current function and in all functions called from it.
   See {!Domain_mode} for more details. *)
module Restrict
    (Context: Abstract_context.S)
    (Value: Abstract_value.S with type context = Context.t)
    (Domain: Abstract.Domain.Internal
     with type context = Context.t
      and type value = Value.t)
    (_: sig val functions: Domain_mode.function_mode list end)
  : Abstract.Domain.Internal
    with type context = Context.t
     and type value = Value.t
     and type location = Domain.location
