(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Datatypes for analyses types *)

open Cil_types
open Analyses_types
open Cil_datatype

module Annotation_kind: Datatype.S with type t = annotation_kind

(** Predicate or term. Note that the notion of egality for predicates is
    structural but for terms it is physical (using [Misc.Id_term]). That means
    that comparison is undefined and that [Map] and [Set] cannot be used. See
    [Misc.Id_term] for more information. *)
module Pred_or_term: Datatype.S_with_collections with type t = pred_or_term

module At_data: sig
  include Datatype.S_with_collections with type t = at_data

  val create:
    ?error:exn ->
    kernel_function ->
    kinstr ->
    lscope ->
    pred_or_term ->
    logic_label ->
    at_data
    (** [create ?error kf kinstr lscope pot label] creates an [at_data] from the
        given arguments. *)
end

module Ival_datatype: Datatype.S_with_collections with type t = ival

(** profile that maps logic variables that are function parameters to their
    interval depending on the arguments at the callsite of the function *)
module Profile: sig
  include
    Datatype.S_with_collections
    with type t = ival Cil_datatype.Logic_var.Map.t

  val make: logic_var list -> ival list -> t
  val is_empty: t -> bool
  val empty: t
  val is_included: t -> t -> bool
  val pretty: Format.formatter -> t -> unit
end

(** term with a profile: a term inside a logic function or predicate may
    contain free variables. The profile indicates the interval for those
    free variables. *)
module Id_term_in_profile: Datatype.S_with_hashtbl
  with type t = term * Profile.t

(** profile of logic function or predicate: a logic info representing a logic
    function or a predicate together with a profile for its arguments. *)
module LFProf: Datatype.S_with_collections
  with type t = logic_info * Profile.t

(** logic environment: interval of all bound variables. It consists in two
    components
    - a profile for variables bound through function arguments
    - an association list for variables bound by a let or a quantification *)
module Logic_env : sig
  type t

  (** forward reference to meet of intervals *)
  val ival_meet_ref : (ival -> ival -> ival) ref

  (** add a new binding for a let or a quantification binder only *)
  val add : t -> logic_var -> ival -> t

  (** the empty environment *)
  val empty : t

  (** create a new environment from a profile, for function calls *)
  val make : Profile.t -> t

  (** find a logic variable in the environment *)
  val find : t -> logic_var -> ival

  (** get the profile of the logic environment, i.e. bindings through function
      arguments *)
  val get_profile : t -> Profile.t

  (** refine the interval of a logic variable: replace an interval with a more
      precise one *)
  val refine : t -> logic_var -> ival -> t

  val pretty: Format.formatter -> t -> unit
end


(** Imperative environment to perform fixpoint algorithm for recursive
    functions. This environment store four pieces of information associated
    to every logic_info:
    - the current profile in which the interval for the logic_info is inferred.
    - the current interval that it is inferred to.
    - a map associating to each parameter all the arguments in their profiles
      that this parameter has been called with up until now.
    - the depth of calls to the fixpoint algorithm associated to the logic_info.
      When this depth reaches 0, the entry corresponding to the logic_info is
      cleared thus avoiding unification between two independent calls to the same
      logic function, which is unsafe.

    The third argument is used so that whenever a parameter of a logic_info is
    unified by widening with a new value, the interval inference algorithm
    updates all the arguments that this parameter have been called with, to
    assign the new interval to it *)
module LF_env : sig

  (** find the current profile in which a recursive function or predicate is
      being inferred *)
  val find_profile : logic_info -> Profile.t

  (** find the currently inferred interval for a call to a logic function *)
  val find_ival : logic_info -> ival

  (** return the pair of both the results of [find_profile] and [find_ival]*)
  val find_profile_ival : logic_info -> Profile.t * ival

  (** find all the arguments a recursive function or predicate has been
      called with*)
  val find_args : logic_info -> Profile.t Misc.Id_term.Hashtbl.t Logic_var.Map.t

  (** clear the table of intervals for logic function *)
  val clear : unit -> unit

  (** add [ival] as the current one for a logic function or predicate call *)
  val add :
    logic_env:Logic_env.t ->
    logic_info ->
    Profile.t ->
    ival ->
    term list ->
    unit

  (** decrease the counter of fixpoint depth *)
  val decrease : logic_info -> unit

  (** update the current interval for the a given logic_info *)
  val update_ival : logic_info -> ival -> unit

  (** determine whether a logic function or predicate is recursive *)
  val is_rec : logic_info -> bool
end

module Number_ty:  Datatype.S_with_collections
  with type t = number_ty
