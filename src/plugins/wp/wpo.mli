(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open VCS
open Cil_types
open Cil_datatype
open WpPropId

type index =
  | Axiomatic of string option
  | Function of kernel_function * string option

(* ------------------------------------------------------------------------ *)
(**{1 Proof Obligations}                                                    *)
(* ------------------------------------------------------------------------ *)

module DISK :
sig
  val file_goal : pid:prop_id -> model:WpContext.model ->
    Why3.Driver.driver -> Why3Provers.t -> Filepath.t
end

module GOAL :
sig
  type t
  open Lang
  val dummy : t
  val trivial : t
  val is_trivial : t -> bool
  val is_computed : t -> bool
  val make : Conditions.sequent -> t
  val compute : pid:WpPropId.prop_id -> t -> unit
  val compute_proof : pid:WpPropId.prop_id -> ?opened:bool -> t -> F.pred
  val compute_descr : pid:WpPropId.prop_id -> t -> Conditions.sequent
  val compute_probes : pid:WpPropId.prop_id -> t -> F.term Probe.Map.t
  val get_descr : t -> Conditions.sequent
  val qed_time : t -> float
end

module VC_Annot :
sig
  type t = {
    (* Generally empty, but for Lemmas *)
    axioms : Definitions.axioms option ;
    goal : GOAL.t ;
    tags : Splitter.tag list ;
    warn : Warning.t list ;
    deps : Property.Set.t ;
    path : Stmt.Set.t ;
    source : (stmt * Mcfg.goal_source) option ;
  }

  val is_trivial : t -> bool
  val resolve : pid:prop_id -> t -> bool

end

(* ------------------------------------------------------------------------ *)
(**{1 Proof Obligations}                                                    *)
(* ------------------------------------------------------------------------ *)

type po = t and t = {
    po_gid   : string ;  (** goal identifier *)
    po_sid   : string ;  (** goal short identifier (without model) *)
    po_name  : string ;  (** goal informal name *)
    po_idx   : index ;   (** goal index *)
    po_model : WpContext.model ;
    po_pid   : WpPropId.prop_id ; (* goal target property *)
    po_formula : VC_Annot.t ; (* proof obligation *)
  }

module S : Datatype.S_with_collections with type t = po

val get_gid: t -> string
val get_property: t -> Property.t
val get_index : t -> index
val get_label : t -> string
val get_model : t -> WpContext.model
val get_scope : t -> WpContext.scope
val get_context : t -> WpContext.context
val get_file_logout : t -> Prover.t -> Filepath.t
(** only filename, might not exists *)

val get_file_logerr : t -> Prover.t -> Filepath.t
(** only filename, might not exists *)

val qed_time : t -> float

val clear : unit -> unit
val remove : t -> unit

val add : t -> unit
val age : t -> int (* generation *)

val reduce : t -> bool
(** tries simplification *)

val resolve : t -> bool
(** tries simplification and set result if valid *)

val set_result : t -> Prover.t -> result -> unit
val clear_results : t -> unit

val add_modified_hook : (t -> unit) -> unit
(** Hook is invoked for each goal results modification.
    Remark: [clear()] does not trigger those hooks,
    Cf. [add_cleared_hook] instead. *)

val add_removed_hook : (t -> unit) -> unit
(** Hook is invoked for each removed goal.
    Remark: [clear()] does not trigger those hooks,
    Cf. [add_cleared_hook] instead. *)

val add_cleared_hook : (unit -> unit) -> unit
(** Register a hook when the entire table is cleared. *)

val modified : t -> unit
val computed : t -> bool
val compute : t -> Definitions.axioms option * Conditions.sequent

(** Warning: Prover results are stored as they are from prover output,
    without taking into consideration that validity is inverted
    for smoke tests.

    On the contrary, proof validity is computed with respect to
    smoke test/non-smoke test.
*)

(** Definite result for this prover (not computing) *)
val has_verdict : t -> Prover.t -> bool

(** Raw prover result (without any respect to smoke tests) *)
val get_result : t -> Prover.t -> result

(** Return all results (without any respect to smoke tests). *)
val get_results : t -> (Prover.t * result) list

(** Return all prover results (without any respect to smoke tests). *)
val get_prover_results : t -> (Prover.t * result) list

(** Consolidated wrt to associated property and smoke test. *)
val get_proof : t -> [`Passed|`Failed|`Unknown] * Property.t

(** Associated property. *)
val get_target : t -> Property.t

val is_trivial : t -> bool
(** Currently trivial sequent (no forced simplification) *)

val is_fully_valid : t -> bool
(** Checks for some prover or script with valid verdict (no forced qed) *)

val is_locally_valid : t -> bool
(** Checks for some prover (no tactic) with valid verdict (no forced qed) *)

val all_not_valid : t -> bool
(** Checks for all provers to give a non-valid, computed verdict *)

val is_passed : t -> bool
(** valid, or all-not-valid for smoke tests *)

val has_unknown : t -> bool
(** Checks there is some provers with a non-valid verdict *)

val warnings : t -> Warning.t list

val is_tactic : t -> bool
val is_smoke_test : t -> bool

val iter :
  ?ip:Property.t ->
  ?index:index ->
  ?on_axiomatics:(string option -> unit) ->
  ?on_behavior:(kernel_function -> string option -> unit) ->
  ?on_goal:(t -> unit) ->
  unit -> unit

val iter_on_goals: (t -> unit) -> unit
val goals_of_property: Property.t -> t list

val pp_index : Format.formatter -> index -> unit
val pp_goal : Format.formatter -> t -> unit
val pp_title : Format.formatter -> t -> unit

val pp_axiomatics : Format.formatter -> string option -> unit
val pp_function : Format.formatter -> Kernel_function.t -> string option -> unit
val pp_goal_flow : Format.formatter -> t -> unit
val pp_flow : Format.formatter -> unit

(* -------------------------------------------------------------------------- *)
(* --- Generators                                                         --- *)
(* -------------------------------------------------------------------------- *)

(** VC Generator interface. *)

class type generator =
  object
    method model : WpContext.model

    (** Generate VCs for the given Property. *)
    method compute_ip : Property.t -> t Bag.t

    (** Generate VCs for call preconditions at the given statement. *)
    method compute_call : stmt -> t Bag.t

    (** Generate VCs for all functions
        matching provided behaviors and property names.
        For `~bhv` and `~prop` optional arguments,
        default and empty list means {i all} properties. *)
    method compute_main :
      ?fct:Wp_parameters.functions ->
      ?bhv:string list ->
      ?prop:string list ->
      unit -> t Bag.t
  end

(* -------------------------------------------------------------------------- *)
(* --- Internal hook                                                      --- *)
(* -------------------------------------------------------------------------- *)

(** / **)

val wp_reached_set_doomed: (Emitter.emitter -> WpPropId.prop_id -> unit) ref

(** / **)

(* -------------------------------------------------------------------------- *)
