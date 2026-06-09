(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type scope =
  | SC_Global
  | SC_Frame_in
  | SC_Frame_out
  | SC_Block_in
  | SC_Block_out

type effect_source =
  | FromCode
  | FromCall
  | FromReturn

type terminates_source =
  | Loop
  | Terminates
  | Decreases
  | MissingTerminates
  | MissingDecreases
  | Dependencies

type goal_source =
  | Effect of effect_source
  | Terminates of terminates_source

module type Export =
sig
  type pred
  type decl
  val export_section : Format.formatter -> string -> unit
  val export_goal : Format.formatter -> string -> pred -> unit
  val export_decl : Format.formatter -> decl -> unit
end

module type Splitter =
sig
  type pred
  val simplify : pred -> pred
  val split : bool -> pred -> pred Bag.t
end

(**
 * This is what is really needed to propagate something through the CFG.
 * Usually, the propagated thing should be a predicate,
 * but it can be more sophisticated like lists of predicates,
 * or maybe a structure to keep hypotheses and goals separated.
 * Moreover, proof obligations may also need to be handled.
 **)
module type S = sig

  type t_env
  type t_prop

  val pretty : Format.formatter -> t_prop -> unit
  val merge : t_env -> t_prop -> t_prop -> t_prop
  val empty : t_prop

  (** optionally init env with user logic variables *)
  val new_env : ?lvars:Cil_types.logic_var list -> kernel_function -> t_env

  val add_axiom : WpPropId.prop_id -> LogicUsage.logic_lemma -> unit

  val add_probe : t_env -> ?stmt:stmt -> string -> term -> t_prop -> t_prop

  val add_hyp :
    ?for_pid:WpPropId.prop_id -> t_env -> WpPropId.pred_info -> t_prop -> t_prop
  val add_goal : t_env -> WpPropId.pred_info -> t_prop -> t_prop

  val add_terminates_subgoal :
    t_env -> WpPropId.prop_id * 'a -> ?deps:Property.Set.t ->
    predicate -> stmt -> terminates_source -> t_prop -> t_prop

  val add_assigns : t_env -> WpPropId.assigns_info -> t_prop -> t_prop

  (** [use_assigns env hid kind assgn goal] performs the havoc on the goal.
   * [hid] should be [None] iff [assgn] is [WritesAny],
   * and tied to the corresponding identified_property otherwise.*)
  val use_assigns : t_env ->
    WpPropId.prop_id option -> WpPropId.assigns_desc -> t_prop -> t_prop

  val label  : t_env -> stmt option -> Clabels.c_label -> t_prop -> t_prop
  val init : t_env -> varinfo -> init_or_str option -> t_prop -> t_prop
  val const : t_env -> varinfo -> t_prop -> t_prop
  val assign : t_env -> stmt -> lval -> exp -> t_prop -> t_prop
  val return : t_env -> stmt -> exp option -> t_prop -> t_prop
  val test : t_env -> stmt -> exp -> t_prop -> t_prop -> t_prop
  val switch : t_env -> stmt -> exp -> (exp list * t_prop) list -> t_prop -> t_prop

  val has_init : t_env -> bool

  val loop_entry : t_prop -> t_prop
  val loop_step : t_prop -> t_prop

  (* -------------------------------------------------------------------------- *)
  (* --- Call Rules                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  val call_dynamic : t_env -> stmt ->
    WpPropId.prop_id -> lhost -> (kernel_function * t_prop) list -> t_prop

  val call_goal_precond : t_env -> stmt ->
    kernel_function -> exp list ->
    pre: WpPropId.pred_info list ->
    t_prop -> t_prop

  val call_terminates : t_env -> stmt ->
    kind:terminates_source ->
    ?kf:kernel_function -> exp list ->
    WpPropId.pred_info ->
    callee_t:predicate -> t_prop -> t_prop

  val call_decreases : t_env -> stmt ->
    ?kf:kernel_function -> exp list ->
    WpPropId.variant_info ->
    ?caller_t: predicate ->
    ?callee_d: variant -> t_prop -> t_prop

  val call : t_env -> stmt ->
    lval option -> kernel_function -> exp list ->
    pre:     WpPropId.pred_info list ->
    post:    WpPropId.pred_info list ->
    pexit:   WpPropId.pred_info list ->
    assigns: assigns ->
    p_post: t_prop ->
    p_exit: t_prop ->
    t_prop

  (* -------------------------------------------------------------------------- *)
  (* --- SCOPING RULES                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  val scope : t_env -> varinfo list -> scope -> t_prop -> t_prop
  val close : t_env -> t_prop -> t_prop

  (* -------------------------------------------------------------------------- *)
  (* --- FROM                                                               --- *)
  (* -------------------------------------------------------------------------- *)

  (** build [p => alpha(p)] for functional dependencies verification. *)
  val build_prop_of_from : t_env -> WpPropId.pred_info list -> t_prop -> t_prop

end
