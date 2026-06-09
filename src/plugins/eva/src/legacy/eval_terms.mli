(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Evaluation of terms and predicates *)

open Cil_types
open Cvalue

(** Evaluating a predicate. [Unknown] is the Top of the lattice *)
type predicate_status = Abstract_interp.Comp.result = True | False | Unknown

val join_predicate_status :
  predicate_status -> predicate_status -> predicate_status
(* val join_list_predicate_status :
    predicate_status list -> predicate_status *)


(** Error during the evaluation of a term or a predicate *)
type logic_evaluation_error =
  | Unsupported of string
  | UnsupportedLogicVar of logic_var
  | AstError of string
  | NoEnv of logic_label
  | NoResult
  | CAlarm
val pretty_logic_evaluation_error :
  Format.formatter -> logic_evaluation_error -> unit

exception LogicEvalError of logic_evaluation_error

type labels_states = Cvalue.Model.t Cil_datatype.Logic_label.Map.t

(** Evaluation environment. Currently available are function Pre and Post, or
    the environment to evaluate an annotation. *)
type eval_env

val make_env: Model.t Abstract_domain.logic_environment -> Model.t -> eval_env

val env_pre_f : pre:Model.t -> unit -> eval_env
val env_annot :
  ?c_labels:labels_states -> pre:Cvalue.Model.t -> here:Cvalue.Model.t ->
  unit -> eval_env
val env_post_f :
  ?c_labels:labels_states -> pre:Model.t -> post:Model.t ->
  result:varinfo option -> unit -> eval_env
val env_assigns: pre:Model.t -> eval_env

(** Used by auxiliary plugins, that do not supply the other states *)
val env_only_here: Model.t -> eval_env

val env_current_state: eval_env -> Model.t

(** Dependencies needed to evaluate a term or a predicate *)
type logic_deps = Memory_zone.t Cil_datatype.Logic_label.Map.t

(** Three modes to handle the alarms when evaluating a logical term. *)
type alarm_mode =
  | Ignore             (* Ignores all alarms. *)
  | Fail               (* Raises a LogicEvalError when an alarm is encountered. *)
  | Track of bool ref  (* Tracks the possibility of an alarm in the boolean: the
                          boolean is set to true if an alarm is encountered. *)

(** Return a pair of (under-approximating, over-approximating) zones. *)
val eval_tlval_as_zone_under_over:
  alarm_mode:alarm_mode ->
  Locations.access -> eval_env -> term -> Memory_zone.t * Memory_zone.t

(* ML: Should not be exported. *)
type 'a eval_result = {
  etype: Cil_types.typ;
  eunder: 'a;
  eover: 'a;
  empty: bool;
  ldeps: logic_deps;
}


val eval_term :
  alarm_mode:alarm_mode ->
  eval_env -> term -> V.t eval_result

val eval_tlval_as_location :
  alarm_mode:alarm_mode ->
  eval_env -> term -> Locations.t

val eval_tlval_as_zone :
  alarm_mode:alarm_mode ->
  Locations.access -> eval_env -> term -> Memory_zone.t

val eval_predicate :
  eval_env -> predicate -> predicate_status

(** [predicate_deps env p] computes the logic dependencies needed to evaluate
    [p] in the given evaluation environment [env].
    Returns None on either an evaluation error or on unsupported construct. *)
val predicate_deps: eval_env -> Cil_types.predicate -> logic_deps option

(** [tlval_deps env t] computes the logic dependencies needed to evaluate the
    address of lvalue term [t] in the given evaluation environment [env].
    @raises LogicEvalError on evaluation errors, unsupported constructs, or
    if [t] is not an lvalue term. *)
val tlval_deps: eval_env -> Cil_types.term -> logic_deps

val reduce_by_predicate :
  eval_env -> bool -> predicate -> eval_env
