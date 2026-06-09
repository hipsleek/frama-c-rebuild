(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Compilation of ACSL Logic-Info                                     --- *)
(* -------------------------------------------------------------------------- *)

open LogicUsage
open Cil_types
open Cil_datatype
open Clabels
open Lang.F
open Memory
open Sigma
open Definitions

type polarity = [ `Positive | `Negative | `NoPolarity ]

module Make( M : Memory.Model ) :
sig

  (** {3 Definitions} *)

  type value = M.loc Memory.value
  type logic = M.loc Memory.logic
  type result = M.loc Memory.result

  (** {3 Frames} *)

  type call
  type frame

  val pp_frame : Format.formatter -> frame -> unit

  val local : descr:string -> frame
  val frame : kernel_function -> frame
  val call : ?result:M.loc -> kernel_function -> value list -> call
  val call_pre   : sigma -> call -> sigma -> frame
  val call_post  : sigma -> call -> sigma sequence -> frame

  val mk_frame :
    ?kf:Cil_types.kernel_function ->
    ?result:result ->
    ?status:Lang.F.var ->
    ?formals:value Varinfo.Map.t ->
    ?labels:sigma Clabels.LabelMap.t ->
    ?descr:string ->
    unit -> frame

  val formal : varinfo -> value option
  val return : unit -> typ
  val result : unit -> result
  val status : unit -> var
  val trigger : trigger -> unit

  val guards : frame -> pred list
  val mem_frame : c_label -> sigma
  val has_at_frame : frame -> c_label -> bool
  val mem_at_frame : frame -> c_label -> sigma
  val set_at_frame : frame -> c_label -> sigma -> unit

  val in_frame : frame -> ('a -> 'b) -> 'a -> 'b
  val get_frame : unit -> frame

  (** {3 Environment} *)

  type env

  val mk_env : ?here:sigma -> ?lvars:Logic_var.t list -> unit -> env
  val current : env -> sigma
  val move_at : env -> sigma -> env
  val env_at : env -> c_label -> env
  val mem_at : env -> c_label -> sigma
  val env_let : env -> logic_var -> logic -> env
  val env_letp : env -> logic_var -> pred -> env
  val env_letval : env -> logic_var -> value -> env

  (** {3 Compiler} *)

  val term : env -> Cil_types.term -> term
  val pred : polarity -> env -> predicate -> pred
  val logic : env -> Cil_types.term -> logic
  val region : env -> Cil_types.term -> M.loc Memory.region

  val bootstrap_term : (env -> Cil_types.term -> term) -> unit
  val bootstrap_pred : (polarity -> env -> predicate -> pred) -> unit
  val bootstrap_logic : (env -> Cil_types.term -> logic) -> unit
  val bootstrap_region : (env -> Cil_types.term -> M.loc Memory.region) -> unit

  (** {3 Application} *)

  val call_fun : env
    -> tau
    -> logic_info
    -> logic_label list
    -> F.term list -> F.term

  val call_pred : env -> logic_info
    -> logic_label list
    -> F.term list -> F.pred

  (** {3 Logic Variable and ACSL Constants} *)

  val logic_var : env -> logic_var -> logic
  val logic_info : env -> logic_info -> pred option

  val has_ltype : logic_type -> term -> pred

  (** {3 Logic Lemmas} *)

  val lemma : logic_lemma -> dlemma

end
