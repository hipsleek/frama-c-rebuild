(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Interactive Proof Engine *)
(* -------------------------------------------------------------------------- *)

(** A proof tree *)
type tree

(** A proof node *)
type node

module Node :
sig
  type t = node
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

val get : Wpo.t -> [ `Script | `Proof | `Saved | `None ]
val proof : main:Wpo.t -> tree

(** Consolidate and update status of the entire script (memoized). *)
val validate : tree -> unit

(** Consolidate proof tree (memoized). *)
val consolidate : Wpo.t -> unit

(** Consolidated statistics (memoized). *)
val consolidated : Wpo.t -> Stats.stats

(** Consolidated results (memoized). *)
val results : Wpo.t -> (Prover.t * VCS.result) list

(** Leaves are numbered from 0 to n-1 *)

type status = [
  | `Unproved (** proof obligation not proved *)
  | `Proved   (** proof obligation is proved *)
  | `Pending of int (** proof is pending *)
  | `Passed   (** smoke test is passed (PO is not proved) *)
  | `Invalid  (** smoke test has failed (PO is proved) *)
  | `StillResist of int (** proof is pending *)
]
type current = [ `Main | `Internal of node | `Leaf of int * node ]
type position = [ `Main | `Node of node | `Leaf of int ]

val pool : tree -> Lang.F.pool
val saved : tree -> bool
val set_saved : tree -> bool -> unit

val status : tree -> status
val current : tree -> current
val goto : tree -> position -> unit
val root : tree -> node
val main : tree -> Wpo.t
val goal : node -> Wpo.t
val tree : node -> tree
val head : tree -> node option
val head_goal : tree -> Wpo.t
val tree_context : tree -> WpContext.t
val node_context : node -> WpContext.t

val title : node -> string
val fully_proved : node -> bool
val locally_proved : node -> bool
val pending : node -> int
val stats : node -> Stats.stats
val depth : node -> int
val path : node -> node list
val parent : node -> node option
val tactic : node -> Tactical.t option
val child_label : node -> string option
val tactic_label : node -> string option
val subgoals : node -> node list
val children : node -> (string * node) list
val tactical : node -> ProofScript.jtactic option
val get_strategies : node -> int * Strategy.t array (* current index *)
val set_strategies : node -> ?index:int -> Strategy.t array -> unit
val get_hint : node -> string option
val set_hint : node -> string -> unit

val forward : tree -> unit
val clear_goal : Wpo.t -> unit
val clear_tree : tree -> unit
val clear_node : tree -> node -> unit
val clear_node_tactic : tree -> node -> unit
val clear_parent_tactic : tree -> node -> unit

val add_goal_hook : (Wpo.t -> unit) -> unit
val add_clear_hook : (Wpo.t -> unit) -> unit
val add_remove_hook : (node -> unit) -> unit
val add_update_hook : (node -> unit) -> unit

val cancel_parent_tactic : tree -> unit
val cancel_current_node : tree -> unit

type fork
val anchor : tree -> ?node:node -> unit -> node
val fork : tree -> anchor:node -> ProofScript.jtactic -> Tactical.process -> fork
val iter : (Wpo.t -> unit) -> fork -> unit
val commit : fork -> node * (string * node) list
val pretty : Format.formatter -> fork -> unit

val has_proof : Wpo.t -> bool
val has_script : tree -> bool
val script : tree -> ProofScript.jscript
val bind : node -> ProofScript.jscript -> unit
val bound : node -> ProofScript.jscript
