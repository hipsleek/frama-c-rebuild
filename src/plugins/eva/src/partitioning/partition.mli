(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A partition is a collection of states, each identified by a unique key.
    The keys define the states partition: states with identical keys are joined
    together, while states with different keys are maintained separate.
    A key contains the reason for which a state must be kept separate from
    others, or joined with similar states.

    Partitioning actions allow updating the keys or splitting some states to
    define or change the partition. Actions are applied to flows, in which
    states with the same key are *not* automatically joined. This allows
    applying multiple actions before recomputing the partitions. Flows can then
    be converted into partitions, thus merging states with identical keys.

    Flows are used to transfer states from one partition to another. Transfer
    functions can be applied to flows; keys are maintained through transfer
    functions, until partitioning actions update them.  *)

(** {2 Keys and partitions.} *)

type branch =
  | Branch of int
  (** Junction branch id in the control flow *)
  | Builtin_result of Kernel_function.t * Cil_datatype.Kinstr.t * int
  (** Case of a builtin *)
  | Spec_behavior of Kernel_function.t * Cil_datatype.Kinstr.t * int
  (** Behavior of a spec *)
  | Disjunction_case of Cil_datatype.Stmt.t * int
  (** Case of a disjunction in an ACSL annotation *)

(** Partitioning keys attached to states. *)
type key

type call_return_policy = {
  callee_splits: bool;
  callee_history: bool;
  caller_history: bool;
  history_size: int;
}

module Key : sig
  include Datatype.S_with_collections with type t = key

  val empty : t
  (** Initial key: no partitioning. *)

  val add_branch : ?history_size:int -> branch -> t -> t
  (** Key for a branch appended to an existing key. *)

  val exceed_rationing: t -> bool
  val combine : policy:call_return_policy -> caller:t -> callee:t -> t
  (** Recombinaison of keys after a call. *)
end

(** Collection of states, each identified by a unique key. *)
type 'state partition

val empty : 'a partition
val is_empty : 'a partition -> bool
val size : 'a partition -> int
val to_list : 'a partition -> (key*'a) list
val find : key -> 'a partition -> 'a
val replace : key -> 'a -> 'a partition -> 'a partition
val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a partition ->
  'b partition -> 'c partition
val iter : (key -> 'a -> unit) -> 'a partition -> unit
val filter : (key -> 'a -> bool) -> 'a partition -> 'a partition
val map : ('a  -> 'a) -> 'a partition -> 'a partition


(** {2 Partitioning actions.} *)

(** Rationing are used to keep separate the [n] first states propagated at
    a point, by creating unique stamp until the limit is reached.
    Implementation of the option -eva-slevel. *)
type rationing

(** Creates a new rationing, that can be used successively on several flows. *)
val new_rationing: limit:int -> merge:bool -> rationing

(** The unroll limit of a loop. *)
type unroll_limit =
  | ExpLimit of Cil_types.exp
  (** Value of the expression for each incoming state. The expression must
      evaluate to a singleton integer in each state.  *)
  | IntLimit of int
  (** Integer limit. *)
  | AutoUnroll of Eva_automata.loop * int * int
  (** [AutoUnroll(loop, min, max)] requests to find a "good" unrolling limit
      between [min] and [max] for the loop [loop]. *)

(** Splits on an expression can be static or dynamic:
    - static splits are processed once: the expression is only evaluated at the
      split point, and the key is then kept unchanged until a merge.
    - dynamic splits are regularly redone: the expression is re-evaluated, and
      states are then split or merged accordingly. *)
type split_kind = Eva_annotations.split_kind = Static | Dynamic

(* Same as Eva_annotations.split_term but with Eva_ast. *)
type split_term =
  | Expression of Eva_ast.Exp.t
  | Predicate of Cil_datatype.PredicateStructEq.t

(** Split monitor: prevents splits from generating too many states. *)
type split_monitor

(** Creates a new monitor that allows to split up to [limit] states according
    to [term] evaluation. *)
val new_monitor:
  limit:int ->
  kind:split_kind ->
  term:split_term ->
  loc:Cil_types.location ->
  split_monitor

(** These actions redefine the partitioning by updating keys or splitting
    states. They are applied to all the pair (key, state) in a flow. *)
type action =
  | Enter_loop of unroll_limit * Eva_automata.loop
  (** Enters a loop in which the n first iterations will be kept separate:
      creates an iteration counter at 0 for each states in the flow; states at
      different iterations will be kept separate, until reaching the
      [unroll_limit]. Counters are incremented by the [Incr_loop] action. *)
  | Leave_loop
  (** Leaves the current loop: removes its iteration counter. States that were
      kept separate only by this iteration counter will be joined together. *)
  | Incr_loop
  (** Increments the iteration counter of the current loop for all states in
      the flow. States with different iteration counter are kept separate. *)
  | Add_branch of int * int
  (** Identifies all the states in the flow as coming from the branch identified
      by the first integer. They will be kept separated from states coming from
      other branches. The second integer is the maximum number of successive
      branches kept in the keys: this action also removes the oldest branches
      from the keys to meet this constraint. *)
  | Ration of rationing
  (** Ensures that the first states encountered are kept separate, by creating a
      unique ration stamp for each new state until the [limit] is reached. The
      same rationing can be used on multiple flows. Applying a new rationing
      replaces the previous one.
      If the rationing has been created with [merge:true], all the states from
      each flow receive the same stamp, but states from different flows receive
      different stamps, until [limit] states have been tagged. *)
  | Restrict of Eva_ast.exp * Z.t list
  (** [Restrict (exp, list)] restricts the rationing according to the evaluation
      of the expression [exp]:
      – for each integer [i] in [list], states in which [exp] evaluates exactly
        to the singleton [i] receive the same unique stamp, and will thus be
        joined together but kept separate from other states;
      – all other states are joined together.
      Previous rationing is erased and replaced by this new stamping.
      Implementation of the option -eva-split-return. *)
  | Split of split_monitor
  (** If [monitor] has been built as [new_monitor ~limit ~kind ~term] then
      [Split monitor] tries to separate states such as the [term] evaluates
      to a singleton value in each state in the flow. If necessary and
      possible, splits states into multiple states. States in which the [term]
      evaluates to different values will be kept separate. Gives up the split
      if [term] evaluates to more than [limit] values. A same monitor can
      be used for successive splits on different flows. *)
  | Merge of split_term
  (** Forgets the split of an expression: states that were kept separate only
      by the split of this expression will be joined together. *)
  | SyntacticSplit of int * int
  (** Record that the state attached to this key have been obtained after
      taking a if-then-else or switch branch - identified by the vertex id
      of the split and the edge id of the branch taken. *)
  | MergeSyntacticSplits
  (** Forget every syntactic split. *)
  | Update_dynamic_splits
  (** Updates dynamic splits by evaluating the expression and splitting the
      states accordingly. *)

exception InvalidAction


(** {2 Flows.} *)

(** Flows are used to transfer states from one partition to another, by
    applying transfer functions and partitioning actions. They do not enforce
    the unicity of keys. *)
module MakeFlow (Abstract: Engine_abstractions_sig.S) :
sig
  type state = Abstract.Dom.t
  type t

  val empty : t

  val initial : state list -> t
  val to_list : t -> (key * state) list
  val of_partition : state partition -> t
  val to_partition : t -> state partition

  val is_empty : t -> bool
  val size : t -> int

  val union : t -> t -> t

  val transfer : ((key * state) -> (key * state) list) -> t -> t
  val transfer_keys : t -> action -> t

  val iter : (key -> state -> unit) -> t -> unit
  val filter_map: (key -> state -> state option) -> t -> t

  val join_duplicate_keys: t -> t
end
