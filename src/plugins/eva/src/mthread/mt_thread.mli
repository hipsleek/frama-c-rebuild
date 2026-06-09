(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Mt_memory.Types
open Mt_cil
open Mt_types
open Mt_shared_vars_types
open Mt_cfg_types


type recompute_reason =
  | FirstIteration
  | NewMsgReceived
  | PotentialSharedVarsChanged
  | SharedVarsValuesChanged
  | InitialArgsChanged
  | InitialEnvChanged
  | InterferencesChanged
;;

module RecomputeReason: sig
  type t = recompute_reason
  val compare: t -> t -> int
  val pretty: t Pretty.aformatter
end

module SetRecomputeReason: sig
  include Set.S with type elt = recompute_reason
  val pretty: t Pretty.aformatter
end

type priority =
  | PDefault (** No priority specified, but it is possible to specify one *)
  | PUnknown (** Contradictory priorities specified *)
  | PPriority of int (** Known priority *)

module Priority: Datatype.S with type t = priority


type thread = Thread.t

(** The representation of a thread *)
type thread_state = {
  th_eva_thread : Thread.t; (* Thread as represented in Eva's engine*)
  th_parent : thread_state option (** Thread in which the thread is created. [None]
                                      for the root thread *);
  th_fun : kernel_function (** Function which the thread executes *);
  th_stack : Callstack.t
(** Call stack resulting in the creation of the thread *);

  mutable th_init_state : Cvalue.Model.t
(** Memory state at the moment the thread is created *);

  mutable th_params : Cvalue.V.t list
(** Arguments to the the thread function *);

  mutable th_amap: Trace.t (** map interesting statements to sets
                               concurrent actions with their call stacks *);

  mutable th_to_recompute: SetRecomputeReason.t
(** Does this thread needs to be recomputed on the next iteration *);

  mutable th_read_written: AccessesByZone.map
(** Globals read and written by the thread, and at which statement *);

  mutable th_cfg : CfgNode.t (** Cfg for the current thread *);

  mutable th_read_written_cfg: AccessesByZoneNode.map
(** Globals read and written by the thread, and at which node
    in the cfg*);

  mutable th_values_written: Mt_memory.Types.state
(** Join of all the values written by this thread in shared locations.
    Currently not contextual *);

  mutable th_priority: priority
(** determines which threads execute without the possibility of being
    preempted by another thread. *);
}

module ThreadState : sig
  type t = thread_state

  val is_main: t -> bool

  (** The name of the thread *)
  val label: t -> string

  (** Prints the name of the thread *)
  val pretty: t Pretty_utils.formatter

  (** Prints the name of the thread with detailed information *)
  val pretty_detailed: t Pretty_utils.formatter

  (** Equality based on thread id *)
  val equal: t -> t -> bool

  (** [one_creates_other th1 th2] returns [`Creates (th1, th2)]
      if [th1] creates [th2] directly or through another threads,
      [`Creates(th2, th1)] if [th2] creates [th1], and [`Unrelated]
      otherwise *)
  val one_creates_other: t -> t -> [`Creates of t * t | `Unrelated]

  val recompute_because: t -> recompute_reason -> unit

  (** Does a thread need to be recomputed? If not and if [feedback] is true,
      prints a debug or feedback message explaining why the thread should not
      be recomputed. *)
  val needs_recomputation: ?feedback:bool -> t -> bool
end


type analysis_state = {
  all_threads : thread_state Thread.Hashtbl.t
(** List of all threads. Is kept (and can thus increase) from one
    iteration to the next *);

  mutable all_mutexes: Mutex.Set.t; (** Set of all mutexes of the analysis *)

  mutable all_queues: Mqueue.Set.t; (** Set of all queues of the analysis *)

  mutable iteration: int (** Current iteration of the analysis *);

  mutable main_thread: thread_state (** Starting thread *);

  mutable curr_thread: thread_state (** Thread currently running. *);

  mutable curr_events_stack: Trace.t list (** Mthread events that have been
                                              found during the current analysis of the current thread. The list
                                              has the same height as [curr_stack]. The top of the list is the trace
                                              containing the events for the function being analyzed by Value, and
                                              so on until the top of the list. When the list is popped, the events
                                              of the callee are merged inside the trace of the caller. *);

  mutable memexec_cache: Trace.t Datatype.Int.Hashtbl.t
(** Cache for the results obtained during the analysis of the current
    thread *);

  mutable curr_stack: Callstack.t
(** stack of a multithread event. Asynchronously set by a callback and used
    by another, because of a slightly too restricted signature in the
    value analysis. *);

  mutable concurrent_accesses: Memory_zone.t
(** Shared variables that have been detected in the analysis so far,
    with the crude analysis. Updated at the end of an iteration,
    and used to reach the fixpoint *);

  mutable precise_concurrent_accesses: Memory_zone.t
(** Really shared variables that have been detected in the analysis so far,
    Subset of the previous field *);

  mutable concurrent_accesses_by_nodes:
    (Memory_zone.t * SetNodeIdAccess.t) list
(** List of concurrent accesses that have been found. Used to
    compute the field [precise_concurrent_accesses] *);
}

val threads: analysis_state -> thread_state list
val thread_state: analysis_state -> thread -> thread_state
val fold_threads: analysis_state -> 'a -> (thread_state -> 'a -> 'a) -> 'a
val iter_threads: analysis_state -> (thread_state -> unit) -> unit

val calling_ki: analysis_state -> kinstr
val current_fun: analysis_state -> kernel_function

val curr_events: analysis_state -> Trace.t

val register_event: analysis_state -> ?top:stack_elt -> event -> unit
val register_memory_states:
  analysis_state -> before:functions_states -> after:functions_states -> unit
val register_multiple_events:
  analysis_state -> Trace.t -> unit

val push_function_call: analysis_state -> unit
val pop_function_call: analysis_state -> unit


val pretty_recompute_reasons: analysis_state Pretty_utils.formatter

val needs_recomputation: analysis_state -> bool

module OrderedThreads : sig
  val family_tree: analysis_state -> thread list Thread.Hashtbl.t
  (** Create a table mapping each thread that creates a thread
      to the threads it creates *)

  val creation_map: analysis_state -> Thread.Set.t Thread.Map.t
  (** Map each existing threads to the id of the threads it recursively
      creates *)

  val ordered_fold : ('a -> thread -> 'a) -> 'a -> analysis_state -> 'a
  (** Fold a function f with accumulator acc
      over program threads following the partial order of thread creations.
  *)

  val ordered_iter : analysis_state -> (thread -> 'a -> 'a) -> 'a -> unit
  (** Iter a function f over program threads following the partial order of
      thread creations. The ['a] argument passed to the function is the
      value returned by the function on the creating thread
  *)
end
