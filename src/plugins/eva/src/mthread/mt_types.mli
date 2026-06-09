(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Mt_cil
open Mt_memory.Types

(** Kind of access to zones *)

type rw = Read | Write of Locations.t
        | ReadPos of Position.t | WritePos of Position.t
module RW: sig
  include Datatype.S with type t = rw

  val loc : t -> location
  (** [loc op] returns the source location of the operation *)

  val is_read : t -> bool
  (** [is_read op] returns true if [op] is a read operation. *)

  val pretty_op : Format.formatter -> t -> unit
  (** Pretty-print the name of the operation (i.e. "read" or "write"). *)

  val pretty_loc : Format.formatter -> t -> unit
  (** Pretty-print the source location of the operation and its callstack if
      applicable. *)
end


(** Multithread events *)

type event =
  | CreateThread of Thread.t
  | StartThread of Thread.t
  | SuspendThread of Thread.t
  | CancelThread of Thread.t
  | ThreadExit of value
  | MutexLock of Mutex.t
  | MutexRelease of Mutex.t
  | CreateQueue of Mqueue.t * int option
  | SendMsg of Mqueue.t * (slice * int)
  (** [SendMsg(q, (msg, size))]
      - [q]: the queue
      - [msg]: content of the message
      - [size]: size of the message
  *)
  | ReceiveMsg of Mqueue.t * pointer * int
  (** [ReceiveMsg(q, ptr, size)]
      - [q]: the queue
      - [ptr]: loc to which the message must be written
      - [size]: max size to read
  *)
  | VarAccess of rw * Memory_zone.t (** Access to some shared variables *)
  | Dummy of string * value list


module Event : sig
  type t = event
  val equal: t -> t -> bool
  val hash: t -> int
  val pretty: t Pretty_utils.formatter
end


(** Maps from statements to multithread events, together with the context
    in which they occur *)

module EventsSet : sig
  include Set.S with type elt = event

  val pretty: ?sep:Pretty_utils.sformat -> unit -> Format.formatter -> t -> unit
  val threads_created : t -> Thread.t list
end
type events_set = EventsSet.t


(** Execution trace, mapping execution stacks to sets of events occurring
    at this point *)
module Trace : sig

  type t

  type data = private {
    trace_events: events_set;
    trace_states: state Cil_datatype.Stmt.Map.t; (* ??? *)
    trace_states_after: state Cil_datatype.Stmt.Map.t;
  }

  val empty : t
  val is_empty : t -> bool

  val add_event: t -> stack_elt -> event -> t
  val add_states: t -> before:functions_states -> after:functions_states -> t
  val add_prefix: stack_elt -> t -> t

  val find_at_stmt: t -> stmt -> (stack_elt * t) list

  val subtrace_at_call: t -> stack_elt -> t

  val at_root : t ->                 data option
  val at_call:  t -> stack_elt ->    data option

  val union: t -> t -> t

  val iter : t -> (Stack.t -> event -> unit) -> unit
  val iter' : t -> (event -> unit) -> unit
  val fold : t -> (Stack.t -> event -> 'a -> 'a) -> 'a -> 'a
  val fold' : t -> (event -> 'a -> 'a) -> 'a -> 'a
  val exists : t -> (Stack.t -> event -> bool) -> bool

  val find_events : (event -> bool) -> t -> events_set

  val pretty : Format.formatter -> t -> unit

  val no_deep_call: t -> bool

end


(** Live threads/taken mutexes at a given point of execution *)

type presence_flag = NotPresent | Present | MaybePresent

module type Presence = sig
  type key
  type t

  module KeySet: Datatype.Set with type elt = key

  val pretty: t Pretty_utils.formatter

  val equal: t -> t -> bool
  val hash: t -> int
  val compare: t -> t -> int

  val empty: t
  val is_empty: t -> bool

  val find: t -> key -> presence_flag

  val add: key -> presence_flag -> t -> t

  val combine: t -> t -> t

  (** Returns only [Present] keys. *)
  val only_present: t -> KeySet.t

  (** Returns [Present] and [MaybePresent] keys. *)
  val all_present: t -> KeySet.t
end

module ThreadPresence: Presence
  with type key = Thread.t
   and module KeySet = Thread.Set

module MutexPresence: Presence
  with type key = Mutex.t
   and module KeySet = Mutex.Set
