(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Summary of an Mthread analysis, used by server requests for the dedicated
    component in Ivette. *)

(** Computes the summary from an analysis state. *)
val compute : Mt_thread.analysis_state -> unit

(** Clears summary. *)
val clear : unit -> unit


(** Summary for each thread. *)

type mutex_summary = {
  taken : Mutex.Set.t; (** Set of locks taken. *)
  released : Mutex.Set.t; (** Set of locks released. *)
}

type queue_summary = {
  created : Mqueue.Set.t; (** Set of message queues created. *)
  receivers : Mqueue.Set.t; (** Set of message queues receiving some message. *)
  senders : Mqueue.Set.t; (** Set of message queues sending some messages. *)
}

type shared_var_summary = {
  read : Memory_zone.Set.t; (** Shared locations read. *)
  written : Memory_zone.Set.t (** Shared locations written. *)
}

type thread_summary = {
  locks : mutex_summary;
  mqueues : queue_summary;
  shared_vars : shared_var_summary;
}

(** Table binding each analyzed thread to its summary. *)
module ThreadTable : State_builder.Hashtbl with type key = Thread.t
                                            and type data = thread_summary


(** Summary of accesses to shared memory. *)

(** An access is a combination between a zone accessed, a kind of access (read,
    write) and a protection status. *)
type access

(** Memory zone of an access. *)
val access_zone: access -> Memory_zone.t

(** Kind of an access: read or write. *)
val access_kind: access -> Mt_shared_vars_types.access_kind

(** Mutex protection of an access. *)
val access_protection: access -> Mt_shared_vars_types.protection

(** Unique id of an access. *)
val access_id: access -> string

(** Table binding each access to the set of source code locations where it occurs. *)
module AccessTable : State_builder.Hashtbl
  with type key = access
   and type data = Cil_datatype.Stmt.Set.t
