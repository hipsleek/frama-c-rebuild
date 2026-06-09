(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Summary } *)

type alarm_category =
  | Division_by_zero
  | Memory_access
  | Index_out_of_bound
  | Unaligned_pointer
  | Invalid_shift
  | Overflow
  | Uninitialized
  | Dangling
  | Nan_or_infinite
  | Float_to_int
  | Other

type coverage =
  { mutable reachable: int;
    mutable dead: int; }

type statuses =
  { mutable valid: int;
    mutable unknown: int;
    mutable invalid: int; }

type events =
  { mutable errors: int;
    mutable warnings: int; }

type alarms = (alarm_category * int) list (* Alarm count for each category *)

type fun_stats =
  { fun_coverage: coverage;
    fun_alarm_count: alarms;
    fun_alarm_statuses: statuses; }

type program_stats =
  { prog_fun_coverage: coverage;
    prog_stmt_coverage: coverage;
    prog_alarms: alarms;
    eva_events: events;
    kernel_events: events;
    alarms_statuses: statuses;
    assertions_statuses: statuses;
    preconds_statuses: statuses; }

module FunctionStats :
sig
  type key = Kernel_function.t
  type data = fun_stats

  (** Iterate on every function statistics *)
  val iter: (key -> data -> unit) -> unit

  (** Trigger the recomputation of function stats *)
  val recompute: key -> unit

  (* Trigger the recomputation of all function stats. *)
  val recompute_all: unit -> unit

  (** Set a hook on function statistics computation *)
  val add_hook_on_change:
    ((key, data) State_builder.hashtbl_event -> unit) -> unit

  module Datatype: Datatype.S

  (** Set a hook on statistics changes by the project library *)
  val add_hook_on_update: (Datatype.t -> unit) -> unit
end

(** Compute analysis statistics. *)
val compute_stats: unit -> program_stats

(** Prints a summary of the analysis. *)
val print: unit -> unit
