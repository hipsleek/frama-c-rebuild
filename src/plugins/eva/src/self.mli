(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.General_services

val proxy: State_builder.Proxy.t
val state: State.t

(** Return [true] iff the value analysis has been done. *)
val is_computed: unit -> bool

(** Clears the results of the Eva analysis. *)
val clear_results: unit -> unit

(** Computation state of the analysis. *)
type computation_state = NotComputed | Computing | Computed | Aborted

(** The current computation state of the analysis, updated by
    [force_compute] and states updates. *)
module ComputationState : State_builder.Ref with type data = computation_state

(** Exception used to cleanly abort the analysis (without killing Frama-C). *)
exception Abort

(** Debug categories responsible for printing initial and final states of Value.
    Enabled by default, but can be disabled via the command-line:
    -value-msg-key="-initial_state,-final_state" *)
val dkey_initial_state : category
val dkey_final_states : category
val dkey_summary : category

(** {2 Debug categories.} *)

(** Same as Log's {!register_category}, but [help] is mandatory, and a verbosity
    level can be associated to the category. *)
val register_category: ?level:int -> help:string -> string -> category

val dkey_show: category
val dkey_pointer_comparison: category
val dkey_cvalue_domain: category
val dkey_iterator : category
val dkey_widening : category
val dkey_partition : category
val dkey_split_return : category
val dkey_precision_settings : category
val dkey_progress : category
val dkey_callstacks : category
val dkey_include_string_literal: category

(** {2 Warning categories.} *)

(* Default status of warning categories: feedback is associated to a verbosity
   level. *)
type warn_default = Inactive | Feedback of int | Error

(** Same as Log's {!register_warn_category}, but [help] is mandatory and the
    [Feedback] default status is associated to a verbosity level. *)
val register_warn_category:
  help:string -> ?default:warn_default -> string -> warn_category

val wkey_alarm: warn_category
val wkey_locals_escaping: warn_category
val wkey_garbled_mix_write: warn_category
val wkey_garbled_mix_assigns: warn_category
val wkey_garbled_mix_summary: warn_category
val wkey_builtins_missing_spec: warn_category
val wkey_builtins_override: warn_category
val wkey_libc_unsupported_spec : warn_category
val wkey_loop_unroll_auto : warn_category
val wkey_loop_unroll_partial : warn_category
val wkey_missing_loop_unroll : warn_category
val wkey_missing_loop_unroll_for : warn_category
val wkey_signed_overflow : warn_category
val wkey_invalid_assigns : warn_category
val wkey_missing_assigns : warn_category
val wkey_missing_assigns_result : warn_category
val wkey_experimental : warn_category
val wkey_unknown_size : warn_category
val wkey_ensures_false : warn_category
val wkey_watchpoint : warn_category
val wkey_recursion : warn_category
val wkey_acsl : warn_category
val wkey_acsl_unsupported : warn_category

(** {2 Logging.} *)

(** This modules adapt the interface of {!Log.Messages} to be usable with
    {!Position.t}.

    If [position] is given, then message will be located at this position.
    Otherwise if [current] or [source] are given, then the current position
    tracked by the kernel or the given location will respectively be used.
    [stacktrace] optional parameter controls whether the call stack must
    be printed at the end of the message and always default to false.

    See {!Log.Messages} for documentation *)

type 'a pretty_printer =
  ?emitwith:(Log.event -> unit) -> ?once:bool ->
  ?pos:Position.t -> ?current:bool -> ?source:Filepos.t ->
  ?stacktrace:bool ->  ?append:(Format.formatter -> unit) -> ?echo:bool ->
  ('a,Format.formatter,unit) format -> 'a

type ('a,'b) pretty_aborter =
  ?pos:Position.t -> ?current:bool -> ?source:Filepos.t ->
  ?stacktrace:bool ->  ?append:(Format.formatter -> unit) -> ?echo:bool ->
  ('a,Format.formatter,unit,'b) format4 -> 'a

(** Results of analysis. *)
val result : ?level:int -> ?dkey:category -> 'a pretty_printer

(** Progress and feedback. *)
val feedback : ?ontty:Log.ontty -> ?level:int -> ?dkey:category -> 'a pretty_printer

(** Debugging information. *)
val debug : ?level:int -> ?dkey:category -> 'a pretty_printer

(** Warnings. *)
val warning : ?wkey:warn_category -> 'a pretty_printer

(** Alarm emitted by the analysis. *)
val alarm : 'a pretty_printer

(** User error. *)
val error : 'a pretty_printer

(** User error stopping the plugin. *)
val abort : ('a,'b) pretty_aborter

(** Internal error of the plug-in. *)
val failure : 'a pretty_printer

(** Internal error stopping the plug-in. *)
val fatal   : ('a,'b) pretty_aborter


(** Called at the beginning of the analysis to configure Eva verbosity,
    by automatically enabling/disabling message keys. *)
val configure_verbosity : unit -> unit
