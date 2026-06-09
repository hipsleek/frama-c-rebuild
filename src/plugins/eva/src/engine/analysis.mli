(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val compute : unit -> unit
(** Computes the Eva analysis, if not already computed, using the entry point
    of the current project. You may set it with {!Globals.set_entry_point}.
    @raise Globals.No_such_entry_point if the entry point is incorrect
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val is_computed : unit -> bool
(** Return [true] iff the Eva analysis has been done.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)

val abort : unit -> unit
(** Cleanly abort the analysis at the next safe point: partial results will be
    saved and Frama-C is not killed. *)

val self : State.t
(** Internal state of Eva analysis from projects viewpoint. *)

type computation_state = NotComputed | Computing | Computed | Aborted
(** Computation state of the analysis. *)

val current_computation_state : unit -> computation_state
(** Get the current computation state of the analysis, updated by
    [force_compute] and states updates. *)

val register_computation_hook: ?on:computation_state ->
  (computation_state -> unit) -> unit
(** Registers a hook that will be called each time the analysis starts or
    finishes. If [on] is given, the hook will only be called when the
    analysis switches to this specific state. *)

val emitter: Emitter.t
(** Emitter used by Eva to emit property statuses. *)

(** Kind of results for the analysis of a function body. *)
type results =
  | Complete
  (** The results are complete: they cover all possible call contexts of the
      given function. *)
  | Partial
  (** The results are partial, as the functions has not been analyzed in all
      possible call contexts. This happens for recursive functions that are
      not completely unrolled, or if the analysis has stopped unexpectedly. *)
  | NoResults
  (** No results were saved for the function, due to option -eva-no-results.
      Any request at a statement of this function will lead to a Top result. *)

(* Analysis status of a function. *)
type status =
  | Unreachable
  (** The function has not been reached by the analysis. Any request in this
      function will lead to a Bottom result. *)
  | SpecUsed
  (** The function specification has been used to interpret its calls:
      its body has not been analyzed. Any request at a statement of this
      function will lead to a Bottom result. *)
  | Builtin of string
  (** The builtin of the given name has been used to interpret the function:
      its body has not been analyzed. Any request at a statement of this
      function will lead to a Bottom result. *)
  | Analyzed of results
  (** The function body has been analyzed. *)

(** Returns the analysis status of a given function. *)
val status: Cil_types.kernel_function -> status

(** Does the analysis ignores the body of a given function, and uses instead
    its specification or a builtin to interpret it?
    Please use {!Eva.Results.are_available} instead to known whether results
    are available for a given function. *)
val use_spec_instead_of_definition: Cil_types.kernel_function -> bool

(** Returns [true] if the user has requested that no results should be recorded
    for the given function. Please use {!Eva.Results.are_available} instead
    to known whether results are available for a given function. *)
val save_results: Cil_types.kernel_function -> bool
