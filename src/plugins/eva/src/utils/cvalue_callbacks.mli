(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Register actions to performed during the Eva analysis,
    with access to the states of the cvalue domain.
    This API is for internal use only, and may be modified or removed
    in a future version. Please contact us if you need to register callbacks
    to be executed during an Eva analysis. *)

(** Interface exported via Eva.ml. *)
include Cvalue_callbacks_sig.API

val register_statement_hook:
  (Callstack.t -> Cil_types.stmt -> state list -> unit) -> unit

val apply_at_start_hooks:
  unit -> unit
val apply_call_hooks:
  Callstack.t -> Cil_types.kernel_function -> state -> analysis_kind -> unit
val apply_call_results_hooks:
  Callstack.t -> Cil_types.kernel_function -> state -> call_results -> unit
val apply_statement_hooks:
  Callstack.t -> Cil_types.stmt -> state list -> unit
