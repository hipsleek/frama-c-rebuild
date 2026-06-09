(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Mt_memory.Types
open Mt_thread

(** A correct value for the main thread *)
val main_thread: Cil_types.kernel_function -> state -> thread_state


(** Exception to be returned when a hook did not process fully correctly, to be
    caught the level of hook registration. The [int] is the error code *)
exception Hook_failure of int

(** List of builtins that are to be registered by Mthread. We use
    a simplified interface compared to what the value analysis can
    require. The callbacks can raise [Hook_failure] if they did not
    proceed correctly *)
val mthread_builtins:
  (string *
   (analysis_state ->
    state ->
    (Eva_ast.exp * value) list ->
    state * value option)
  ) list


(** Function to register with [Cvalue_callbacks.register_call_hooks]
    (called before each function call processed in the analysis) *)
val catch_functions_calls: analysis_state -> Cvalue_callbacks.call_hook

(** Function to register with [Cvalue_callbacks.register_call_results_hook]
    (called after each function call processed by the analysis. *)
val catch_functions_record:
  analysis_state -> Cvalue_callbacks.call_results_hook
