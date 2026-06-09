(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Variable __fc_mthread_threads in mthread.c *)
val array_threads : unit -> Cil_types.varinfo

(** Variable __fc_mthread_mutexes in mthread.c *)
val array_mutexes : unit -> Cil_types.varinfo

(** Variable __fc_mthread_queues in mthread.c *)
val array_queues : unit -> Cil_types.varinfo

(** Variable __fc_mthread_threads_running in mthread.c *)
val var_thread_created : unit -> Cil_types.varinfo

(** Checks that all variables above are in the source files. *)
val check_mthread_library : unit -> unit

(** Threading library stubbed by Mthread. *)
type threads_lib =
  | BuiltinsOnly (** Only Mthread built-ins are available. *)
  | Pthreads (** Pthreads stubs and Mthread built-ins. *)

(** Load the given threads library into Frama-C. *)
val load_threads_library : threads_lib -> unit

(** Aborts if [kf] is from a library whose stubs have not been loaded;
    warn if [kf] is an unsupported function from a loaded library. *)
val warn_on_unsupported_library_function : Cil_types.kernel_function -> unit
