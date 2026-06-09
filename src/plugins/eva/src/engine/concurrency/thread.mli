(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Thread id} *)

include Datatype.S_with_collections

val main : t
val is_main : t -> bool

val id : t -> int
val label : t -> string
val find : int -> t option
val from_callstack : Callstack.t -> t
val from_local_position : Position.local -> t
val from_position : Position.t -> t

(** [spawn al name kf args] registers the creation of a thread encountered
    in Eva analysis, and either add this spawn to an existing thread analysis
    or create a new thread analysis.
    @param al the stmt and callstack of the thread creation
    @param name an optional name often defined by the memory location where the
         thread identifier will be stored
    @param kf the entry point for the new thread
    @param args the list of arguments used for the thread invocation *)
val spawn :
  Position.local ->
  Concurrency.Name.t option ->
  Cil_types.kernel_function ->
  Cvalue.V.t list ->
  t

(** [is_interrupt_handler kf] returns [true] if [kf] has been registered as an
    interrupt handler. *)
val is_interrupt_handler : Cil_types.kernel_function -> bool

(** [interrupt_handler kf] returns the thread representing the interrupt
    handler [kf]. *)
val interrupt_handler : Cil_types.kernel_function -> t

(** [interrupt_handlers ()] returns the threads representing the registered
    interrupt handlers. *)
val interrupt_handlers : unit -> t list

(** {2 Internal state of the current analysis } *)

(** Register a set of functions serving as interrupt handlers. *)
val register_interrupt_handlers : Kernel_function.Set.t -> unit

val reset_state : unit -> unit

type properties = {
  entry_point : Cil_types.kernel_function;
  spawn_points : Position.Local.Set.t;
  arguments : (Cil_types.varinfo * Cvalue.V.t) list;
}

val properties : t -> properties
val entry_point : t -> Kernel_function.t
