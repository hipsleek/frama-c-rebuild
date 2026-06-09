(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type id_type

type raw_id = id_type * int
val pretty_raw_id : Format.formatter -> raw_id -> unit

val read_id_state : Mt_memory.Types.state -> raw_id -> Mt_memory.Types.value
val read_id_state_enumerate :
  int -> Mt_memory.Types.state -> raw_id -> int list Mt_memory.conversion
val write_id_state :
  Mt_memory.Types.state -> raw_id -> int -> Mt_memory.Types.state
val replace_id_value :
  Mt_memory.Types.state ->
  raw_id -> before:int -> after:int -> Mt_memory.Types.state

val of_thread : Thread.t -> raw_id
val of_mutex: Mutex.t -> raw_id
val of_queue: Mqueue.t -> raw_id
