(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Mt_memory.Types

type id_type = IdThread | IdMutex | IdQueue

let to_string = function
  | IdThread -> "thread"
  | IdMutex -> "mutex"
  | IdQueue -> "queue"

type raw_id = id_type * int

let pretty_raw_id fmt (idt, offset) =
  Format.fprintf fmt "%s_%d" (to_string idt) offset


let array_of_idt = function
  | IdThread -> Mt_lib.array_threads ()
  | IdMutex -> Mt_lib.array_mutexes ()
  | IdQueue -> Mt_lib.array_queues ()

let pointer_of_id ((idt, offset): raw_id) : pointer =
  assert (offset > 0);
  let array = array_of_idt idt
  and offset = (offset - 1) * (Machine.Sizeof.int ())
  (* Let us not lose the first cell of the array *)
  in
  array, offset


let read_id_state state raw_id =
  let p = pointer_of_id raw_id in
  Mt_memory.read_int_pointer p state

let read_id_state_enumerate cardinal state raw_id =
  read_id_state state raw_id |>
  Mt_memory.extract_int_list ~cardinal |>
  Result.map_error
    (fun s -> Format.asprintf "Id %a contains %s" pretty_raw_id raw_id s)


let write_id_state state raw_id v =
  let p = pointer_of_id raw_id in
  Mt_memory.write_int_pointer p v state

let replace_id_value state raw_id ~before ~after =
  let p = pointer_of_id raw_id in
  Mt_memory.replace_value_at_int_pointer p ~before ~after state

let of_thread th = IdThread, Thread.id th
let of_mutex m = IdMutex, Mutex.id m
let of_queue q = IdQueue, Mqueue.id q
