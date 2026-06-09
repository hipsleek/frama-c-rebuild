(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Stored messages for persistence between sessions. *)

val iter: (Log.event -> unit) -> unit
(** Iter over all stored messages. The messages are passed in emission order. *)

val fold: ('a -> Log.event -> 'a) -> 'a -> 'a
(** Fold over all stored messages. The messages are passed in emission order. *)

val dump_messages: unit -> unit
(** Dump stored messages to standard channels *)

val self: State.t
(** Internal state of stored messages *)

val reset_once_flag : unit -> unit
(** Reset the [once] flag of pretty-printers. Messages already printed
    will be printed again.
    @since Boron-20100401 *)

val nb_errors: unit -> int
val nb_warnings: unit -> int
val nb_messages: unit -> int
(** Number of stored warning messages, error messages, or all
    messages.*)

val add_hook: (Log.event * int -> unit) -> unit
(** Register a global hook (not projectified) called on each message addition,
    with the new event and its index (counting from 0). *)
