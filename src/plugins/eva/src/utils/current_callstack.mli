(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Callstack tracking. *)

(** Returns the current callstack or None if it has not been initialized. *)
val get : unit -> Callstack.t option

(** Returns the current callstack; fails if it has not been initialized.
    This should only be called during the analysis of a function. *)
val get_exn : unit -> Callstack.t

(** [with_callstack ~finally cs job] creates a wrapper around [job] such that
    the wrapped executions happen in environment where the current callstack
    returned by {!get} is set to [cs]. When [job] returns or raises an
    exception, [finally] is called and then the current callstack is restored to
    its previous value. *)
val with_callstack :
  ?finally:(unit -> unit) -> Callstack.t -> ('a -> 'b) -> 'a -> 'b
