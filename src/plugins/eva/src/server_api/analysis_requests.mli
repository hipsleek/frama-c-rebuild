(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Registers a function called each time the computation state of Eva changes. *)
val register_computation_hook: (unit -> unit) -> unit

(** Signal emitted each time the computation state of Eva changes. *)
val computation_signal: Server.Request.signal
