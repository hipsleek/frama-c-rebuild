(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type mode = NoCache | Update | Replay | Rebuild | Offline | Cleanup

val set_mode : mode -> unit
val get_mode : unit -> mode
val add_hook_on_mode_update: (unit -> unit) -> unit


val get_hits : unit -> int
val get_miss : unit -> int
val get_removed : unit -> int

val is_active : mode -> bool
val is_updating : mode -> bool

val cleanup_cache : unit -> unit

type 'a digest = Why3Provers.t -> 'a -> string

type 'a runner =
  timeout:float option -> steplimit:int option -> Why3Provers.t -> 'a ->
  VCS.result Task.task

val promote: ?timeout:float -> ?steplimit:int -> VCS.result -> VCS.result
(** Converts some known results to the given limits.
    In particular, if the result shall be discarded with respect to the limits,
    the function returns [VCS.no_result]. *)

val get_result: digest:('a digest) -> runner:('a runner) -> 'a runner
val clear_result: digest:('a digest) -> Why3Provers.t -> 'a -> unit

(**************************************************************************)
