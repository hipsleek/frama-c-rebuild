(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Frama-C main interface.
    @since Lithium-20081201
    @before 29.0-Copper it was in a module named Db
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
module Main : sig
  val extend : (unit -> unit) -> unit
  (** Register a function to be called by the Frama-C main entry point.
      @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
end

val play_analysis : unit -> unit
(** Run all the Frama-C analyses. This function should be called only by
    toplevels.
    @since 29.0-Copper
*)

val boot : unit -> unit
(** Start and define the Frama-C kernel main loop. *)

val set_toplevel: ((unit -> unit) -> unit) -> unit
(** Changes the toplevel function to run on boot
    @since 29.0-Copper
    @before 29.0-Copper it was provided in a different way in Db.Toplevel
*)
