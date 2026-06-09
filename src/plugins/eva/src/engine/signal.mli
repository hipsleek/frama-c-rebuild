(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Signal emission} *)

(** Mark the analysis as aborted: it will be stopped at the next safe point. *)
val abort: unit -> unit

(** Mark the analysis as killed: it will be stopped at the next safe point. *)
val kill: unit -> unit

(** Remove any previous mark from {!abort} or {!kill} if present. *)
val reset: unit -> unit

(** {2 Signal check} *)

(** Check for emitted signal.
    @raises Self.Abort if {!abort} have been called.
    @raises Sys.Break if {!kill} have been called.*)
val check: unit -> unit

(** {2 System signal} *)

(** Setup system signals:
    - On [SIGUSR1], [kill ()] is issued
    - On [SIGINT], [Sys.break] is raised.

    @return a function to restore previous system signal handlers. *)
val setup: unit -> (unit -> unit)

(** {2 Signal catching} *)

(** [protect f ~cleanup] runs [f]. On a user interruption or a Frama-C error,
    applies [cleanup]. This is used to clean up and save partial results when
    the analysis is aborted. *)
val protect: (unit -> 'a) -> cleanup:(unit -> unit) -> 'a
