(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Server Main Process *)
(* -------------------------------------------------------------------------- *)

type json = Json.t
type kind = [ `GET | `SET | `EXEC ]
val string_of_kind : kind -> string
val pp_kind : Format.formatter -> kind -> unit

(* -------------------------------------------------------------------------- *)
(** {2 Request Registry} *)
(* -------------------------------------------------------------------------- *)

val register : kind -> string -> (json -> json) -> unit
val find : string -> (kind * (json -> json)) option
val exec : string -> json -> json (** @raises Not_found if not registered. *)

(* -------------------------------------------------------------------------- *)
(** {2 Signals Registry} *)
(* -------------------------------------------------------------------------- *)

type signal
val signal : string -> signal
val signal_name : signal -> string

(* -------------------------------------------------------------------------- *)
(** {2 Server Main Process} *)
(* -------------------------------------------------------------------------- *)

(** Type of request messages.
    Parametrized by the type of request identifiers. *)
type 'a request = [
  | `Poll
  | `Request of 'a * string * json
  | `Kill of 'a
  | `SigOn of string
  | `SigOff of string
  | `Shutdown
]

(** Type of response messages.
    Parametrized by the type of request identifiers. *)
type 'a response = [
  | `Data of 'a * json
  | `Error of 'a * string
  | `Killed of 'a
  | `Rejected of 'a
  | `Signal of string
  | `CmdLineOn
  | `CmdLineOff
]

(** A paired request-response message.
    The callback will be called exactly once for each received message. *)
type 'a message = {
  requests : 'a request list ;
  callback : 'a response list -> unit ;
}

type 'a server

(** Run a server with the provided low-level network primitives to actually
    exchange data. Logs are monitored unless [~logs:false] is specified.

    Default equality is the standard `(=)` one. *)
val create :
  pretty:(Format.formatter -> 'a -> unit) ->
  ?equal:('a -> 'a -> bool) ->
  fetch:(unit -> 'a message option) ->
  unit -> 'a server

(** Start the server in background.

    The function returns immediately after installing a daemon that (only)
    accepts GET requests received by the server on calls to [Async.yield()].

    Shall be scheduled at command line main stage {i via}
    [Boot.Main.extend] extension point.
*)
val start : 'a server -> unit

(** Stop the server if it is running in background.

    Can be invoked to force server shutdown at any time.

    It shall be typically scheduled {i via} [Extlib.safe_at_exit] along with
    other system cleanup operations to make sure the server is property
    shutdown before Frama-C main process exits.
*)
val stop : 'a server -> unit

(** Run the server forever.
    The server would now accept any kind of requests and start handling them.
    While executing an [`EXEC] request, the server would
    continue to handle (only) [`GET] pending requests on [Async.yield()]
    at every [server.polling] time interval.

    The function will {i not} return until the server is actually shutdown.

    Shall be scheduled at normal command line termination {i via}
    [Cmdline.at_normal_exit] extension point. *)
val run : 'a server -> unit

(** Kill the currently running request by raising an exception. *)
val kill : unit -> 'a

(** Emit the server signal to the client. *)
val emit : signal -> unit

(** Register a callback on signal listening.

    The callback is invoked with [true] on [SIGON] command
    and [false] on [SIGOFF].
*)
val on_signal : signal -> (bool -> unit) -> unit

(** Register a callback to listen for server activity.
    All callbacks are executed in their order of registration.
    Callbacks shall {i never} raise any exception. *)
val on : (bool -> unit) -> unit

(** Register a callback to listen for server initialization. All callbacks are
    executed once, in their order of registration, and before activity
    callbacks.
    Callbacks shall {i never} raise any exception. *)
val once : (unit -> unit) -> unit

(** Register an asynchronous task on the server.
    When the server is not working in background, this is
    equivalent to [Task.call] ; otherwise,
    the continuation is scheduled on the server
    like an [`EXEC] request. *)
val async : ('a -> 'b) -> 'a -> 'b Task.task

(** Returns true if the server is currently running. *)
val is_active : unit -> bool

(* -------------------------------------------------------------------------- *)
