(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Handling errors. *)

module type S = sig
  type 'a result = ('a, exn) Result.t
  (** Represent either a result of type ['a] or an error with an exception. *)

  exception Typing_error of Fileloc.t * Options.category option * string
  (** Typing error where the first element is the phase where the error occurred
      and the second element is the error message. *)

  exception Not_yet of Fileloc.t * Options.category option * string
  (** "Not yet supported" error where the first element is the phase where the
      error occurred and the second element is the error message. *)

  exception Not_memoized of Fileloc.t * Options.category option
  (** "Not memoized" error with the phase where the error occurred. *)

  val make_untypable: ?loc:Fileloc.t -> string -> exn
  (** Make a [Typing_error] exception with the given message. *)

  val make_not_yet: ?loc:Fileloc.t -> string -> exn
  (** Make a [Not_yet] exception with the given message. *)

  val make_not_memoized: ?loc:Fileloc.t -> unit -> exn
  (** Make a [Not_memoized] exception with the given message. *)

  val untypable: ?loc:Fileloc.t -> string -> 'a
  (** @raise Typing_error with the given message for the current phase. *)

  val not_yet: ?loc:Fileloc.t -> string -> 'a
  (** @raise Not_yet with the given message for the current phase. *)

  val not_memoized: ?loc:Fileloc.t -> unit -> 'a
  (** @raise Not_memoized for the current phase. *)

  val print_not_yet: string -> unit
  (** Print the "not yet supported" message without raising an exception. *)

  val handle: ('a -> 'a) -> 'a -> 'a
  (** Run the closure with the given argument and handle potential errors.
      Return the provide argument in case of errors. *)

  val generic_handle: ('a -> 'b) -> 'b -> 'a -> 'b
  (** Run the closure with the given argument and handle potential errors.
      Return the additional argument in case of errors. *)

  val retrieve_preprocessing:
    string ->
    ('a -> 'b result) ->
    'a ->
    (Format.formatter -> 'a -> unit) ->
    'b
  (** Retrieve the result of a preprocessing phase, which possibly failed.
      The [string] argument and the formatter are used to display a message in
      case the preprocessing phase did not compute the required result. *)

  val pp_result:
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a result ->
    unit
  (** [pp_result pp] where [pp] is a formatter for ['a] returns a formatter for
      ['a result]. *)

  val iter : ('a -> unit) -> 'a result -> unit
  val map : ('a -> 'b) -> 'a result -> 'b
  val map2 : ('a -> 'b -> 'c) -> 'a result -> 'b result -> 'c
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a result -> 'b result -> 'c result -> 'd
  (** Apply a function to one or several results and propagate the errors *)

end

(** Functor to build an [Error] module for a given [phase]. *)
module Make(_: sig val phase:Options.category end): S

(** The [Error] module implements [Error.S] with no phase. *)
include S
