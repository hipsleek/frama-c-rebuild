(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Useful high-level system operations. *)

(* ************************************************************************* *)
(** {2 Pretty from files} *)
(* ************************************************************************* *)

val pp_from_file : Format.formatter -> Filepath.t -> unit
(** [pp_from_file fmt file] dumps the content of [file] into the [fmt].
    Exceptions in [pp] are re-raised after closing. *)

(* ************************************************************************* *)
(** {2 Timing Utility} *)
(* ************************************************************************* *)

type timer = float ref

val time : ?rmax:timer -> ?radd:timer -> ('a -> 'b) -> 'a -> 'b
(** Compute the elapsed time with [Sys.time].
    The [rmax] timer is maximized and the [radd] timer is cumulated.
    Computed result is returned, or exception is re-raised. *)

(* ************************************************************************* *)
(** {2 System commands} *)
(* ************************************************************************* *)

type process_result =
  | Not_ready of (unit -> unit)
  | Result of Unix.process_status
  (** [Not_ready f] means that the child process is not yet finished and
      may be terminated manually with [f ()]. *)

val async :
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  string -> string list
  -> (unit -> process_result)
(** Same arguments as {!Unix.create_process}.
    @return a function to call to check if the process execution
    is complete.
    You must call this function until it returns a Result
    to prevent Zombie processes.
    When this function returns a Result, the stdout and stderr of the child
    process will be filled into the arguments buffer.
    @raise Sys_error when a system error occurs
    @before 31.0-Gallium this function was named [command_async] *)

val spawn :
  async:bool ->
  ?timeout:int ->
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  string -> string list
  -> Unix.process_status
(** Same arguments as {!Unix.create_process}.
    When this function returns, the stdout and stderr of the child
    process will be filled into the arguments buffer.
    @raise Sys_error when a system error occurs
    @raise Async.Cancel when the computation is interrupted or on timeout
    @before 29.0-Copper Async.Cancel was Db.Cancel
    @before 31.0-Gallium this function was named [command]
    @before 33.0-Arsenic [async] was not present and set to true when Frama-C was
    in GUI mode.
*)

(* ************************************************************************* *)
(** {2 Specialized command} *)
(* ************************************************************************* *)

(** Specialized command for the Graphviz's dot program to process dot files.
    @since 32.0-Germanium *)
module Dot :
sig
  type format = Jpeg | Pdf | Png | Svg

  (** [format_to_string format] convert [format] to a string corresponding to
      the variant name in lowercase. It can be used to derive a file
      extension. *)
  val format_to_string : format -> string

  (** [Dot.spawn ~format ~output input] create a process to run dot on [input]
      dot file to generate [output] file with output format [format].
      @raise Sys_error when a system error occurs
      @raise Async.Cancel when the computation is interrupted or on timeout
      @before 33.0-Arsenic [async] was not present and set to true when Frama-C
      was in GUI mode. *)
  val spawn : async:bool -> ?timeout:int -> ?layout:string -> format:format ->
    output:Filepath.t -> Filepath.t -> Unix.process_status
end
