(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type input
(** The type of input channel. *)

type output
(** The type of output channel. *)

val open_in_bin : string -> input
(** Open the given file for reading in binary mode, and return a new input
    channel on that file, positioned at the beginning of the file. *)

val close_in : input -> unit
(** Close the given channel. *)

val input_value : input -> 'a
(** Read the representation of a structured value, as produced by
    [output_value], and return the corresponding value. *)

val input_char : input -> char
(** Read one character from the given input channel.
    @raise End_of_file if there are no more characters to read. *)

val unsafe_really_input : input -> bytes -> int -> int -> unit
(** [unsafe_really_input ic buf pos len] reads [len] characters from channel
    [ic], storing them in byte sequence [buf], starting at character number
    [pos]. The function is unsafe as no verification is done that [0 <= pos],
    [0 <= len] or [Bytes.length buf > pos + len]. *)

val open_out_bin : ?compress:bool -> string -> output
(** Open the given file for writing in binary mode, and return a new output
    channel on that file, positioned at the beginning of the file. If
    [compress] is true then the content of the file will be compressed by
    [Compression]. *)

val close_out : output -> unit
(** Close the given channel. *)

val output_value : output -> 'a -> unit
(** Write the representation of a structured value of any type to a channel. The
    object can be read back by [input_value]. *)
