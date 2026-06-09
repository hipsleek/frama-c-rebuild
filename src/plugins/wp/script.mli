(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Lexer for Script files                                             --- *)
(* -------------------------------------------------------------------------- *)

type token =
  | Id of string
  | Key of string
  | Proof of string * string
  | Word
  | Eof

type input

val open_file : string -> input
val close : input -> unit
val skip : input -> unit
val token : input -> token
val error : input -> ('a,Format.formatter,unit,'b) format4 -> 'a

val key : input -> string -> bool
val eat : input -> string -> unit
val ident : input -> string
val idents : input -> string list

val filter : string -> string option
