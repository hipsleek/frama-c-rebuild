(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

(** utilities for parsing automaton's formulas *)

(** aborts the execution using current lexbuf position as source. *)
val abort_current:
  Lexing.lexbuf -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** aborts in case the current character
    is not the beginning of a valid token. *)
val unknown_token: Lexing.lexbuf -> 'a

(** initiate a new line in the lexbuf *)
val newline: Lexing.lexbuf -> unit

(** aborts in case of an unterminated comment *)
val unterminated_comment: Lexing.lexbuf -> 'a
