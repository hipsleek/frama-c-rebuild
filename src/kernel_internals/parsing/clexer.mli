(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(** The C Lexer. *)

val init:
  filename:string -> (Lexing.lexbuf -> 'a) ->
  Lexing.lexbuf * (Lexing.lexbuf -> 'a)

val finish: unit -> unit

val initial: Lexing.lexbuf -> Cparser.token
(** This is the main lexing function *)

val push_context: unit -> unit
(** Start a context  *)

val add_type: string -> unit
(** Add a new string as a type name  *)

val add_identifier: string -> unit
(** Add a new string as a variable name  *)

val pop_context: unit -> unit
(** Remove all names added in this context  *)

val annot_char : char ref
(** The character to recognize logic formulae in comments *)

val currentLoc : unit -> Cabs.cabsloc

val is_c_keyword: string -> bool
(** [true] if the given string is a C keyword.
    @since Nitrogen-20111001 *)
