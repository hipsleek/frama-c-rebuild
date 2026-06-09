(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

(** Lexer for logic annotations *)

exception Error of (int * int) * string

val token : Lexing.lexbuf -> Logic_parser.token
(** For plugins that need to call functions of [Logic_parser] themselves *)

val chr : Lexing.lexbuf -> string
val is_acsl_keyword : string -> bool


type 'a parse = Filepos.t * string -> (Filepos.t * 'a) option
(** Generic type for parsing functions built on tip of the lexer. Given
    such a function [f], [f (pos, s)] parses [s], assuming that it starts at
    position [pos]. If parsing is successful, it returns the final position,
    and the result. If an error occurs with a warning status other than [Wabort]
    for [annot-error], returns [None]
*)

val lexpr : Logic_ptree.lexpr parse
val annot : Logic_ptree.annot parse
val spec : Logic_ptree.spec parse

val ext_spec : Lexing.lexbuf -> Logic_ptree.ext_spec
(** ACSL extension for parsing external spec file.
    Modifies tokens as follows:
    - C-comments [/* ... */] can be used and can be nested
    - ["module"] keyword is interpreted as [EXT_SPEC_MODULE]
*)
