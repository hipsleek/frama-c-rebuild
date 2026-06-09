(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

exception Error of Cil_types.location * string
exception Unbound of string

(** For the three functions below, [env] can be used to specify which
    logic labels are parsed. By default, only [Here] is accepted. All
    the C labels inside the function are also  accepted, regardless of
    [env]. [loc] is used as the source for the beginning of the string.
    All three functions may raise {!Logic_interp.Error} or
    {!Parsing.Parse_error}. *)

val code_annot : kernel_function -> stmt -> string -> code_annotation
val term_lval :
  kernel_function -> ?loc:location -> ?env:Logic_typing.Lenv.t -> string ->
  Cil_types.term_lval
val term :
  kernel_function -> ?loc:location -> ?env:Logic_typing.Lenv.t -> string ->
  Cil_types.term
val predicate :
  kernel_function -> ?loc:location -> ?env:Logic_typing.Lenv.t -> string ->
  Cil_types.predicate
