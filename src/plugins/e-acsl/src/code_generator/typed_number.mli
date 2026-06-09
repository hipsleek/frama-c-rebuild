(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Manipulate the type of numbers. *)

open Cil_types

(** [add_cast ~loc ?name env kf ctx sty t_opt e] convert number expression [e]
    in a way that it is compatible with the given typing context [ctx].
    [sty] indicates if the expression is a string representing a number (integer
    or real) or directly a C number type.
    [t_opt] is the term that is represented by the expression [e]. *)
val add_cast:
  loc:location ->
  ?name:string ->
  Env.t ->
  kernel_function ->
  typ option ->
  Analyses_types.strnum ->
  term option ->
  exp ->
  exp * Env.t
