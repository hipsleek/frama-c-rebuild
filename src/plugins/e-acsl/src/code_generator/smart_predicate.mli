(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val prel :
  ?smart:bool ->
  ?loc:location ->
  ?names:string list ->
  relation ->
  term ->
  term ->
  predicate
(** create a relation predicate and try to optimize it if [smart] is [true]. *)
