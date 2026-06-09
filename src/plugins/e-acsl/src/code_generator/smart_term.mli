(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val tsizeof : ?smart:bool -> ?loc:location -> typ -> term
(** make a [sizeof(ty)] term and optimize it if [smart] is [true]. *)

val talignof : ?smart:bool -> ?loc:location -> typ -> term
(** make a [alignof(ty)] term and optimize it if [smart] is [true]. *)

val copy : ?smart:bool -> term -> term
(** copy a term using the [Misc.Id_term.deep_copy] function and optimize it
    if [smart] is [true]. *)
