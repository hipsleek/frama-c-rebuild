(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Built-in Tactical for Product & Division Comparison  (auto-registered) *)

open Tactical
open Strategy

val tactical : tactical
val strategy : ?priority:float -> selection -> strategy

(**************************************************************************)
