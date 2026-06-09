(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Built-in Range Tactical (auto-registered) *)

open Tactical
open Strategy

val vmin : int field
val vmax : int field
val tactical : tactical
val strategy :
  ?priority:float -> selection -> vmin:int -> vmax:int -> strategy

(**************************************************************************)
