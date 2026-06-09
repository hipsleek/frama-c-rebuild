(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Built-in Filtering Tactic (auto-registered) *)

val vanti : bool Tactical.field
val tactical : Tactical.t
val strategy : ?priority:float -> ?anti:bool -> unit -> Strategy.t
