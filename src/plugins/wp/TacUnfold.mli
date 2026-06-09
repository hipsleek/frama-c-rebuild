(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Built-in Unfold Tactical (auto-registered) *)

open Tactical
open Strategy

(** @raises Not_found *)
val unfold : Lang.lfun -> Lang.F.term list -> Lang.F.term

val tactical : tactical
val strategy : ?priority:float -> selection -> strategy

(**************************************************************************)
