(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Built-in Cut Tactical (auto-registered) *)

open Tactical
open Strategy

type mode = CASES | MODUS
val fmode : mode field
val tactical : tactical
val strategy : ?priority:float -> ?modus:bool -> selection -> strategy

(**************************************************************************)
