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

type dir = [ `Left | `Right ]
val tactical : dir -> tactical
val strategy : ?priority:float -> dir -> selection -> strategy

(**************************************************************************)
