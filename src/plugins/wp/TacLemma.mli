(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Self registered 'Lemma' Tactical *)

open Tactical
open Strategy

type lemma = Definitions.dlemma Tactical.named
val named : Definitions.dlemma -> lemma
val find : string -> lemma
val search : lemma option Tactical.field
val tactical : tactical
val strategy :
  ?priority:float -> ?at:selection -> string -> selection list -> strategy

(**************************************************************************)
