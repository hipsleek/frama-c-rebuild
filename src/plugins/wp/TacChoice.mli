(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Built-in Choice, Absurd & Contrapose Tactical (auto-registered) *)

open Tactical
open Strategy

module Choice :
sig
  val tactical : tactical
  val strategy : ?priority:float -> selection -> strategy
end

module Absurd :
sig
  val tactical : tactical
  val strategy : ?priority:float -> selection -> strategy
end

module Contrapose :
sig
  val tactical : tactical
  val strategy : ?priority:float -> selection -> strategy
end
