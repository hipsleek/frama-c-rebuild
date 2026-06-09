(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Built-in Havoc Tactical (auto-registered) *)

open Tactical
open Strategy

module Havoc :
sig
  val tactical : tactical
  val strategy :
    ?priority:float -> selection -> strategy
end

module Separated :
sig
  val tactical : tactical
  val strategy : ?priority:float -> selection -> strategy
end

module Validity :
sig
  val tactical : tactical
  val strategy : ?priority:float -> selection -> strategy
end
