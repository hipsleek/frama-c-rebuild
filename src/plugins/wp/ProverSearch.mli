(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proof Search Engine                                                --- *)
(* -------------------------------------------------------------------------- *)

open ProofEngine
open Conditions

val first : tree -> ?anchor:node -> Strategy.t array -> fork option

val index : tree -> anchor:node -> index:int -> fork option

val search : tree -> ?anchor:node -> ?sequent:sequent ->
  Strategy.heuristic list -> fork option

val backtrack : tree -> ?anchor:node -> ?loop:bool -> ?width:int ->
  unit -> fork option

(* -------------------------------------------------------------------------- *)
