(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Server API for the Interactive Prover *)
(* -------------------------------------------------------------------------- *)

module Node : Server.Data.S with type t = ProofEngine.node
module Tactic : Server.Data.S with type t = Tactical.t
module Part : Server.Data.S with type t = [ `Term | `Goal | `Step of int ]
module Term : Server.Data.S with type t = Lang.F.term

val proofStatus : Server.Request.signal
val printStatus : Server.Request.signal
val selection : ProofEngine.node -> Tactical.selection
val setSelection : ProofEngine.node -> Tactical.selection -> unit

val lookup_printer: ProofEngine.node -> Ptip.pseq

val runProvers :
  ?mode:Prover.InteractiveMode.t ->
  ?timeout:int ->
  ?provers:Prover.t list ->
  ProofEngine.node -> unit

val killProvers :
  ?provers:Prover.t list ->
  ProofEngine.node -> unit

val clearProvers :
  ?provers:Prover.t list ->
  ProofEngine.node -> unit

(* -------------------------------------------------------------------------- *)
