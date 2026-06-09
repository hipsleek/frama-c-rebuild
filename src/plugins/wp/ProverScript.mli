(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open VCS

(** - [valid]: Play provers with valid result (default: true)
    - [failed]: Play provers with invalid result (default: true)
    - [scratch]: Discard existing script (default: false)
    - [provers]: Additional list of provers to {i try} when stuck
    - [depth]: Strategy search depth (default: 0)
    - [width]: Strategy search width (default: 0)
    - [backtrack]: Strategy backtracking (default: 0)
    - [auto]: Strategies to try (default: none)
*)
type 'a process =
  ?valid:bool ->
  ?failed:bool ->
  ?scratch:bool ->
  ?provers:Prover.t list ->
  ?depth:int ->
  ?width:int ->
  ?backtrack:int ->
  ?auto:Strategy.heuristic list ->
  ?strategies:bool ->
  ?start:(Wpo.t -> unit) ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> Prover.t -> result -> unit) ->
  ?success:(Wpo.t -> Prover.t option -> unit) ->
  Wpo.t -> 'a

val prove : unit Task.task process
val spawn : unit process

val search :
  ?depth:int ->
  ?width:int ->
  ?backtrack:int ->
  ?auto:Strategy.heuristic list ->
  ?provers:Prover.t list ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> Prover.t -> result -> unit) ->
  ?success:(Wpo.t -> Prover.t option -> unit) ->
  ProofEngine.tree ->
  ProofEngine.node ->
  unit

val explore :
  ?depth:int ->
  ?strategy:ProofStrategy.strategy ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> Prover.t -> result -> unit) ->
  ?success:(Wpo.t -> Prover.t option -> unit) ->
  ProofEngine.tree ->
  ProofEngine.node ->
  unit

val get : Wpo.t -> [ `Script | `Proof | `Saved | `None ]
