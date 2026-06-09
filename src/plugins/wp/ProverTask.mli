(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Prover Implementation against Task API                             --- *)
(* -------------------------------------------------------------------------- *)

(** If provided, the number of procs is forwarded to the Why3 and the server *)
val server : ?procs:int -> unit -> Task.server

val simplify :
  ?start:(Wpo.t -> unit) ->
  ?result:(Wpo.t -> Prover.t -> VCS.result -> unit) ->
  ?commit:bool ->
  Wpo.t -> bool Task.task

val prove : Wpo.t ->
  ?config:VCS.config ->
  ?mode:Prover.InteractiveMode.t ->
  ?start:(Wpo.t -> unit) ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> Prover.t -> VCS.result -> unit) ->
  Prover.t -> bool Task.task

val spawn : Wpo.t ->
  delayed:bool ->
  ?config:VCS.config ->
  ?start:(Wpo.t -> unit) ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> Prover.t -> VCS.result -> unit) ->
  ?success:(Wpo.t -> Prover.t option -> unit) ->
  (Prover.InteractiveMode.t * Prover.t) list -> unit
