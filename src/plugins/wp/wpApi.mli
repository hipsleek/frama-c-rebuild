(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module WP_Prover = Prover

(* -------------------------------------------------------------------------- *)
(** Server API for WP *)
(* -------------------------------------------------------------------------- *)

val package : Server.Package.package

module Prover : Server.Data.S with type t = Prover.t
module Provers : Server.Data.S with type t = Prover.t list
module Result : Server.Data.S with type t = VCS.result
module Goal : Server.Data.S with type t = Wpo.t
module InteractiveMode : Server.Data.S with type t = WP_Prover.InteractiveMode.t

val goals : Wpo.t Server.States.array
val getProvers : unit -> Prover.t list

(* -------------------------------------------------------------------------- *)
