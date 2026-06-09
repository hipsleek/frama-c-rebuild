(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- WP Computer (main entry points)                                    --- *)
(* -------------------------------------------------------------------------- *)

(** Compute model setup from command line options. *)
val user_setup : unit -> Factory.setup

val create :
  ?dump:bool ->
  ?setup:Factory.setup ->
  ?driver:Factory.driver ->
  unit -> Wpo.generator

(* -------------------------------------------------------------------------- *)
