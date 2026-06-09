(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val cmdline_run: unit -> unit

val do_wp_proofs:
  ?provers:Why3.Whyconf.prover list ->
  ?interactive_mode:Prover.InteractiveMode.t ->
  ?scripts:bool ->
  ?strategies:bool ->
  Wpo.t Bag.t -> unit
