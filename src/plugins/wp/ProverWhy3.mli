(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Equality used in the goal, simpler to prove than polymorphic equality *)
val add_specific_equality:
  for_tau:(Lang.tau -> bool) ->
  mk_new_eq:Lang.F.binop ->
  unit

(** Return NoResult if it is already proved by Qed *)
val prove :
  ?mode:Prover.InteractiveMode.t ->
  ?timeout:float ->
  ?steplimit:int ->
  ?memlimit:int ->
  prover:Why3Provers.t -> Wpo.t -> VCS.result Task.task

(**************************************************************************)
