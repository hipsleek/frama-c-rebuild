(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** {1 Performance Reporting} *)
(* -------------------------------------------------------------------------- *)

(** Prover Stats *)
type pstats = {
  tmin : float ; (** minimum prover time (non-smoke proof only) *)
  tval : float ; (** cumulated prover time (non-smoke proof only) *)
  tmax : float ; (** maximum prover time (non-smoke proof only) *)
  tnbr : float ; (** number of non-smoke proofs *)
  time : float ; (** cumulated prover time (smoke and non-smoke) *)
  success : float ; (** number of success (valid xor smoke) *)
}

(** Cumulated Stats

    Remark: for each sub-goal, only the _best_ prover result is kept *)
type stats = {
  best : VCS.verdict ; (** provers best verdict (not verdict of the goal) *)
  provers : (Prover.t * pstats) list ; (** meaningful provers *)
  tactics : int ; (** number of tactics *)
  proved : int ; (** number of proved sub-goals *)
  timeout : int ; (** number of timeouts and stepouts sub-goals *)
  unknown : int ; (** number of unknown sub-goals *)
  noresult : int ; (** number of no-result sub-goals *)
  failed : int ; (** number of failed sub-goals *)
  cached : int ; (** number of cached prover results *)
  cacheable : int ; (** number of prover results that can be cached *)
}

val pp_pstats : Format.formatter -> pstats -> unit
val pp_stats :
  shell:bool ->
  cache:Cache.mode ->
  Format.formatter -> stats -> unit

val pretty : Format.formatter -> stats -> unit

val empty : stats
val add : stats -> stats -> stats

val results : smoke:bool -> (Prover.t * VCS.result) list -> stats
val tactical : qed:float -> stats list -> stats
val script : stats -> VCS.result

val subgoals : stats -> int
(* sum of proved + timeout + unit + noresult + failed *)

val complete : stats -> bool

(* -------------------------------------------------------------------------- *)
