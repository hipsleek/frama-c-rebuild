(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Verification Condition Status *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(** {2 Config}
    [None] means current WP option default.
    [Some 0] means prover default. *)
(* -------------------------------------------------------------------------- *)

type config = {
  valid : bool ;
  timeout : float option ;
  stepout : int option ;
  memlimit : int option ;
}

val current : unit -> config (** Current parameters *)

val default : config (** all None *)

val get_timeout : ?kf:Kernel_function.t -> smoke:bool -> config -> float
(** 0.0 means no-timeout *)

val get_stepout : config -> int
(** 0 means no-stepout *)

val get_memlimit : config -> int
(** 0 means no-memlimit *)

(** {2 Results} *)

type verdict =
  | NoResult
  | Unknown
  | Timeout
  | Stepout
  | Computing of (unit -> unit) (* kill function *)
  | Valid
  | Invalid (* model *)
  | Failed

type model = Why3Provers.model Probe.Map.t

type result = {
  verdict : verdict ;
  cached : bool ;
  solver_time : float ;
  prover_time : float ;
  prover_steps : int ;
  prover_errpos : Lexing.position option ;
  prover_errmsg : string ;
  prover_model : model ;
}

val no_result : result
val valid : result
val unknown : result
val stepout : int -> result
val timeout : float -> result
val computing : (unit -> unit) -> result
val failed : ?pos:Lexing.position -> string -> result
val kfailed : ?pos:Lexing.position -> ('a,Format.formatter,unit,result) format4 -> 'a
val cached : result -> result (** only for true verdicts *)

val result : ?model:model -> ?cached:bool ->
  ?solver:float -> ?time:float -> ?steps:int -> verdict -> result

val is_result : verdict -> bool
val is_proved: smoke:bool -> verdict -> bool

val is_none : result -> bool
val is_verdict : result -> bool
val is_valid: result -> bool
val is_trivial: result -> bool
val is_not_valid: result -> bool
val is_computing: result -> bool
val is_cacheable: result -> bool
val has_model: result -> bool

val configure : result -> config
val autofit : result -> bool (** Result that fits the default configuration *)

val name_of_verdict : ?computing:bool -> verdict -> string

val pp_result : Format.formatter -> result -> unit
val pp_model : Format.formatter -> model -> unit
val pp_result_qualif : ?updating:bool -> Prover.t -> result ->
  Format.formatter -> unit

val conjunction : verdict -> verdict -> verdict (* for tactic children *)
val compare : result -> result -> int (* minimal is best *)
val best : (Prover.t * result) list -> Prover.t * result
