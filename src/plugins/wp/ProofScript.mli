(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Tactical
open Conditions

class console : pool:Lang.F.pool -> title:string -> Tactical.feedback

type jscript = alternative list
and alternative = private
  | Prover of Prover.t * VCS.result
  | Tactic of int * jtactic * (string * jscript) list (** With number of pending goals *)
  | Error of string * Json.t
and jtactic = {
  header : string ;
  tactic : string ;
  params : Json.t ;
  select : Json.t ;
  strategy : string option ;
}

val is_prover : alternative -> bool
val is_tactic : alternative -> bool
val a_prover : Prover.t -> VCS.result -> alternative
val a_tactic : jtactic -> (string * jscript) list -> alternative

val pending : alternative -> int
(** pending goals *)

val pending_any : jscript -> int
(** minimum of pending goals *)

val has_proof : jscript -> bool
(** Has a tactical alternative *)

val decode : Json.t -> jscript
val encode : jscript -> Json.t

val jtactic : ?strategy:string -> tactical -> selection -> jtactic
val configure : jtactic -> sequent -> (tactical * selection) option

(** Json Codecs *)

val json_of_selection : selection -> Json.t
val selection_of_json : sequent -> Json.t -> selection
val selection_target : Json.t -> string

val json_of_param : tactical -> parameter -> string * Json.t
val param_of_json : tactical -> sequent -> Json.t -> parameter -> unit

val json_of_parameters : tactical -> Json.t
val parameters_of_json : tactical -> sequent -> Json.t -> unit

val json_of_tactic : jtactic -> (string * Json.t) list -> Json.t
val json_of_result : Prover.t -> VCS.result -> Json.t

val prover_of_json : Json.t -> Prover.t option
val result_of_json : Json.t -> VCS.result
