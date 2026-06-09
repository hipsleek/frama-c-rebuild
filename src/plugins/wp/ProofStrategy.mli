(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open ProofEngine
open Pattern

(* -------------------------------------------------------------------------- *)
(* --- Proof Strategy Engine                                              --- *)
(* -------------------------------------------------------------------------- *)

type 'a loc = { loc : Cil_types.location ; value : 'a }

(* Abstract Syntax Tree: must be stdlib-marshallable *)
type strategy = {
  name: string loc ;
  alternatives: alternative loc list ;
}

and tactic = {
  tactic : string loc ;
  lookup : lookup list ;
  select : value list ;
  params : (string loc * value) list ;
  children : (string loc * string loc) list ; (* name prefix and strategy *)
  default: string loc option; (* None is default *)
}

and alternative =
  | Default
  | Strategy of string loc
  | Provers of string loc list * float option (* timeout *)
  | Auto of string loc (* deprecated -wp-auto *)
  | Tactic of tactic

type context

val context: ?tc:Logic_typing.typing_context -> unit -> context
val debug_table: context -> (string, pattern) Hashtbl.t

val parse_alternatives: context -> Logic_ptree.lexpr list -> alternative loc list

val typecheck : unit -> unit

val typecheck_strategy : env -> string loc -> unit
val typecheck_prover : env -> string loc -> unit
val typecheck_auto : env -> string loc -> unit
val typecheck_tactic : env -> tactic -> unit

val name : strategy -> string
val loc : strategy -> Cil_types.location
val find : string -> strategy option
val hints : ?node:ProofEngine.node -> Wpo.t -> strategy list
val has_hint : Wpo.t -> bool


val tactical: string loc -> Tactical.tactical
val select:
  sigma -> ?goal:Lang.F.pred -> value list -> Tactical.selection
val configure:
  env -> Tactical.tactical -> sigma -> string loc * value -> unit


val iter : (strategy -> unit) -> unit
val default : unit -> strategy list
val alternatives : strategy -> alternative loc list
val provers : ?default:Prover.t list -> alternative loc -> Prover.t list * float
val auto : alternative loc -> Strategy.heuristic option
val fallback : alternative loc -> strategy option
val tactic : tree -> node -> strategy -> alternative loc -> node list option

val pp_strategy : Format.formatter -> strategy -> unit
val pp_alternative : Format.formatter -> alternative loc -> unit

(* -------------------------------------------------------------------------- *)
