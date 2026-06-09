(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(** Helper functions for Cabs *)

val nextident : int ref

val getident : unit -> int
val cabslu : Cabs.cabsloc

(* List of comments together with the location where they are found. *)
module Comments: sig
  val self: State.t
  (* adds a comment at a given location. *)
  val add: Cabs.cabsloc -> string -> unit
  (*  gets all the comment located between the two positions. *)
  val get: Cabs.cabsloc -> string list
  (* iter over all registered comments. *)
  val iter: (Cabs.cabsloc -> string -> unit) -> unit
  (* fold over all registered comments. *)
  val fold: (Cabs.cabsloc -> string -> 'a -> 'a) -> 'a -> 'a
end

val missingFieldDecl :
  Cabs.cabsloc -> string * Cabs.decl_type * 'a list * Cabs.cabsloc
val isStatic : Cabs.spec_elem list -> bool
val isExtern : Cabs.spec_elem list -> bool
val isInline : Cabs.spec_elem list -> bool
val isTypedef : Cabs.spec_elem list -> bool
val get_definitionloc : Cabs.definition -> Cabs.cabsloc
val get_statementloc : Cabs.statement -> Cabs.cabsloc
val explodeStringToInts : string -> int64 list
val valueOfDigit : char -> int64
val d_cabsloc : Cabs.cabsloc Pretty_utils.formatter

(* hack to avoid shift/reduce conflict is attr parsing. *)
val push_attr_test: unit -> unit
val pop_attr_test: unit -> unit
val is_attr_test: unit -> bool

val mk_behavior :
  ?name:string ->
  ?assumes:Logic_ptree.lexpr list ->
  ?requires:Logic_ptree.toplevel_predicate list ->
  ?post_cond:
    (Cil_types.termination_kind * Logic_ptree.toplevel_predicate) list ->
  ?assigns:Logic_ptree.assigns ->
  ?allocation:Logic_ptree.allocation ->
  ?extended:Logic_ptree.extension list ->
  unit ->
  Logic_ptree.behavior

val mk_asm_templates : string list -> string list
