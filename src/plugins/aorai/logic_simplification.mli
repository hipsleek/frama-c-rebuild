(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

(** Basic simplification over {!Automaton_ast.typed_condition} *)

open Automaton_ast

(** {2 smart constructors for typed conditions} *)
val tand: typed_condition -> typed_condition -> typed_condition
val tor: typed_condition -> typed_condition -> typed_condition
val tnot: typed_condition -> typed_condition


(** {2 simplifications} *)

(** Given a condition, this function does some logical simplifications
    and returns an equivalent DNF form together with the simplified version *)
val simplifyCond:
  Automaton_ast.typed_condition ->
  Automaton_ast.typed_condition *(Automaton_ast.typed_condition list list)

(** Given a transition list, this function returns the same transition list with
    simplifyCond done on each cross condition. Uncrossable transition are
    removed. *)
val simplifyTrans:
  Automaton_ast.typed_trans list ->
  (Automaton_ast.typed_trans list)*
  (Automaton_ast.typed_condition list list list)

val dnfToCond :
  (Automaton_ast.typed_condition list list) -> Automaton_ast.typed_condition

val simplifyDNFwrtCtx :
  Automaton_ast.typed_condition list list -> Cil_types.kernel_function ->
  Automaton_ast.funcStatus -> Automaton_ast.typed_condition
