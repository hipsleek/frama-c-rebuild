(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

type state = Automaton_ast.state
type transition = Automaton_ast.typed_trans
type edge = state * transition * state

include Graph.Sig.I
  with type V.t = state
   and type V.label = state
   and type E.t = edge
   and type E.label = transition
   and type edge := edge

val of_automaton : Automaton_ast.typed_automaton -> t

val states : t -> state list
val init_states : t -> state list
val edges : t -> edge list
