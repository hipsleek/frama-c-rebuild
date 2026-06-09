(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

(** [get_edges s1 s2 g] retrieves all edges in [g] between [s1] and [s2]. *)
val get_edges:
  Automaton_ast.state -> Automaton_ast.state -> ('c,'a) Automaton_ast.graph
  -> ('c, 'a) Automaton_ast.trans list

(** retrieve all edges starting at the given node. *)
val get_transitions_of_state:
  Automaton_ast.state -> ('c,'a) Automaton_ast.graph -> ('c,'a) Automaton_ast.trans list

(** return the initial states of the graph. *)
val get_init_states: ('c, 'a) Automaton_ast.graph -> Automaton_ast.state list

(** [true] iff there's at most one path between the two states in the graph. *)
val at_most_one_path:
  ('c, 'a) Automaton_ast.graph -> Automaton_ast.state -> Automaton_ast.state -> bool
