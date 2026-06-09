(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

open Automaton_ast

type 'a printer = Format.formatter -> 'a -> unit

val print_state : state printer
val print_statel : state list printer

module Parsed:
sig
  val print_expression: expression printer
  val print_condition: condition printer
  val print_seq_elt: seq_elt printer
  val print_sequence: sequence printer
  val print_guard: guard printer
  val print_action: action printer
  val print_actionl: action list printer
end

module Typed:
sig
  val print_condition : typed_condition printer
  val print_action: typed_action printer
  val print_actionl: typed_action list printer
  val print_transition: typed_trans printer
  val print_transitionl: typed_trans list printer
  val print_automata : typed_automaton printer
  val output_dot_automata : typed_automaton -> string -> unit
end
