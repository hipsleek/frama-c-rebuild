(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

include Plugin.S

val not_aorai_wkey: warn_category

module Ya: Parameter_sig.Filepath
module Dot: Parameter_sig.Bool
module DotSeparatedLabels: Parameter_sig.Bool
module AbstractInterpretation: Parameter_sig.Bool
module Axiomatization: Parameter_sig.Bool
module GenerateAnnotations: Parameter_sig.Bool
module GenerateDeterministicLemmas: Parameter_sig.Bool
module ConsiderAcceptance: Parameter_sig.Bool
module AutomataSimplification: Parameter_sig.Bool
module AddingOperationNameAndStatusInSpecification: Parameter_sig.Bool

(** if [true], adds assertion at the end of the generated function
    to check that the automaton is not in the rejecting state (in
    the deterministic case), or that at least one non-rejecting state
    is active (in the non-deterministic state).
*)
module SmokeTests: Parameter_sig.Bool

(** [true] if the user declares that its ya automaton is deterministic. *)
module Deterministic: State_builder.Ref with type data = bool

module InstrumentationHistory: Parameter_sig.Int

val is_on : unit -> bool
val advance_abstract_interpretation: unit -> bool

val emitter: Emitter.t
(** The emitter which emits Aorai annotations.
    @since Oxygen-20120901 *)
