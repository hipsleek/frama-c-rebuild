(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Eva public API. *)


(** {2 Main API} *)

(** Run the analysis and check analysis status. *)
module Analysis = Analysis

(** Main API to access the results of an analysis, such as the possible values
    of expressions and memory locations of lvalues at each program point. *)
module Results = Results


(** {2 Statistics} *)

(** Statistics about the analysis performance. *)
module Eva_perf = (Eva_perf : Eva_perf_sig.API)

(** Various statistics collected during an analysis. *)
module Statistics = Statistics


(** {2 Types used in Eva} *)

(** AST used by Eva. *)
module Eva_ast = Eva_ast

(** Position in the analysis *)
module Position = Position

(** Types of callstack. *)
module Callstack = Callstack

(** Types for the memory dependencies of an expression or lvalue. *)
module Deps = Deps


(** {2 Configuration of the analysis} *)

(** Command-line parameters of the analysis. *)
module Parameters = Parameters

(** Add local annotations to guide the analysis. *)
module Eva_annotations = Eva_annotations

(** Register OCaml builtins to be used by the cvalue domain instead of analysing
    the body of some C functions. *)
module Builtins = (Builtins : Builtins_sig.API)


(** {2 Internal API}

    These modules are for internal use only.
    They may be modified or removed in future versions.
    Please contact us if you need features from these modules.  *)

(** Generation of annotations. *)
module Export = Export

(** Types and operations for the From plugin. *)
module Assigns = Assigns

(** Register hooks called during the analysis with states of the cvalue domain. *)
module Cvalue_callbacks = (Cvalue_callbacks : Cvalue_callbacks_sig.API)

(** Interpretation of predicates and assigns clauses for Inout and From plugins. *)
module Logic_inout = Logic_inout

(** Undocumented. *)
module Eva_results = Eva_results

(** Unit testing. *)
module Unit_tests = Unit_tests
