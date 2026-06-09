(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

val reset : unit -> unit
val hypothesis : 'a Log.pretty_printer
val is_interactive : unit -> bool

(** {2 Function Selection} *)

type functions =
  | Fct_none
  | Fct_all
  | Fct_skip of Cil_datatype.Kf.Set.t
  | Fct_list of Cil_datatype.Kf.Set.t

val get_fct : unit -> functions
val iter_fct : (Kernel_function.t -> unit) -> functions -> unit
val iter_kf : (Kernel_function.t -> unit) -> unit

(** {2 Goal Selection} *)

module WP          : Parameter_sig.Bool
module Dump        : Parameter_sig.Bool
module Behaviors   : Parameter_sig.String_list
module Properties  : Parameter_sig.String_list
module StatusAll   : Parameter_sig.Bool
module StatusTrue  : Parameter_sig.Bool
module StatusFalse : Parameter_sig.Bool
module StatusMaybe : Parameter_sig.Bool

(** {2 Model Selection} *)

val has_dkey : category -> bool

module Model : Parameter_sig.String_list
module ByValue : Parameter_sig.String_set
module ByRef : Parameter_sig.String_set
module InHeap : Parameter_sig.String_set
module AliasInit: Parameter_sig.Bool
module InCtxt : Parameter_sig.String_set
module ExternArrays: Parameter_sig.Bool
module Literals : Parameter_sig.Bool
module Volatile : Parameter_sig.Bool
module WeakIntModel : Parameter_sig.Bool

(** {2 Computation Strategies} *)

module Init: Parameter_sig.Bool
module InitWithForall: Parameter_sig.Bool
module BoundForallUnfolding: Parameter_sig.Int
module RTE: Parameter_sig.Bool
module Subst: Parameter_sig.Bool
module Qed: Parameter_sig.Bool
module Core: Parameter_sig.Bool
module Prune: Parameter_sig.Bool
module FilterInit: Parameter_sig.Bool
module Clean: Parameter_sig.Bool
module Filter: Parameter_sig.Bool
module Parasite: Parameter_sig.Bool
module Prenex: Parameter_sig.Bool
module Ground: Parameter_sig.Bool
module Reduce: Parameter_sig.Bool
module ExtEqual : Parameter_sig.Bool
module UnfoldAssigns : Parameter_sig.Int
module Havoc: Parameter_sig.Bool
module SplitBranch: Parameter_sig.Bool
module SplitSwitch: Parameter_sig.Bool
module SplitMax: Parameter_sig.Int
module SplitConj: Parameter_sig.Bool
module SplitCNF: Parameter_sig.Int
module DynCall : Parameter_sig.Bool
module SimplifyIsCint : Parameter_sig.Bool
module SimplifyLandMask : Parameter_sig.Bool
module SimplifyForall : Parameter_sig.Bool
module SimplifyType : Parameter_sig.Bool
module CalleePreCond : Parameter_sig.Bool
module PrecondWeakening : Parameter_sig.Bool
module TerminatesVariantHyp : Parameter_sig.Bool

(** {2 Prover Interface} *)

module ListProvers: Parameter_sig.Bool
module Tactics: Parameter_sig.String_list
module Generate:Parameter_sig.Bool
module ScriptOnStdout: Parameter_sig.Bool
module PrepareScripts: Parameter_sig.Bool
module FinalizeScripts: Parameter_sig.Bool
module DryFinalizeScripts: Parameter_sig.Bool
module Provers: Parameter_sig.String_list
module Interactive: Parameter_sig.String
module StrategyEngine: Parameter_sig.Bool
module ScriptMode: Parameter_sig.String
module DefaultStrategies: Parameter_sig.String_list
module RunAllProvers: Parameter_sig.Bool
module Cache: Parameter_sig.String
module CacheDir: Parameter_sig.User_dir_opt
module CachePrint: Parameter_sig.Bool
module Library: Parameter_sig.Filepath_list
module Drivers: Parameter_sig.Filepath_list
module Timeout: Parameter_sig.Int
module Memlimit: Parameter_sig.Int
module FctTimeout:
  Parameter_sig.Map
  with type key = Cil_types.kernel_function
   and type value = int
module SmokeTimeout: Parameter_sig.Int
module InteractiveTimeout: Parameter_sig.Int
module TimeExtra: Parameter_sig.Int
module TimeMargin: Parameter_sig.String
module Steps: Parameter_sig.Int
module Procs: Parameter_sig.Int
module ProofTrace: Parameter_sig.Bool

module Auto: Parameter_sig.String_list
module AutoDepth: Parameter_sig.Int
module AutoWidth: Parameter_sig.Int
module BackTrack: Parameter_sig.Int

(** {2 Why3 configuration} *)

module Why3Config: Parameter_sig.Filepath
module Why3Autodetect: Parameter_sig.Bool
module Why3ExtraConfig: Parameter_sig.String_list
module Why3Flags: Parameter_sig.String_list

(** {2 Proof Obligations} *)

module TruncPropIdFileName: Parameter_sig.Int
module Print: Parameter_sig.Bool
module Status: Parameter_sig.Bool
module Report: Parameter_sig.Filepath_list
module ReportJson: Parameter_sig.Filepath
module OldReportJson: Parameter_sig.String
module ReportName: Parameter_sig.String
module MemoryContext: Parameter_sig.Bool
module CheckMemoryContext: Parameter_sig.Bool
module SmokeTests: Parameter_sig.Bool
module SmokeDeadassumes: Parameter_sig.Bool
module SmokeDeadcode: Parameter_sig.Bool
module SmokeDeadcall: Parameter_sig.Bool
module SmokeDeadlocalinit: Parameter_sig.Bool
module SmokeDeadloop: Parameter_sig.Bool
module Probes: Parameter_sig.Bool
module CounterExamples: Parameter_sig.Bool

module Output : sig
  val exists: unit -> bool
  val get : unit -> Filepath.t
  val get_dir : string -> Filepath.t
  val mkdir : Filepath.t -> unit
  val add_update_hook : (Filepath.t -> Filepath.t -> unit) -> unit
end

(** {2 Debugging Categories} *)

val has_print_generated: unit -> bool
val print_generated: ?header:string -> Filepath.t -> unit
(** print the given file if the debugging category
    "print-generated" is set *)

val cat_print_generated: category

val protect : exn -> bool
