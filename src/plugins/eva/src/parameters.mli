(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Eva: Parameter_sig.Bool

(** Meta-option *)
module Precision: Parameter_sig.Int

(** {2 Domains} *)

module Domains: Parameter_sig.String_set
module DomainsFunction: Parameter_sig.Multiple_map
  with type key = string
   and type value = Domain_mode.function_mode

module EqualityCall: Parameter_sig.String
module EqualityCallFunction:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                     and type value = string

module OctagonCall: Parameter_sig.Bool

module MultidimSegmentLimit: Parameter_sig.Int
module MultidimDisjunctiveInvariants: Parameter_sig.Bool
module MultidimFastImprecise: Parameter_sig.Bool

module TaintAuto: Parameter_sig.Bool
module TaintSingletons: Parameter_sig.Bool
module SecureFlow: Parameter_sig.Bool

module NumerorsInteraction : Parameter_sig.String

module TracesUnrollLoop: Parameter_sig.Bool
module TracesUnifyLoop: Parameter_sig.Bool
module TracesDot: Parameter_sig.Filepath
module TracesProject: Parameter_sig.Bool

(** {2 Precision} *)

module WideningDelay: Parameter_sig.Int
module WideningPeriod: Parameter_sig.Int

module ArrayPrecisionLevel: Parameter_sig.Int

module SubdivideNonLinear: Parameter_sig.Int
module SubdivideNonLinearFunction:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                     and type value = int

module EnumerateCond: Parameter_sig.Bool
module OracleDepth: Parameter_sig.Int
module ReductionDepth: Parameter_sig.Int

(** {2 State partitioning} *)

module MinLoopUnroll : Parameter_sig.Int
module AutoLoopUnroll : Parameter_sig.Int
module DefaultLoopUnroll : Parameter_sig.Int

module HistoryPartitioning : Parameter_sig.Int
module HistoryPartitioningFunction :
  Parameter_sig.Map with type key = Cil_types.kernel_function
                     and type value = int

module ValuePartitioning : Parameter_sig.String_set
module SplitLimit : Parameter_sig.Int

module InterproceduralSplits : Parameter_sig.Bool
module InterproceduralHistory : Parameter_sig.Bool

module SLevel: Parameter_sig.Int
module SlevelFunction:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                     and type value = int
module SlevelMergeAfterLoop: Parameter_sig.Kernel_function_set

module SplitReturnFunction:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                     and type value = Split_strategy.t
module SplitReturn: Parameter_sig.Custom with type t = Split_strategy.t

(** {2 Performance} *)

module NoResultsDomain: Parameter_sig.String_set
module NoResultsFunction: Parameter_sig.Fundec_set
module ResultsAll: Parameter_sig.Bool
module JoinResults: Parameter_sig.Bool

module Memexec: Parameter_sig.Bool

(** {2 Initial context of the analysis} *)

module ContextDepth: Parameter_sig.Int
module ContextWidth: Parameter_sig.Int
module ContextValidPointers: Parameter_sig.Bool

module InitializationPaddingGlobals: Parameter_sig.S
  with type t = [ `Initialized | `Uninitialized | `MaybeInitialized ]

(** {2 Builtins} *)

module BuiltinsOverrides:
  Parameter_sig.Map with type key = Cil_types.kernel_function
                     and type value = string
module BuiltinsAuto: Parameter_sig.Bool
module BuiltinsList: Parameter_sig.Bool

module AllocBuiltin: Parameter_sig.String
module AllocFunctions: Parameter_sig.String_set
module AllocReturnsNull: Parameter_sig.Bool
module MallocLevel: Parameter_sig.Int

(** {2 Analysis behavior} *)

module UseSpec: Parameter_sig.Kernel_function_set
module SkipLibcSpecs: Parameter_sig.Bool

module RecursiveUnroll: Parameter_sig.Int

module RmAssert: Parameter_sig.Bool

module InterpreterMode: Parameter_sig.Bool
module StopAtNthAlarm: Parameter_sig.Int

(** {2 Non standard alarms} *)

module WarnCopyIndeterminate: Parameter_sig.Kernel_function_set
module WarnSignedConvertedDowncast: Parameter_sig.Bool
module WarnPointerSubtraction: Parameter_sig.Bool
module WarnPointerComparison: Parameter_sig.S
  with type t = [ `All | `Pointer | `None ]

module UndefinedPointerComparisonPropagateAll: Parameter_sig.Bool

module InitializedLocals: Parameter_sig.Bool

(** {2 Verbosity and outputs} *)

module Verbose: Parameter_sig.Int
module ShowPerf: Parameter_sig.Bool
module ShowSlevel: Parameter_sig.Int
module PrintCallstacks: Parameter_sig.Bool
module Flamegraph: Parameter_sig.Filepath
module StatisticsFile: Parameter_sig.Filepath
module ReportRedStatuses: Parameter_sig.Filepath
module Annot: Parameter_sig.Kernel_function_set

(** {2 Experimental} *)

module ReduceOnLogicAlarms: Parameter_sig.Bool

module AllRoundingModesConstants: Parameter_sig.Bool

type descending_strategy = NoIteration | FullIteration | ExitIteration
module DescendingIteration: Parameter_sig.S with type t = descending_strategy
module HierarchicalConvergence: Parameter_sig.Bool

(** {2 Utility functions} *)

(** Returns the list (name, descr) of currently enabled abstract domains. *)
val enabled_domains: unit -> (string * string) list

(** [use_builtin kf name] instructs the analysis to use the builtin [name]
    to interpret calls to function [kf].
    Raises [Not_found] if there is no builtin of name [name]. *)
val use_builtin: Cil_types.kernel_function -> string -> unit

(** [use_global_value_partitioning vi] instructs the analysis to use
    value partitioning on the global variable [vi]. *)
val use_global_value_partitioning: Cil_types.varinfo -> unit

(** This function should be called whenever the correctness of the analysis
    is externally changed through the Eva API, to ensure that the property
    statuses emitted by Eva are properly reset. *)
val change_correctness: unit -> unit

(** {2 Internal use only} *)

(* Automatically sets some parameters according to the meta-option
   -eva-precision. *)
val configure_precision: unit -> unit

(** List of parameters having an impact on the correctness of the analysis. *)
val parameters_correctness: Typed_parameter.t list

(** List of parameters having an impact only on the analysis precision. *)
val parameters_tuning: Typed_parameter.t list

(** Registers available cvalue builtins for the -eva-builtin option. *)
val register_builtin: string -> unit

(** Unregister a cvalue builtin. *)
val unregister_builtin: string -> unit

(** Registers available domain names for the -eva-domains option. *)
val register_domain: name:string -> descr:string -> priority:int -> unit
