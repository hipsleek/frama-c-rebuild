(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This the API of the WP plug-in *)

(* -------------------------------------------------------------------------- *)
(** {1 High-Level External API}

    The following modules are the recommended entry points for using WP
    programmatically. They are meant to be relatively stable over time.

*)
(* -------------------------------------------------------------------------- *)

(** WP Proof Obligation Generator and Management *)
module VC = VC

(** Provers and Proof Obligations Results *)
module VCS = VCS

(** WP Plugin Interface *)
module Wp_parameters = Wp_parameters

(* -------------------------------------------------------------------------- *)
(** {1 Advanced Usage API}

    The following modules entry points for using WP advanced features,
    such as proof obligation manipulation, tactics and strategies.
    These modules might expose internal features of WP that are subject
    to change. Developers using this API are encouraged to contact us
    for a roadmap information and further collaboration.
*)
(* -------------------------------------------------------------------------- *)

(** High-Level Term Representation *)
module Repr = Repr

(** Low-Level Logic Terms and Predicates *)
module Lang = Lang

(** Generated Logic Definitions *)
module Definitions = Definitions

(** Proof Task and Simplifiers *)
module Conditions = Conditions

(** Tactics Entry Points *)
module Tactical = Tactical

(** Strategies Entry Points *)
module Strategy = Strategy

(** WP Proof Obligation Generator *)
module Generator = Generator

(** Command Line Processing *)
module Register = Register

(* -------------------------------------------------------------------------- *)
(** {1 Low-Level Internal API}

    The following modules are _not_ intended to be used externally. The target
    audience is WP plug-in developers. However, developers interested in
    such low-level features are encouraged to contact us for more information.
*)
(* -------------------------------------------------------------------------- *)

(** {2 Model Registration} *)

module Factory = Factory
module WpContext = WpContext

(** {2 Memory Models} *)

module MemDebug = MemDebug
module MemEmpty = MemEmpty
module MemLoader = MemLoader
module MemMemory = MemMemory
module MemBytes = MemBytes
module MemTyped = MemTyped
module MemVal = MemVal
module MemVar = MemVar
module MemZeroAlias = MemZeroAlias

(** {2 Other Models} *)

module Cint = Cint
module Cfloat = Cfloat
module Cmath = Cmath
module Cstring = Cstring

(** {2 State Model} *)

module Sigma = Sigma
module Passive = Passive
module Mstate = Mstate

(** {2 Model Hypotheses}*)

module MemoryContext = MemoryContext
module RefUsage = RefUsage
module WpTarget = WpTarget
module AssignsCompleteness = AssignsCompleteness

(** {2 Region Analysis} *)

module Layout = Layout

(** {2 Compilers} *)

module Memory = Memory
module Driver = Driver
module Context = Context
module Ctypes = Ctypes
module Cvalues = Cvalues
module Clabels = Clabels
module CodeSemantics = CodeSemantics
module LogicAssigns = LogicAssigns
module LogicBuiltins = LogicBuiltins
module LogicCompiler = LogicCompiler
module LogicSemantics = LogicSemantics
module LogicUsage = LogicUsage
module StmtSemantics = StmtSemantics
module Dyncall = Dyncall
module Matrix = Matrix
module NormAtLabels = NormAtLabels

(** {2 Core Engine} *)

module CfgAnnot = CfgAnnot
module CfgCalculus = CfgCalculus
module CfgCompiler = CfgCompiler
module CfgDump = CfgDump
module CfgGenerator = CfgGenerator
module CfgInfos = CfgInfos
module CfgInit = CfgInit
module CfgWP = CfgWP
module WpReached = WpReached
module WpPropId = WpPropId
module WpRTE = WpRTE

(** {2 Proof Engine} *)

module Wpo = Wpo
module Auto = Auto
module Cache = Cache
module Cleaning = Cleaning
module Letify = Letify
module Splitter = Splitter
module Filtering = Filtering

(** {2 Prover Interface} *)

module Why3Provers = Why3Provers
module Prover = Prover
module ProverTask = ProverTask
module ProverWhy3 = ProverWhy3

(** {2 Script Engine} *)

module Script = Script
module Footprint = Footprint
module ProofEngine = ProofEngine
module ProofScript = ProofScript
module ProverScript = ProverScript
module ProverSearch = ProverSearch
module ProofSession = ProofSession
module ProofStrategy = ProofStrategy

(** {2 Tactics} *)

module WpTac = WpTac
module TacArray = TacArray
module TacBitrange = TacBitrange
module TacBittest = TacBittest
module TacBitwised = TacBitwised
module TacChoice = TacChoice
module TacClear = TacClear
module TacCompound = TacCompound
module TacCongruence = TacCongruence
module TacCut = TacCut
module TacFilter = TacFilter
module TacHavoc = TacHavoc
module TacInduction = TacInduction
module TacInstance = TacInstance
module TacLemma = TacLemma
module TacModMask = TacModMask
module TacNormalForm = TacNormalForm
module TacOverflow = TacOverflow
module TacRange = TacRange
module TacRewrite = TacRewrite
module TacSequence = TacSequence
module TacShift = TacShift
module TacSplit = TacSplit
module TacUnfold = TacUnfold
module TacCompute = TacCompute

(** {2 Error Management} *)

module Warning = Warning
module Wp_error = Wp_error

(** {2 Printers and Reporting} *)

module Plang = Plang
module Pcfg = Pcfg
module Pcond = Pcond
module Ptip = Ptip
module Rformat = Rformat
module Stats = Stats
module WpReport = WpReport

(** {2 EVA Proxy} *)

module Wp_eva = Wp_eva

(* -------------------------------------------------------------------------- *)
