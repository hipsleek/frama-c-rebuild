(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** behavior of option -mthread. *)
module Enabled: Parameter_sig.Bool

(** behavior of option -mt-keep-analyses. Three
    possible values: all, last or none. *)
module KeepProjects: Parameter_sig.String

(** behavior of option -mt-projects-on-disk *)
module ToDisk: Parameter_sig.Bool

(** behavior of option -mt-projects-on-disk-prefix. *)
module ToDiskPrefix: Parameter_sig.String

(** behavior of option -mt-ignore-null. *)
module IgnoreNull: Parameter_sig.Bool

(** behavior of option -mt-threads-lib. *)
module ThreadsLib: Parameter_sig.S with type t = Mt_lib.threads_lib

(** behavior of option -mt-write-races. *)
module WriteWriteRaces: Parameter_sig.Bool

(** behavior of option -mt-shared-values. From 0 to 2. *)
module DumpSharedVarsValues: Parameter_sig.Int

(** behavior of option -mt-shared-accesses-synchronization. *)
module CheckProtections: Parameter_sig.Bool

(** behavior of option -mt-interrupts*)
module InterruptHandlers: Parameter_sig.Kernel_function_set

(** behavior of option -mt-moderate-warning. *)
module ModerateWarnings: Parameter_sig.Bool

(** behavior of option -mt-print-callstacks. *)
module PrintCallstacks: Parameter_sig.Bool

(** behavior of option -mt-skip-threads. *)
module SkipThreads: Parameter_sig.String_set

(** behavior of option -mt-only-threads. *)
module OnlyThreads: Parameter_sig.String_set

(** behavior of option -mt-stop-after. *)
module StopAfter: Parameter_sig.Int

(** behavior of option -mt-concat-dot-files-to. *)
module ConcatDotFilesTo: Parameter_sig.Filepath

(** behavior of option -mt-keep-dot-files. *)
module KeepDotFiles: Parameter_sig.Bool

(** behavior of option -mt-extract. *)
module ExtractModels: Parameter_sig.String_set

(** behavior of option -mt-full-cfg. *)
module FullCfg: Parameter_sig.Bool

(** behavior of option -mt-non-shared-accesses. *)
module KeepWhiteNodes: Parameter_sig.Bool

(** behavior of option -mt-non-concurrent-accesses. *)
module KeepGreenNodes: Parameter_sig.Bool

(** behavior of option -mt-return-edges. *)
module ShowReturnEdges: Parameter_sig.Bool
