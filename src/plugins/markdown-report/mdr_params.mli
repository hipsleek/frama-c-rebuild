(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

(** Value of [-mdr-out]. *)
module Output: Parameter_sig.Filepath

(** Value of [-mdr-gen]. *)
module Generate: Parameter_sig.String

(** Value of [-mdr-remarks]. *)
module Remarks: Parameter_sig.Filepath

(** Value of [-mdr-flamegraph]. *)
module FlameGraph: Parameter_sig.String

(** Value of [-mdr-authors]. *)
module Authors: Parameter_sig.String_list

(** Value of [-mdr-title]. *)
module Title: Parameter_sig.String

(** Value of [-mdr-date]. *)
module Date: Parameter_sig.String

(** Value of [-mdr-stubs]. *)
module Stubs: Parameter_sig.String_list

(** Value of [-mdr-print-libc]. *)
module PrintLibc: Parameter_sig.Bool

(** Value of [-mdr-sarif-deterministic]. *)
module SarifDeterministic: Parameter_sig.Bool
