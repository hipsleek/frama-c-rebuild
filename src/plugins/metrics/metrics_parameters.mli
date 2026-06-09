(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

module Enabled: Parameter_sig.Bool
(** Activate metrics *)

module ByFunction: Parameter_sig.Bool
(** Activate metrics by function *)

module ValueCoverage: Parameter_sig.Bool
(** Give an estimation about value analysis code penetration.
    Only works on CIL AST. *)

module AstType: Parameter_sig.String
(** Set the ASTs on which the metrics should be computed *)

module OutputFile: Parameter_sig.Filepath
(** Pretty print metrics to the given file.
    The output format will be recognized through the extension.
    Supported extensions are:
    "html" or "htm" for HTML
    "txt" or "text" for text
*)

module SyntacticallyReachable: Parameter_sig.Kernel_function_set
(** Set of functions for which we compute the functions they may call *)

module LocalsSize: Parameter_sig.Kernel_function_set
(** Compute and print the total size of local variables for all functions in
    this set (option -metrics-locals-size) *)

module Libc: Parameter_sig.Bool

module UsedFiles: Parameter_sig.Bool
