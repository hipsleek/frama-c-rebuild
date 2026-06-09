(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.General_services

module Print: Parameter_sig.Bool
module PrintProperties: Parameter_sig.Bool

module Untried: Parameter_sig.Bool
module Specialized: Parameter_sig.Bool
module Proven: Parameter_sig.Bool

module CSVFile: Parameter_sig.Filepath
module Classify: Parameter_sig.Bool
module Rules: Parameter_sig.String_list
module Warning: Parameter_sig.String
module Error: Parameter_sig.String
module Status: Parameter_sig.Bool
module UntriedStatus: Parameter_sig.String
module UnknownStatus: Parameter_sig.String
module InvalidStatus: Parameter_sig.String

module Output: Parameter_sig.Filepath
module OutputReviews: Parameter_sig.Filepath
module OutputErrors: Parameter_sig.Filepath
module OutputUnclassified: Parameter_sig.Filepath
module AbsolutePath: Parameter_sig.Bool
module Stdout: Parameter_sig.Bool
module Stderr: Parameter_sig.Bool
module Exit: Parameter_sig.Bool
