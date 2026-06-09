(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Callgraph plugin. *)

module Options: sig
  include Plugin.S
  module Filename: Parameter_sig.Filepath
  module Service_roots: Parameter_sig.Kernel_function_set
  module Uncalled: Parameter_sig.Bool
  module Uncalled_leaf: Parameter_sig.Bool
  module Services: Parameter_sig.Bool
  module Roots : Parameter_sig.Kernel_function_set
end

module Cg: module type of Cg
(** The callgraph itself *)

module Services: module type of Services
(** The graph of services built on top of the callgraph *)

module Uses: module type of Uses
(** Several useful functions over the callgraph *)
