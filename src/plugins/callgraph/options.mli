(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

val name: string
module Filename: Parameter_sig.Filepath
module Roots: Parameter_sig.Kernel_function_set
module Service_roots: Parameter_sig.Kernel_function_set
module Function_pointers: Parameter_sig.Bool
module Uncalled: Parameter_sig.Bool
module Uncalled_leaf: Parameter_sig.Bool
module Services: Parameter_sig.Bool

val dump: (out_channel -> 'a -> unit) -> 'a -> unit
(** dump the given value into [Filename.get ()] by using [output] *)
