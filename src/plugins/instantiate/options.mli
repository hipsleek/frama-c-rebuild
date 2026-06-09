(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.General_services

(** Instantiate transformation enabled *)
module Enabled : Parameter_sig.Bool

(** Set of kernel function provided for transformation *)
module Kfs : Parameter_sig.Kernel_function_set

(** Used by [Instantiator_builder] to generate options. For a given instantiator
    the module generates an option "-instantiate-(no-)<function_name>" that defaults
    to true.
*)
module NewInstantiator
    (_ : sig val function_name: string end) : Parameter_sig.Bool

val emitter: Emitter.t
