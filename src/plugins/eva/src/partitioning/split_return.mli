(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module is used to merge together the final states of a function
    according to a given strategy. Default is to merge all states together *)

val pretty_strategies: unit -> unit

val kf_strategy: Kernel_function.t -> Split_strategy.t
