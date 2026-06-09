(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Region_analysis_sig
open Cil_types

module type M = sig
  val kf: Kernel_function.t

  type abstract_value
  val compile_node: stmt -> abstract_value -> (stmt edge * abstract_value) list
  val mu: (abstract_value -> abstract_value) -> abstract_value -> abstract_value
  val join: abstract_value list -> abstract_value
end

(* Helper function to make region analysis on Frama-C stmts. Produces
   a Node suitable as an argument to the [Region_analysis.Make]
   functor.*)
module MakeNode(M:M):Node with type abstract_value = M.abstract_value
                           and type node = stmt
