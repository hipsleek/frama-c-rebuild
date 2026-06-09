(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Make (_ : sig val kf: kernel_function end) : sig
  val widening_delay : int
  val widening_period : int
  val unroll : Eva_automata.loop -> Partition.unroll_limit
  val history_size : int
  val universal_splits : Partition.action list

  (** Returns the partitioning actions to be applied on the analysis flow at the
      given vertex and a boolean indicating whether the propagated states must
      be stored (and duplicate states filtered out). *)
  val flow_actions : Eva_automata.vertex -> Partition.action list * bool

  val call_return_policy : Partition.call_return_policy
end
