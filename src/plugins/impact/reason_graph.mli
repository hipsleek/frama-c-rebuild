(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Pdg_types

(** Why is a node impacted. The reasons will be given as [n is impacted
    by the effect of [n'], and the impact is of type reason]. *)
type reason_type =
  | Intraprocedural of PdgTypes.Dpd.t
  (** The effect of [n'] in [f] impact [n], which is also in [f]. *)

  | InterproceduralDownward (** the effect of [n'] in [f] has an effect on a
                                callee [f'] of [f], in which [n] is located. *)

  | InterproceduralUpward  (** the effect of [n'] in [f] has an effect on a
                               caller [f'] of [f] (once the call to [f] has ended), [n] being in [f']. *)

module ReasonType: Datatype.S with type t = reason_type

module Reason: Datatype.S_with_collections
  with type t = PdgTypes.Node.t * PdgTypes.Node.t * reason_type

type reason_graph = Reason.Set.t

(** Map from a node to the kernel_function it belongs to *)
type nodes_origin = Cil_types.kernel_function PdgTypes.Node.Map.t

type reason = {
  reason_graph: reason_graph;
  nodes_origin: nodes_origin;
  initial_nodes: Pdg_aux.NS.t;
}

module DatatypeReason: Datatype.S with type t = reason

val empty: reason

val to_dot_formatter:
  ?in_kf:Cil_types.kernel_function -> reason -> Format.formatter -> unit

val print_dot_graph: reason -> unit
