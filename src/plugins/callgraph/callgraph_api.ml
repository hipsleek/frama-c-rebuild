(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Callgraph API *)

module type Graph = sig

  module G: Graph.Sig.G
  (** The underlying graph datastructure *)

  val compute: unit -> unit
  (** Compute the graph *)

  val get: unit -> G.t
  (** Get the graph from the AST. *)

  module Subgraph: sig val get: unit -> G.t end
  (** Subgraph of [get ()] wrt [Options.Roots.get ()] *)

  val dump: unit -> unit
  (** Dump the (possibly sub-)graph in the file of the corresponding command
      line argument. *)

  val is_computed: unit -> bool
  (** Is the graph already built? *)

  val add_hook: (G.t -> unit) -> unit
  (** Call registered hook each time the graph is computed *)

  val self: State.t

end

(** Signature for a callgraph. Each edge is labeled by the callsite. Its source
    is the caller, while the destination is the callee. *)
module type S = Graph with type G.V.t = Kernel_function.t
                       and type G.E.label = Cil_types.stmt

(** Signature for a graph of services *)
module type Services = sig

  include Graph with type G.V.t = Kernel_function.t Service_graph.vertex
                 and type G.E.label = Service_graph.edge

  val entry_point: unit -> G.V.t option
  val is_root: Kernel_function.t -> bool
end
