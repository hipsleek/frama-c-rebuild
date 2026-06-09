(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** State Dependency Graph.
    @since Carbon-20101201 *)

(** {2 Signatures} *)

(** Signature of a State Dependency Graph.
    It is compatible with the signature of OcamlGraph imperative graph
    [Graph.Sig.I].
    @since Carbon-20101201 *)
module type S = sig

  module G: Graph.Sig.G with type V.t = State.t
                         and type E.t = State.t * State.t
  val graph: G.t

  val add_dependencies: from:State.t -> State.t list -> unit
  (** Add an edge in [graph] from the state [from] to each state of the list.
      @since Carbon-20101201 *)

  val add_codependencies: onto:State.t -> State.t list -> unit
  (** Add an edge in [graph] from each state of the list to the state [onto].
      @since Carbon-20101201 *)

  val remove_dependencies: from:State.t -> State.t list -> unit
  (** Remove an edge in [graph] from the given state to each state of the list.
      @since Fluorine-20130401 *)

  val remove_codependencies: onto:State.t -> State.t list -> unit
  (** Remove an edge in [graph] from each state of the list to the state [onto].
      @since Oxygen-20120901 *)

end

(** Signature required by [Graph.GraphViZ.Dot]. See the OcamlGraph's
    documentation for additional details.
    @since Carbon-20101201 *)
module type Attributes = sig
  open Graph.Graphviz
  val graph_attributes: 'a -> DotAttributes.graph list
  val default_vertex_attributes: 'a -> DotAttributes.vertex list
  val vertex_name : State.t -> string
  val vertex_attributes: State.t -> DotAttributes.vertex list
  val default_edge_attributes: 'a -> DotAttributes.edge list
  val edge_attributes: State.t * State.t -> DotAttributes.edge list
  val get_subgraph : State.t -> DotAttributes.subgraph option
end

include S
val add_state: State.t -> State.t list -> unit

module Attributes: Attributes
module Dot(_: Attributes) : sig val dump: Filepath.t -> unit end
val dump: Filepath.t -> unit
