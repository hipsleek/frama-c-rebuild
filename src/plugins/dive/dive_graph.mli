(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Dive_types

include Graph.Sig.G
  with type V.t = node
   and type E.t = node * dependency * node

module Node : Datatype.S_with_collections with type t = node

module Dependency : Graph.Sig.COMPARABLE with type t = dependency

val create : ?size:int -> unit -> t

val create_node :
  node_kind:node_kind ->
  node_locality:node_locality -> t -> node

val remove_node : t -> node -> unit

val update_node_values : node ->
  typ:Cil_types.typ -> cvalue:Cvalue.V.t -> taint:Eva.Results.taint option ->
  unit

val create_dependency : t -> origin:origin -> kind:dependency_kind ->
  node -> node -> node * dependency * node

val remove_dependency : t -> node * dependency * node -> unit
val remove_dependencies : t -> node -> unit

val find_independent_nodes : t -> node list -> node list
val bfs : ?iter_succ:((node -> unit) -> t -> node -> unit) -> ?limit:int ->
  t -> node list -> node list

val output_to_dot : out_channel -> t -> unit
