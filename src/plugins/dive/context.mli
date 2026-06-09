(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Dive_types

type t

type element = Node of node | Edge of (node * dependency * node)

val create : unit -> t
val clear : t -> unit (* reset to almost an empty context,
                         but keeps folded and hidden bases, and hooks *)

val get_graph : t -> Dive_graph.t
val find_node : t -> int -> node
val get_max_dep_fetch_count : t -> int

val get_roots : t -> node list
val set_unique_root : t -> node -> unit
val add_root : t -> node -> unit
val remove_root : t -> node -> unit

val is_folded : t -> Cil_types.varinfo -> bool
val unfold : t -> Cil_types.varinfo -> unit
val fold : t -> Cil_types.varinfo -> unit

val is_hidden : t -> node_kind -> bool
val hide : t -> Cil_types.varinfo -> unit
val show : t -> Cil_types.varinfo -> unit

val add_node : t -> node_kind:node_kind -> node_locality:node_locality -> node
val remove_node : t -> node -> unit
val add_dep : t -> origin:origin -> kind:dependency_kind ->
  node -> node -> unit
val remove_node_deps : t -> node -> unit

val update_node_values : t -> node ->
  typ:Cil_types.typ ->
  cvalue:Addresses.Bytes.t -> taint:Eva.Results.taint option -> unit
val set_node_writes : t -> node -> origin list -> unit

val set_update_hook : t -> (element -> unit) -> unit
val set_remove_hook : t -> (element -> unit) -> unit
val set_clear_hook : t -> (unit -> unit) -> unit

