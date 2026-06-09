(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Dive_types

module Graph = Dive_graph


module NodeRef = Datatype.Pair_with_collections
    (Node_kind) (Callstack)

module Index = Datatype.Int.Hashtbl
module NodeTable = Hashtbl.Make (NodeRef)

module NodeSet = Graph.Node.Set
module BaseSet = Cil_datatype.Varinfo.Set

type element = Node of node | Edge of (node * dependency * node)

type t = {
  mutable graph: Graph.t;
  mutable vertex_table: node Index.t; (* node_key -> node *)
  mutable node_table: node NodeTable.t; (* node_kind * callstack -> node *)
  mutable unfolded_bases: BaseSet.t;
  mutable hidden_bases: BaseSet.t;
  mutable max_dep_fetch_count: int;
  mutable roots: NodeSet.t;
  mutable update_hook : element -> unit;
  mutable remove_hook : element -> unit;
  mutable clear_hook : unit -> unit;
}


(* --- initialization --- *)

let no_hook = fun _ -> ()

let create () =
  {
    graph = Graph.create ();
    vertex_table = Index.create 13;
    node_table = NodeTable.create 13;
    unfolded_bases = BaseSet.empty;
    hidden_bases = BaseSet.empty;
    max_dep_fetch_count = 10;
    roots = NodeSet.empty;
    update_hook = no_hook;
    remove_hook = no_hook;
    clear_hook = no_hook;
  }

let clear context =
  context.graph <- Graph.create ();
  context.vertex_table <- Index.create 13;
  context.node_table <- NodeTable.create 13;
  context.max_dep_fetch_count <- 10;
  context.roots <- NodeSet.empty;
  context.clear_hook ()


(* Hooks *)

let set_update_hook context f =
  context.update_hook <- f

let set_remove_hook context f =
  context.remove_hook <- f

let set_clear_hook context f =
  context.clear_hook <- f

let notify_node_update context node =
  context.update_hook (Node node)


(* --- Accessors --- *)

let get_graph context =
  context.graph

let find_node context node_key =
  Index.find context.vertex_table node_key

let get_max_dep_fetch_count context =
  context.max_dep_fetch_count


(* --- Roots --- *)

let get_roots context =
  NodeSet.elements context.roots

let update_roots context new_roots =
  let old_roots = context.roots in
  context.roots <- new_roots;
  let unset n =
    n.node_is_root <- false;
    context.update_hook (Node n);
  and set n =
    n.node_is_root <- true;
    context.update_hook (Node n);
  in
  NodeSet.iter unset old_roots;
  NodeSet.iter set new_roots

let set_unique_root context root =
  update_roots context (NodeSet.singleton root)

let add_root context root =
  update_roots context (NodeSet.add root context.roots)

let remove_root context root =
  update_roots context (NodeSet.remove root context.roots)


(* --- Folding --- *)

let is_folded context vi =
  not (BaseSet.mem vi context.unfolded_bases)

let fold context vi =
  context.unfolded_bases <- BaseSet.remove vi context.unfolded_bases

let unfold context vi =
  context.unfolded_bases <- BaseSet.add vi context.unfolded_bases


(* --- Base hiding --- *)

let is_hidden context node_kind =
  match Node_kind.get_base node_kind with
  | Some vi when BaseSet.mem vi context.hidden_bases -> true
  | _ -> false

let show context vi =
  context.hidden_bases <- BaseSet.add vi context.hidden_bases

let hide context vi =
  context.hidden_bases <- BaseSet.add vi context.hidden_bases


(* --- Building --- *)

let add_node context ~node_kind ~node_locality =
  let node_ref = (node_kind, node_locality.loc_callstack) in
  let add_new _ =
    let node = Graph.create_node context.graph ~node_kind ~node_locality in
    node.node_hidden <- is_hidden context node.node_kind;
    Index.add context.vertex_table node.node_key node;
    context.update_hook (Node node);
    node
  in
  NodeTable.memo context.node_table node_ref add_new

let remove_node context node =
  let node_ref = (node.node_kind, node.node_locality.loc_callstack) in
  let graph = context.graph in
  Graph.iter_succ (fun n -> n.node_writes_computation <- NotDone) graph node;
  Graph.iter_pred (fun n -> n.node_reads_computation <- NotDone) graph node;
  Graph.iter_succ_e (fun e -> context.remove_hook (Edge e)) graph node;
  Graph.iter_pred_e (fun e -> context.remove_hook (Edge e)) graph node;
  Graph.remove_node context.graph node;
  Index.remove context.vertex_table node.node_key;
  NodeTable.remove context.node_table node_ref;
  context.remove_hook (Node node)

let add_dep context ~origin ~kind src dest =
  let edge = Graph.create_dependency context.graph ~origin ~kind src dest in
  context.update_hook (Edge edge)

let remove_dep context edge =
  Graph.remove_dependency context.graph edge;
  context.remove_hook (Edge edge)

let remove_node_deps context node =
  Graph.iter_pred_e (remove_dep context) context.graph node

let update_node_values context node ~typ ~cvalue ~taint =
  Graph.update_node_values node ~typ ~cvalue ~taint;
  notify_node_update context node

let set_node_writes context node writes =
  node.node_writes <- List.sort_uniq Studia.Writes.compare writes;
  notify_node_update context node
