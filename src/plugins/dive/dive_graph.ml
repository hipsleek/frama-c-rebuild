(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Dive_types

let fresh_key =
  let next_key = ref 0 in
  fun () -> incr next_key; !next_key

let new_node
    ?(node_kind=Error "no kind")
    ?(node_locality={loc_file=""; loc_callstack=[]})
    () = {
  node_key = fresh_key ();
  node_kind;
  node_locality;
  node_is_root = false;
  node_hidden = false;
  node_values = None;
  node_range = Empty;
  node_taint = None;
  node_writes_computation = NotDone;
  node_reads_computation = NotDone;
  node_writes = [];
}

module Node = Datatype.Make_with_collections
    (struct
      type t = node
      include Datatype.Serializable_undefined
      let name = "Dive.Node"
      let reprs = [ new_node () ]
      let compare n1 n2 = Datatype.Int.compare n1.node_key n2.node_key
      let hash n = n.node_key
      let equal n1 n2 = n1.node_key = n2.node_key
      let pretty fmt n = Format.pp_print_int fmt n.node_key
    end)

module Dependency =
struct
  type t = dependency
  let compare e1 e2 = e1.dependency_key - e2.dependency_key
  let hash e = e.dependency_key
  let equal e1 e2 = e1.dependency_key = e2.dependency_key
  let default = {
    dependency_key = -1;
    dependency_kind = Data;
    dependency_origins = []
  }
end

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Node) (Dependency)
include G


let create_node ~node_kind ~node_locality g =
  let node = new_node ~node_kind ~node_locality () in
  add_vertex g node;
  node

let remove_node = remove_vertex

let create_dependency g ~origin ~kind v1  v2 =
  let same_kind (_,e,_) =
    e.dependency_kind = kind
  in
  let matching_edge =
    try
      Some (List.find same_kind (G.find_all_edges g v1 v2))
    with Not_found -> None
  in
  let e = match matching_edge with
    | Some (_,e,_) -> e
    | None ->
      let e = {
        dependency_key = fresh_key ();
        dependency_kind = kind;
        dependency_origins = []
      }
      in
      add_edge_e g (v1,e,v2);
      e
  in
  (* Add origin *)
  let add_uniq l x =
    List.sort_uniq Studia.Writes.compare (x :: l)
  in
  e.dependency_origins <- add_uniq e.dependency_origins origin;
  (v1,e,v2)


let remove_dependency g edge =
  remove_edge_e g edge

let remove_dependencies g node =
  iter_pred_e (remove_dependency g) g node


let update_node_values node ~typ ~cvalue ~taint =
  let join_taint t1 t2 =
    let open Eva.Results in
    match t1, t2 with
    | Direct, _ | _, Direct -> Direct
    | Indirect, _ | _, Indirect -> Indirect
    | Untainted, Untainted -> Untainted
  in
  node.node_values <- Some (
      Option.fold ~some:(Cvalue.V.join cvalue) ~none:cvalue node.node_values);
  node.node_range <-
    Node_range.(upper_bound node.node_range (evaluate cvalue typ));
  Option.iter (fun taint ->
      node.node_taint <- Some (
          Option.fold ~some:(join_taint taint) ~none:taint node.node_taint)) taint

let find_independent_nodes g roots =
  let module Dfs = Graph.Traverse.Dfs (struct
      include G
      (* Consider the graph as unoriented *)
      let iter_succ f g n =
        iter_pred f g n;
        iter_succ f g n

      let fold_succ f g n acc =
        let acc = fold_pred f g n acc in
        let acc = fold_succ f g n acc in
        acc
    end)
  in
  let module Table = Hashtbl.Make (Node) in
  let table = Table.create 13 in
  List.iter (Dfs.prefix_component (fun n -> Table.add table n true) g) roots;
  fold_vertex (fun n acc -> if Table.mem table n then acc else n :: acc) g []


let bfs ?(iter_succ=iter_succ) ?(limit=max_int) g roots =
  let module Table = Hashtbl.Make (Node) in
  let explored : int Table.t = Table.create 13
  and queue : (node * int) Queue.t = Queue.create () in
  (* Add roots to queue *)
  List.iter (fun root -> Queue.add (root,0) queue) roots;
  (* Iterate over the queue *)
  while not (Queue.is_empty queue) do
    let (n,d) = Queue.take queue in
    if d <= limit && not (Table.mem explored n) then begin
      Table.add explored n d;
      iter_succ (fun n' -> Queue.add (n',d+1) queue) g n
    end
  done;
  (* Convert the result to list *)
  Table.fold (fun n _ l -> n :: l) explored []


let output_to_dot out_channel g =
  let open Graph.Graphviz.DotAttributes in
  (* let g = add_dummy_nodes g in *)

  let build_label s = `HtmlLabel (String.html_escape s) in

  let module FileTable = Datatype.String.Hashtbl in
  let module CallstackTable = Callstack.Hashtbl in
  let file_table = FileTable.create 13
  and callstack_table = CallstackTable.create 13 in
  let file_counter = ref 0 in
  let callstack_counter = ref 0 in
  let rec build_file_subgraph filename =
    incr file_counter;
    {
      sg_name = "file_" ^ (string_of_int !file_counter);
      sg_attributes = [build_label filename];
      sg_parent = None;
    }
  and build_callstack_subgraph = function
    | [] -> None
    | (kf,_kinstr) :: stack ->
      let parent = get_callstack_subgraph stack in
      incr callstack_counter;
      Some {
        sg_name = "cs_" ^ (string_of_int !callstack_counter);
        sg_attributes = [build_label (Kernel_function.get_name kf)];
        sg_parent = Option.map (fun sg -> sg.sg_name) parent;
      }
  and get_file_subgraph filename =
    FileTable.memo file_table filename build_file_subgraph
  and get_callstack_subgraph cs =
    CallstackTable.memo callstack_table cs build_callstack_subgraph
  in

  let module Dot = Graph.Graphviz.Dot (
    struct
      include G
      let graph_attributes _g = []
      let default_vertex_attributes _g = []
      let vertex_name v = "cp" ^ (string_of_int v.node_key)
      let vertex_attributes v =
        let l = ref [] in
        let text = Pretty_utils.to_string Node_kind.pretty v.node_kind in
        if text <> "" then
          l := build_label text :: !l;
        let kind = match v.node_kind with
          | Scalar _ -> [`Shape `Box]
          | Composite _ -> [ `Shape `Box3d ]
          | Scattered _ -> [ `Shape `Parallelogram ]
          | Unknown _ -> [`Shape `Diamond ; `Color 0xff0000]
          | Alarm _ ->  [ `Shape `Doubleoctagon ;
                          `Style `Bold ; `Color 0xff0000 ;
                          `Style `Filled ; `Fillcolor 0xff0000 ]
          | AbsoluteMemory -> [`Shape `Box3d]
          | Const _ -> [`Shape `Ellipse]
          | Error _ -> [`Color 0xff0000]
        and range = match v.node_range with
          | Empty -> []
          | Singleton ->
            [`Color 0x88aaff ; `Style `Filled ; `Fillcolor 0xaaccff ]
          | Normal _ ->
            [ `Color 0x004400 ; `Style `Filled ; `Fillcolor 0xeeffee ]
          | Wide ->
            [ `Color 0xff0000 ; `Style `Filled ; `Fillcolor 0xffbbbb ]
        in
        l := range @ kind @ !l;
        if v.node_writes_computation <> Done then
          l := [ `Style `Dotted ] @ !l;
        if v.node_is_root then
          l := [ `Style `Bold ] @ !l;
        !l
      let get_subgraph v =
        let {loc_file ; loc_callstack} = v.node_locality in
        match loc_callstack with
        | [] -> Some (get_file_subgraph loc_file)
        | cs -> get_callstack_subgraph cs
      let default_edge_attributes _g = []
      let edge_attributes (_v1,e,_v2) =
        let kind_attribute = match e.dependency_kind with
          | Callee -> [`Color 0x00ff00 ]
          | _ -> []
        and folding_attribute = match e.dependency_origins with
          | [] | [_] -> []
          | _ -> [ `Style `Bold ]
        in kind_attribute @ folding_attribute
    end)
  in
  Dot.output_graph out_channel g
