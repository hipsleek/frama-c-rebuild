(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*  INSA (Institut National des Sciences Appliquees)                          *)
(*                                                                            *)
(******************************************************************************)

open Automaton_ast

type state = Automaton_ast.state
type transition = Automaton_ast.typed_trans

module Vertex =
struct
  type t = state
  let compare x y = Stdlib.compare x.nums y.nums
  let hash x = Hashtbl.hash x.nums
  let equal x y = x.nums = y.nums
  let default = {
    nums = -1; name = ""; multi_state = None;
    acceptation = Bool3.False; init = Bool3.False
  }
end

module Edge =
struct
  type t = transition
  let compare x y = Stdlib.compare x.numt y.numt
  let default = {
    numt = -1; start = Vertex.default; stop = Vertex.default;
    cross = TTrue; actions = []
  }
end

include Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)

let of_automaton auto =
  let g = create () in
  List.iter (fun t -> add_edge_e g (t.start,t,t.stop)) auto.trans;
  g

let states g =
  fold_vertex (fun v acc -> v :: acc) g []

let filter_states f g =
  fold_vertex (fun v acc -> if f v then v :: acc else acc) g []

let init_states g =
  filter_states (fun v -> v.Automaton_ast.init = Bool3.True) g

let edges g =
  fold_edges_e (fun e acc -> e :: acc) g []
