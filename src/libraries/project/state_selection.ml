(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Selection = Graph.Persistent.Digraph.ConcreteBidirectional(State)

(* Set of the states in a selection *)
type concrete_state_selection =
  | Full
  | Subset of Selection.t

(* Reification of the atomic operations that are used to create a selection *)
type witness =
  | WAll
  | WEmpty
  | WSingleton of State.t
  | WOfStateList of State.t list
  | WDependencies of State.t
  | WStrictDependencies of State.t
  | WCoDependencies of State.t
  | WStrictCoDependencies of State.t
  | WUnion of witness * witness
  | WDiff of witness * witness
  | WOfList of witness list

let rec pretty_witness fmt = function
  | WAll -> Format.pp_print_string fmt "*"
  | WEmpty -> Format.pp_print_string fmt "<>"
  | WSingleton s -> pretty_state fmt s
  | WOfStateList l ->
    Pretty_utils.pp_list ~pre:"[@[<hv>" ~suf:"@]]" ~sep:",@ " pretty_state fmt l
  | WDependencies s -> Format.fprintf fmt "Deps(%a)" pretty_state s
  | WStrictDependencies s -> Format.fprintf fmt "StrictDeps(%a)" pretty_state s
  | WCoDependencies s -> Format.fprintf fmt "CoDeps(%a)" pretty_state s
  | WStrictCoDependencies s ->
    Format.fprintf fmt "StrictCoDeps(%a)" pretty_state s
  | WUnion (w1, w2) ->
    Format.fprintf fmt "@[<hv 1>Union(%a,@ %a)@]"
      pretty_witness w1 pretty_witness w2
  | WDiff (w1, w2) ->
    Format.fprintf fmt "@[<hv 1>Diff(%a,@ %a)@]"
      pretty_witness w1 pretty_witness w2
  | WOfList l ->
    Pretty_utils.pp_list ~pre:"[@[<hv>" ~suf:"@]]" ~sep:",@ "
      pretty_witness fmt l
and pretty_state fmt s =
  Format.pp_print_string fmt (State.get_name s)

type state_selection = concrete_state_selection * witness

let empty = Subset Selection.empty, WEmpty
let full = Full, WAll
let singleton s = Subset (Selection.add_vertex Selection.empty s), WSingleton s
let of_list l =
  Subset (List.fold_left Selection.add_vertex Selection.empty l), WOfStateList l

let is_empty (sel, _) = sel = Subset Selection.empty
let is_full (sel, _) = sel = Full
let mem (sel, _) s = match sel with
  | Full -> true
  | Subset sel -> Selection.mem_vertex sel s

include Datatype.Make
    (struct
      include Datatype.Undefined
      type t = state_selection
      let name = "State_selection"
      let reprs = [ full; empty; singleton State.dummy ]
    end)

let transitive_closure next_vertices s =
  let rec visit acc v =
    next_vertices
      (fun v' acc ->
         let e = v, v' in
         if Selection.mem_edge_e acc e then acc
         else visit (Selection.add_edge_e acc e) v')
      State_dependency_graph.graph v acc
  in
  (* add [s] in the selection even if it has no ingoing/outgoing edges *)
  visit (Selection.add_vertex Selection.empty s) s

let with_dependencies s =
  Subset (transitive_closure State_dependency_graph.G.fold_succ s),
  WDependencies s

let with_codependencies s =
  Subset (transitive_closure State_dependency_graph.G.fold_pred s),
  WCoDependencies s

let only_dependencies s =
  let g = transitive_closure State_dependency_graph.G.fold_succ s in
  Subset (Selection.remove_vertex g s), WStrictDependencies s

let only_codependencies s =
  let g = transitive_closure State_dependency_graph.G.fold_pred s in
  Subset (Selection.remove_vertex g s), WStrictCoDependencies s

let diff (sel1, w1) (sel2, w2 as selw2) =
  let sel = match sel1, sel2 with
    | _, Full -> Subset Selection.empty
    | Full, _sel2 when is_empty selw2 -> Full
    | Full, Subset sel2 ->
      let selection =
        State_dependency_graph.G.fold_vertex
          (fun v acc ->
             if Selection.mem_vertex sel2 v then acc
             else Selection.add_vertex acc v)
          State_dependency_graph.graph
          Selection.empty
      in
      let sel =
        State_dependency_graph.G.fold_edges
          (fun v1 v2 acc ->
             if Selection.mem_vertex sel2 v1 || Selection.mem_vertex sel2 v2
             then acc
             else Selection.add_edge acc v1 v2)
          State_dependency_graph.graph
          selection
      in
      Subset sel
    | Subset sel1, Subset sel2 ->
      Subset
        (Selection.fold_vertex
           (fun v acc -> Selection.remove_vertex acc v) sel2 sel1)
  in
  sel, WDiff (w1, w2)

module Operations = Graph.Oper.P(Selection)

let union (sel1, w1) (sel2, w2) =
  let sel = match sel1, sel2 with
    | Full, _ | _, Full -> Full
    | Subset sel1, Subset sel2 -> Subset (Operations.union sel1 sel2)
  in
  sel, WUnion (w1, w2)

let list_union l =
  let sel, _ = List.fold_left union empty l in
  let w = WOfList (List.map snd l) in
  sel, w

let cardinal (sel, _) = match sel with
  | Full -> State_dependency_graph.G.nb_vertex State_dependency_graph.graph
  | Subset sel -> Selection.nb_vertex sel

let iter_succ f (sel, _) v = match sel with
  | Full ->
    State_dependency_graph.G.iter_succ f State_dependency_graph.graph v
  | Subset sel -> Selection.iter_succ f sel v

let fold_succ f (sel, _) v acc = match sel with
  | Full ->
    State_dependency_graph.G.fold_succ f State_dependency_graph.graph v acc
  | Subset sel -> Selection.fold_succ f sel v acc

let iter f (sel, _) = match sel with
  | Full ->
    State_dependency_graph.G.iter_vertex f State_dependency_graph.graph
  | Subset sel -> Selection.iter_vertex f sel

let fold f (sel, _) acc = match sel with
  | Full ->
    State_dependency_graph.G.fold_vertex f State_dependency_graph.graph acc
  | Subset sel -> Selection.fold_vertex f sel acc

let to_list s = fold (fun s acc -> s :: acc) s []

module TG = State_topological.Make(State_dependency_graph.G)
module TS = State_topological.Make(Selection)

let iter_in_order f (sel, _) = match sel with
  | Full -> TG.iter f State_dependency_graph.graph
  | Subset sel -> TS.iter f sel

let fold_in_order f (sel, _) acc = match sel with
  | Full -> TG.fold f State_dependency_graph.graph acc
  | Subset sel -> TS.fold f sel acc

let pretty fmt sel =
  Format.fprintf fmt "contents of the selection:@\n";
  let mem s =
    State_dependency_graph.G.mem_vertex
      State_dependency_graph.graph
      s
  in
  iter_in_order
    (fun s ->
       Format.fprintf fmt "\t state %S%s@\n"
         (State.get_unique_name s)
         (if mem s then "" else "(\"" ^ State.get_name s ^ "\")"))
    sel;
  Format.pp_print_flush fmt ()

let pretty_witness fmt (_, w) =
  pretty_witness fmt w
