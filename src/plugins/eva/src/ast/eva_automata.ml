(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eva_ast

(* --- Vertices and Edges types --- *)

type vertex_info = Interpreted_automata.vertex_info

type vertex = {
  vertex_kf : kernel_function;
  vertex_key : int;
  vertex_start_of : Cil_types.stmt option;
  vertex_info : vertex_info;
  mutable vertex_wto_index : vertex list;
}

type guard_kind = Interpreted_automata.guard_kind = Then | Else

type transition =
  | Skip
  | Enter of block
  | Leave of block
  | Return of exp option * stmt
  | Guard of exp * guard_kind * stmt
  | Assign of lval * exp * stmt
  | Call of lval option * lhost * exp list * stmt
  | Init of varinfo * init * stmt
  | Asm of attributes * string list * extended_asm option * stmt

type edge = {
  edge_kf : kernel_function;
  edge_key : int;
  edge_kinstr : kinstr;
  edge_transition : transition;
  edge_loc : location;
}

let dummy_vertex = {
  vertex_kf = List.hd (Cil_datatype.Kf.reprs);
  vertex_key = -1;
  vertex_start_of = None;
  vertex_info = NoneInfo;
  vertex_wto_index = [];
}

let dummy_edge = {
  edge_kf = List.hd (Cil_datatype.Kf.reprs);
  edge_key = -1;
  edge_kinstr = Kglobal;
  edge_transition = Skip;
  edge_loc = Fileloc.unknown;
}

let (<?>) c lcmp =
  if c <> 0 then c else Lazy.force lcmp

module Vertex = struct
  include Datatype.Make_with_collections (struct
      include Datatype.Serializable_undefined
      type t = vertex
      let reprs = [dummy_vertex]
      let name = "Eva_automata.Vertex"
      let compare v1 v2 =
        v1.vertex_key - v2.vertex_key <?>
        lazy (Kernel_function.compare v1.vertex_kf v2.vertex_kf)
      let hash v =
        Hashtbl.hash (Kernel_function.hash v.vertex_kf, v.vertex_key)
      let equal v1 v2 =
        v1.vertex_key = v2.vertex_key &&
        Kernel_function.equal v1.vertex_kf v2.vertex_kf
      let pretty fmt v =
        Format.pp_print_int fmt v.vertex_key;
        Option.iter
          (fun stmt -> Format.fprintf fmt "@s%d" stmt.sid)
          v.vertex_start_of
    end)

  let stmt v =
    match v.vertex_info with
    | LoopHead { stmt } -> Some stmt
    | NoneInfo -> v.vertex_start_of

  let is_loop_head v =
    match v.vertex_info with
    | LoopHead _ -> true
    | NoneInfo -> false

  let loc v = v.vertex_start_of |> Option.map Cil_datatype.Stmt.loc
end

module Transition = Datatype.Make (struct
    include Datatype.Serializable_undefined
    type t = transition
    let name = "Eva_automata.Transition"
    let reprs = [Skip]
    let pretty fmt =
      let open Format in
      let print_var_list fmt l =
        Pretty_utils.pp_list ~sep:", " Printer.pp_varinfo fmt l
      in
      function
      | Skip -> ()
      | Return (None,_) -> fprintf fmt "return"
      | Return (Some exp,_) -> fprintf fmt "return %a" Eva_ast.pp_exp exp
      | Guard (exp,Then,_) -> Eva_ast.pp_exp fmt exp
      | Guard (exp,Else,_) -> fprintf fmt "!(%a)" Eva_ast.pp_exp exp
      | Assign (_,_,stmt)
      | Call (_,_,_,stmt)
      | Init (_,_,stmt)
      | Asm (_,_,_,stmt) -> Printer.pp_stmt fmt stmt
      | Enter (b) -> fprintf fmt "Enter %a" print_var_list b.blocals
      | Leave (b)  -> fprintf fmt "Exit %a" print_var_list b.blocals
  end)

module Edge = struct
  include Datatype.Make_with_collections
      (struct
        include Datatype.Serializable_undefined
        type t = edge
        let reprs = [dummy_edge]
        let name = "Eva_automata.Edge"
        let compare e1 e2 =
          e1.edge_key - e2.edge_key <?>
          lazy (Kernel_function.compare e1.edge_kf e2.edge_kf)
        let hash e =
          Hashtbl.hash (Kernel_function.hash e.edge_kf, e.edge_key)
        let equal e1 e2 =
          e1.edge_key = e2.edge_key &&
          Kernel_function.equal e1.edge_kf e2.edge_kf
        let pretty fmt e = Transition.pretty fmt e.edge_transition
      end)
  let loc e = Some e.edge_loc
end


(* --- Automata types --- *)

module G = Interpreted_automata.MakeGraph (Vertex) (Edge)

type graph = G.t
type wto = G.wto

module StmtTable = Cil_datatype.Stmt.Hashtbl

type automaton = {
  graph : graph;
  wto : wto;
  entry_point : vertex;
  return_point : vertex;
  stmt_table : (vertex * vertex) StmtTable.t;
}

module Automaton = Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = automaton
      let reprs = [{
          graph=G.create ();
          wto=[];
          entry_point=dummy_vertex;
          return_point=dummy_vertex;
          stmt_table=StmtTable.create 0;
        }]
      let name = "Eva_automata.Automaton"
      let pretty fmt automaton = G.pretty fmt automaton.graph
    end)

(* A vertex should be chosen as a widening point depending on its priority :
   high priority vertices are start of statements and loop heads. *)
let wto_priority v =
  match v.vertex_info with
  | LoopHead _ -> 1
  | NoneInfo ->
    match v.vertex_start_of with
    | Some _stmt -> 1
    | None -> 0

let wto_pref v1 v2 =
  wto_priority v1 - wto_priority v2

let build_wto graph entry_point =
  G.build_wto ~pref:wto_pref graph entry_point


(* Automata translation *)

let translate_instr stmt instr =
  let translate_call dest callee args _loc =
    let dest' = Option.map translate_lval dest in
    let callee' = translate_host callee in
    let args' = List.map translate_exp args in
    Call (dest', callee', args', stmt)
  in
  match instr with
  | Cil_types.Set (lval, exp, _loc) ->
    let lval' = translate_lval lval in
    let exp' = translate_exp exp in
    Assign (lval', exp', stmt)
  | Call (dest, callee, args, loc) ->
    translate_call dest callee args loc
  | Local_init (dest, Cil_types.ConsInit (callee, args, k), loc) ->
    Cil.treat_constructor_as_func translate_call dest callee args k loc
  | Local_init (vi, Cil_types.AssignInit init, _loc) ->
    let init' = translate_init init in
    Init (vi, init', stmt)
  | Asm (attributes, string_list, ext_asm_opt, _loc) ->
    Asm (attributes, string_list, ext_asm_opt, stmt)
  | Skip (_loc) | Code_annot (_, _loc) ->
    Skip

let translate_transition transition =
  match transition with
  | Interpreted_automata.Skip -> Skip
  | Return (exp_opt, stmt) ->
    Return (Option.map translate_exp exp_opt, stmt)
  | Guard (exp, guard_kind, stmt) ->
    Guard (translate_exp exp, guard_kind, stmt)
  | Prop _ ->
    Skip
  | Instr (inst, stmt) ->
    translate_instr stmt inst
  | Enter block ->
    Enter block
  | Leave block ->
    Leave block

(* Fill the wto index of the vertices *)
let build_wto_index wto =
  let rec iter_wto index w =
    List.iter (iter_element index) w
  and iter_element index = function
    | Wto.Node v ->
      v.vertex_wto_index <- index
    | Wto.Component (h, w) ->
      let new_index = h :: index in
      iter_wto new_index (Wto.Node h :: w)
  in
  iter_wto [] wto

let translate_automaton kf =
  let module Src = Interpreted_automata in
  let module VertexTable = Src.Vertex.Hashtbl in
  let src = Interpreted_automata.build_automaton ~annotations:true kf in
  let size = Src.(G.nb_vertex src.graph) in
  let graph = G.create ~size () in
  let table = VertexTable.create size in
  let translate_vertex (v : Src.vertex) =
    let v' = {
      vertex_kf = kf;
      vertex_key = v.vertex_key;
      vertex_start_of = v.vertex_start_of;
      vertex_info = v.vertex_info;
      vertex_wto_index = [];
    }
    in
    G.add_vertex graph v';
    VertexTable.add table v v'
  and translate_edge (v, e, w) =
    let v' = VertexTable.find table v
    and w' = VertexTable.find table w
    and e' = {
      edge_kf = e.Src.edge_kf;
      edge_key = e.Src.edge_key;
      edge_kinstr = e.Src.edge_kinstr;
      edge_transition = translate_transition e.Src.edge_transition;
      edge_loc = e.Src.edge_loc;
    }
    in
    G.add_edge_e graph (v',e',w')
  and translate_stmt_table t =
    let module T = Cil_datatype.Stmt.Hashtbl in
    let t' = T.create (T.length t) in
    let translate_pair (v1, v2) =
      VertexTable.find table v1, VertexTable.find table v2
    in
    T.iter (fun stmt (v1, v2) -> T.add t' stmt (translate_pair (v1, v2))) t;
    t'
  in
  Src.G.iter_vertex translate_vertex src.graph;
  Src.G.iter_edges_e translate_edge src.graph;
  let entry_point = VertexTable.find table src.entry_point
  and return_point = VertexTable.find table src.return_point
  and stmt_table = translate_stmt_table src.stmt_table in
  let wto = build_wto graph entry_point in
  build_wto_index wto;
  { graph; wto; entry_point; return_point; stmt_table }

module State = Kernel_function.Make_Table (Automaton)
    (struct
      let size = 97
      let name = "Eva_automata.State"
      let dependencies = [Ast.self]
    end)

let get_automaton = State.memo translate_automaton

let exit_strategy automaton = G.exit_strategy automaton.graph

let output_to_dot out automaton =
  G.output_to_dot ~wto:automaton.wto out automaton.graph

let wto_index_diff v1 v2 =
  let index1 = v1.vertex_wto_index and index2 = v2.vertex_wto_index in
  let rec remove_common_prefix l1 l2 =
    match l1, l2 with
    | x :: l1, y :: l2 when Vertex.equal x y ->
      remove_common_prefix l1 l2
    | l1, l2 -> l1, l2
  in
  let l1 = List.rev index1
  and l2 = List.rev index2
  in
  let left, entered = remove_common_prefix l1 l2 in
  List.rev left, entered

let is_wto_head v =
  match v.vertex_wto_index with
  | v' :: _ -> Vertex.equal v v'
  | [] -> false

let is_back_edge (v1,v2) =
  List.exists (Vertex.equal v2) (v1.vertex_wto_index)


(* Loop identification *)

type loop = {
  graph: graph;
  head: vertex;
  wto: wto;
  stmt: stmt;
}

let loop_stmt head =
  match head.vertex_info with
  | LoopHead { stmt } -> stmt
  | NoneInfo -> Option.get head.vertex_start_of

let find_loop (automaton: automaton) vertex =
  let graph = automaton.graph in
  match vertex.vertex_wto_index with
  | [] ->
    None
  | head :: _ ->
    (* Find in the wto the component whose head is [head]. *)
    let rec find = function
      | [] -> assert false
      | Wto.Node _ :: tl -> find tl
      | Wto.Component (h, l) :: tl ->
        if Vertex.equal h head
        then {graph; head; wto = l; stmt = loop_stmt head}
        else find (l @ tl)
    in
    Some (find automaton.wto)


(* Dataflow analysis *)

type 'a widening = 'a G.widening = Fixpoint | Widening of 'a

module type Domain = G.Domain

module ForwardAnalysis (D : Domain) =
struct
  module Analysis = G.ForwardAnalysis (D)

  let fixpoint automaton initial_state =
    let wto = (automaton : automaton).wto in
    Analysis.compute automaton.graph wto initial_state
end

module BackwardAnalysis (D : Domain) =
struct
  module Analysis = G.BackwardAnalysis (D)

  let build_wto automaton =
    let init = automaton.return_point
    and succs = fun v -> G.pred automaton.graph v
    and pref = wto_pref in
    G.WTO.partition ~pref ~init ~succs

  let fixpoint automaton initial_state =
    let wto = build_wto automaton in
    Analysis.compute automaton.graph wto initial_state
end
