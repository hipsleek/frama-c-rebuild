(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Eva automata are [Interpreted_automata] where transitions have been
    translated to the Eva AST and where useless transitions have been
    replaced by Skip. As such, it essentially differs by its simpler
    vertex, edge and transitions types. *)

open Cil_types
open Eva_ast

type vertex_info = Interpreted_automata.vertex_info

type vertex = private {
  vertex_kf : kernel_function;
  vertex_key : int;
  vertex_start_of : Cil_types.stmt option;
  vertex_info : vertex_info;
  mutable vertex_wto_index : vertex list;
}

type guard_kind = Then | Else

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

type edge = private {
  edge_kf : kernel_function;
  edge_key : int;
  edge_kinstr : kinstr;
  edge_transition : transition;
  edge_loc : location;
}

module G : Graph.Sig.I
  with type V.t = vertex
   and  type E.t = vertex * edge * vertex
   and  type V.label = vertex
   and  type E.label = edge

type graph = G.t

type wto = vertex Wto.partition

type automaton = {
  graph : graph;
  wto : wto;
  entry_point : vertex;
  return_point : vertex;
  stmt_table : (vertex * vertex) Cil_datatype.Stmt.Hashtbl.t;
}

module Transition : Datatype.S with type t = transition
module Vertex : sig
  include Datatype.S_with_collections with type t = vertex
  val is_loop_head : t -> bool
  val stmt : t -> Cil_types.stmt option
end

module Edge : Datatype.S_with_collections with type t = edge
module Automaton : Datatype.S with type t = automaton

val get_automaton : kernel_function -> automaton
val output_to_dot : out_channel -> automaton -> unit

(* Wto related functions *)

val exit_strategy : automaton -> vertex Wto.component -> wto
val wto_index_diff : vertex -> vertex -> vertex list * vertex list
val is_wto_head : vertex -> bool
val is_back_edge : vertex * vertex -> bool

(* Loops identification *)

type loop = {
  graph: graph; (** The complete graph of the englobing function. *)
  head: vertex; (** The head of the loop. *)
  wto: wto;     (** The wto for the loop body (without the loop head). *)
  stmt: stmt;   (** The statement at the loop head. *)
}

(** Builds the loop type for the englobing loop of vertex. *)
val find_loop : automaton -> vertex -> loop option


(* ************************************************************************* *)
(** {2 Dataflow analysis} *)
(* ************************************************************************* *)

type 'a widening = Fixpoint | Widening of 'a

(** Abstract domain for the dataflow analysis.
    See {!Interpreted_automata.Domain}. *)
module type Domain =
sig
  type t
  val join : t -> t -> t
  val widen : t -> t -> t widening
  val transfer : vertex * edge * vertex -> t -> t option
end

(** Forward Dataflow analysis. See {!Interpreted_automata.ForwardAnalysis}. *)
module ForwardAnalysis (D : Domain):
sig
  val fixpoint : automaton -> D.t -> D.t Vertex.Hashtbl.t
end

(** Backward Dataflow analysis. See {!Interpreted_automata.BackwardAnalysis}. *)
module BackwardAnalysis (D : Domain):
sig
  val fixpoint : automaton -> D.t -> D.t Vertex.Hashtbl.t
end
