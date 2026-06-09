(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** An interpreted automaton is a convenient formalization of programs for
    abstract interpretation. It is a control flow graph where states are
    control point and edges are transitions. It keeps track of conditions
    on which a transition can be taken (guards) as well as actions which are
    computed when a transition is taken. It can then be interpreted w.r.t. the
    operational semantics to reproduce the behavior of the program or
    w.r.t. to the collection semantics to compute a set of reachable states.

    This intermediate representation abstracts almost completely the notion of
    statement in CIL. Edges are either CIL expressions for guards, CIL
    instructions for actions or a return Edge. Thus, it saves the higher
    abstraction layers from interpreting CIL statements and from attaching
    guards to statement successors. *)

type vertex_info =
  | NoneInfo
  | LoopHead of { stmt : stmt; level : int }

(** Control flow information for outgoing edges, if any. *)
type 'a control =
  | Edges (** control flow is only given by vertex edges. *)
  | Loop of 'a (** start of a Loop stmt, with breaking vertex. *)
  | If of { cond: exp; vthen: 'a; velse: 'a }
  (** edges are guaranteed to be two guards `Then` else `Else`
      with the given condition and successor vertices. *)
  | Switch of { value: exp; cases: (exp * 'a) list; default: 'a }
  (** edges are guaranteed to be issued from a `switch()` statement with
      the given cases and default vertices. *)

(** Vertices are control points. When a vertex is the *start* of a statement,
    this statement is kept in [vertex_start_of]. *)

type vertex = private {
  vertex_kf : Cil_types.kernel_function;
  vertex_key : int;
  vertex_blocks : Cil_types.block list;
  mutable vertex_start_of : Cil_types.stmt option;
  mutable vertex_end_of : Cil_types.stmt list;
  mutable vertex_info : vertex_info;
  mutable vertex_control : vertex control;
}

type assert_kind =
  | Invariant
  | Assert
  | Check
  | Assume

(** Maps binding the labels from an annotation to the vertices they refer to in
    the graph. *)
type 'vertex labels = 'vertex Cil_datatype.Logic_label.Map.t

type 'vertex annotation = {
  kind: assert_kind;
  predicate: identified_predicate;
  labels: 'vertex labels;
  property: Property.t;
}

(** Each transition can either be a skip (do nothing), a return, a guard
    represented by a Cil expression, a Cil instruction, an ACSL annotation
    or entering/leaving a block.
    The edge is annotated with the statement from which the transition has been
    generated. This is currently used to choose alarms locations. *)
type 'vertex transition =
  | Skip
  | Return of exp option * stmt
  | Guard of exp * guard_kind * stmt
  | Prop of 'vertex annotation * stmt
  | Instr of instr * stmt
  | Enter of block
  | Leave of block

and guard_kind = Then | Else

type 'vertex edge = private {
  edge_kf : Cil_types.kernel_function;
  edge_key : int;
  edge_kinstr : kinstr;
  edge_transition : 'vertex transition;
  edge_loc : location;
}

module G : Graph.Sig.G
  with type V.t = vertex
   and  type E.t = vertex * vertex edge * vertex
   and  type V.label = vertex
   and  type E.label = vertex edge

type graph = G.t

(** Weak Topological Order is given by a list (in topological order) of
    components of the graph, which are themselves WTOs *)
type wto = vertex Wto.partition

(** Signature for vertices' datatype. *)
module type Vertex = sig
  include Datatype.S_with_collections

  val loc : t -> location option
  (** [loc v] returns the location corresponding to the vertex if it exists. *)
end

(** Datatype for vertices *)
module Vertex : Vertex with type t = vertex

(** Signature for edges' datatype. *)
module type Edge = sig
  include Datatype.S_with_collections

  val loc : t -> location option
  (** [loc e] returns the location corresponding to the edge if it exists. *)
end

(** Datatype for edges *)
module Edge : Edge with type t = vertex edge


(** An interpreted automaton for a given function is a graph whose edges are
    guards and commands.
    - [graph] is the control flow graph
    - [entry_point]: each execution of the function starts at this vertex
    - [return_point]: return statements link to this vertex
    - [exit_points]: each call to a non-returning function (declared with the C
      attribute "noreturn") leads to a vertex from this list, with no successor
    - [stmt_table]: this table links statements to their starting and ending
      vertex *)
type automaton = {
  graph : graph;
  entry_point : vertex;
  return_point : vertex;
  exit_points : vertex list;
  stmt_table : (vertex * vertex) Cil_datatype.Stmt.Hashtbl.t;
}

(** Datatype for automata *)
module Automaton : Datatype.S with type t = automaton

(** Datatype for WTOs *)
module WTO : Wto.S with type node = vertex

(** Build an interpreted automaton for the given kernel_function.
    If [annotations] is true, the automaton includes [Prop] transitions for
    assertions and loop invariants of the function body.
    Note that the automata construction may lead to the build of new Cil
    expressions which will be different at each call: you may need to
    memoize the results of this function. *)
val build_automaton : annotations:bool -> Cil_types.kernel_function -> automaton

(** Build a wto for the given automaton. The [pref] function is a comparison
    function used to determine what is the best vertex to use as a Wto component
    head. See [Wto.Make] for more details. *)
val build_wto : ?pref:WTO.pref -> automaton -> wto

(** Get the automaton for the given kernel_function. This is the memoized
    version of [build_automaton ~annotations:false]  *)
val get_automaton : Cil_types.kernel_function -> automaton

(** Extract an exit strategy from a component, i.e. a sub-wto where all
    vertices lead outside the wto without passing through the head. *)
val exit_strategy : automaton -> vertex Wto.component -> wto

(** Output the automaton in dot format. *)
val output_to_dot :
  ?pp_vertex:(vertex Pretty_utils.formatter) ->
  ?pp_edge:(vertex edge Pretty_utils.formatter) ->
  ?wto:wto ->
  out_channel -> automaton -> unit


(** the position of a statement in a wto given as the list of
    component heads *)
type wto_index = vertex list

module WTOIndex : sig

  (** @return the components left and the components entered when going from
      one index to another *)
  val diff : wto_index -> wto_index -> vertex list * vertex list

  module Table : sig
    type t = wto_index Vertex.Hashtbl.t

    (** Compute the index table from a wto *)
    val build : wto -> t

    (** @return the wto_index for a statement *)
    val find : t -> vertex -> wto_index

    (** @return whether [v] is a component head or not *)
    val is_head : t -> vertex -> bool

    (** @return whether [v1,v2] is a back edge of a loop, i.e. if the vertex v1
        is a wto head of any component where v2 is included. This assumes that
        (v1,v2) is actually an edge present in the control flow graph. *)
    val is_back_edge : t -> vertex * vertex -> bool
  end
end

(** Dataflow computation: simple data-flow analysis using interpreted automata.
    See tests/misc/interpreted_automata_dataflow.ml for a complete example
    using this dataflow computation. *)

type 'a widening =
  | Fixpoint       (** The analysis of the loop has reached a fixpoint. *)
  | Widening of 'a (** The analysis of the loop has not reached a fixpoint yet,
                       and must continue through a new iteration with the given
                       state, widened if termination requires it. *)

(** Input domain for a simple dataflow analysis. *)
module type Domain =
sig
  type t (** States propagated by the dataflow analysis. *)

  (** Merges two states coming from different paths. *)
  val join : t -> t -> t

  (** [widen v1 v2] is called on loop heads after each iteration of the
      analysis on the loop body: [v1] is the previous state before the
      iteration, and [v2] the new state after the iteration.
      The function must return [Fixpoint] if the analysis has reached a fixpoint
      for the loop: this is usually the case if [join v1 v2] is equal to [v1],
      as a new iteration would have the same entry state as the last one.
      Otherwise, it must return the new entry state for the next iteration,
      by over-approximating the join between [v1] and [v2] such that
      any sequence of successive widenings is ultimately stationary,
      i.e. […widen (widen (widen x0 x1) x2) x3…] eventually returns [Fixpoint].
      This ensures the analysis termination. *)
  val widen : t -> t -> t widening

  (** Transfer function for edges: [transfer (u,e,v) s] computes the state
      at vertex [v] after the transition [e.edge_transition] from the state [s]
      at vertex [u]. For backward analyses, edges are thus reversed, i.e.
      [(v,e,u)] is an edge of the graph.

      This function can return None if the end of the transition is not
      reachable from the given state. *)
  val transfer : vertex * vertex edge * vertex -> t -> t option
end

(** Simple dataflow analysis *)
module type DataflowAnalysis =
sig
  type state
  type result

  val fixpoint : ?wto:wto -> Cil_types.kernel_function ->  state -> result

  module Result :
  sig
    (** Extract the result at the entry point of the analysed function *)
    val at_entry : result -> state option

    (** Extract the result at the return point of the analysed function (just
        after the return transfer function) *)
    val at_return : result -> state option

    (** Extract the result obtained for the control point immediately before the
        given statement *)
    val before : result -> Cil_types.stmt -> state option

    (** Extract the result obtained for the control point immediately after the
        given statement *)
    val after : result -> Cil_types.stmt -> state option

    (** Iter on the results obtained at each vertex of the graph.
        Do nothing  when the vertex is not reachable (for instance if transfer
        returned None) *)
    val iter_vertex : (vertex -> state -> unit) -> result -> unit

    (** Iter on the results obtained before each statements of the function.
        Do nothing  when the vertex is not reachable (for instance if transfer
        returned None) *)
    val iter_stmt : (Cil_types.stmt -> state -> unit) -> result -> unit

    (** Same as [iter_stmt] but guarantee that the iteration will always
        be in the same increasing order of statements sid.

        @since 27.0-Cobalt *)
    val iter_stmt_asc : (Cil_types.stmt -> state -> unit) -> result -> unit

    (** Output result to the given channel. Must be supplied with a pretty
        printer for abstract values *)
    val to_dot_output : (Format.formatter -> state -> unit) ->
      result -> out_channel -> unit

    (** Output result to a file with the given path. Must be supplied with
        pretty printer for abstract values *)
    val to_dot_file : (Format.formatter -> state -> unit) ->
      result -> Filepath.t -> unit

    (** Extract the result as a table from control points to states *)
    val as_table : result -> state Vertex.Hashtbl.t
  end
end

(** Forward dataflow analysis. The domain must provide a forward [transfer]
    function that computes the state after a transition from the state before. *)
module ForwardAnalysis (D : Domain) : DataflowAnalysis
  with type state = D.t

(** Backward dataflow analysis. The domain must provide a backward [transfer]
    function that computes the state before a transition from the state after. *)
module BackwardAnalysis (D : Domain) : DataflowAnalysis
  with type state = D.t


(** Generic control flow graphs *)
module type Graph = sig
  include Graph.Sig.I

  module VTable : Hashtbl.S with type key = vertex

  type wto = vertex Wto.partition
  module WTO : Wto.S with type node = vertex

  val pretty : t Pretty_utils.formatter

  (** Build a wto for the given automaton. The [pref] function is a comparison
      function used to determine what is the best vertex to use as a Wto component
      head. See [Wto.Make] for more details. *)
  val build_wto : pref:WTO.pref -> t -> V.t -> wto

  (** Output the automaton in dot format *)
  val output_to_dot :
    ?pp_vertex:(V.t Pretty_utils.formatter) ->
    ?pp_edge:(E.label Pretty_utils.formatter) ->
    ?wto:wto ->
    out_channel -> t -> unit

  (** Extract an exit strategy from a component, i.e. a sub-wto where all
      vertices lead outside the wto without passing through the head. *)
  val exit_strategy : t -> V.t Wto.component -> wto

  (** Widening for abstract domains *)
  type 'a widening = Fixpoint | Widening of 'a

  (** Abstract domains *)
  module type Domain =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t widening
    val transfer : edge -> t -> t option
  end

  (** Forward dataflow analysis *)
  module ForwardAnalysis (D : Domain) :
  sig
    val compute : t -> wto -> D.t -> D.t VTable.t
  end

  (** Forward dataflow analysis *)
  module BackwardAnalysis (D : Domain) :
  sig
    val compute : t -> wto -> D.t -> D.t VTable.t
  end
end

(** This functor can be used to build generic control flow graphs *)
module MakeGraph (Vertex : Vertex) (Edge : Edge) : Graph
  with type V.t = Vertex.t
   and type E.t = Vertex.t * Edge.t * Vertex.t
   and type V.label = Vertex.t
   and type E.label = Edge.t
   and module VTable = Vertex.Hashtbl


(** Control flow graphs where unnatural loops are modified such that all paths
    entering a loop enters it by its head. *)
module UnrollUnnatural : sig
  module Vertex_Set:
    Datatype.S_with_collections with type t = Vertex.Set.t
  module Version:
    Datatype.S_with_collections with type t = Vertex.t * Vertex.Set.t

  include Graph with type V.t = Version.t
                 and type E.t = Version.t * Version.t edge * Version.t
                 and type V.label = Version.t
                 and type E.label = Version.t edge

  val build : automaton -> G.vertex Wto.partition -> WTOIndex.Table.t -> t
end

