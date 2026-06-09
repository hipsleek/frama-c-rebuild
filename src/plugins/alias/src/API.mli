(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** External API of the plugin Alias *)

open Cil_types

module LSet = Cil_datatype.LvalStructEq.Set
module VarSet = Cil_datatype.Varinfo.Set

module EdgeLabel : sig
  type t = Pointer | Field of fieldinfo

  val compare : t -> t -> int
  val default : t
  val is_pointer : t -> bool
  val is_field : t -> bool
  val pretty : Format.formatter -> t -> unit
end

module G : Graph.Sig.G with type V.t = int and type E.label = EdgeLabel.t

type v = G.V.t

val vid : v -> int

(** analyses at the granularity of a statement, i.e. the results are w.r.t. to
    the state just before the given statement [stmt] *)
module Statement : sig
  (** see [Abstract_state.points_to_vars] *)
  val points_to_vars : stmt:stmt -> lval -> VarSet.t

  (** see [Abstract_state.points_to_lvals] *)
  val points_to_lvals : stmt:stmt -> lval -> LSet.t

  (** see [Abstract_state.alias_sets_vars] *)
  val alias_sets_vars : stmt:stmt -> VarSet.t list

  (** see [Abstract_state.alias_sets_lvals] *)
  val alias_sets_lvals : stmt:stmt -> LSet.t list

  (** see [Abstract_state.alias_vars] *)
  val alias_vars : stmt:stmt -> lval -> VarSet.t

  (** see [Abstract_state.alias_lvals] *)
  val alias_lvals : stmt:stmt -> lval -> LSet.t

  (** aliases of the given lval [lv] created by stmt [s] *)
  val new_aliases_lvals : stmt:stmt -> lval -> LSet.t

  (** aliases of the given lval [lv] created by stmt [s] *)
  val new_aliases_vars : stmt:stmt -> lval -> VarSet.t

  val are_aliased: stmt:stmt -> lval -> lval -> bool
end

(** analyses at the level of a [kernel_function], i.e. the results are w.r.t.
    to the end of the given function *)
module Function : sig
  (** see [Abstract_state.points_to_vars] *)
  val points_to_vars : kf:kernel_function -> lval -> VarSet.t

  (** see [Abstract_state.points_to_lvals] *)
  val points_to_lvals : kf:kernel_function -> lval -> LSet.t

  (** see [Abstract_state.alias_sets_vars] *)
  val alias_sets_vars : kf:kernel_function -> VarSet.t list

  (** see [Abstract_state.alias_sets_lvals] *)
  val alias_sets_lvals : kf:kernel_function -> LSet.t list

  (** see [Abstract_state.alias_vars] *)
  val alias_vars : kf:kernel_function -> lval -> VarSet.t

  (** see [Abstract_state.alias_lvals] *)
  val alias_lvals : kf:kernel_function -> lval -> LSet.t

  val are_aliased: kf:kernel_function -> lval -> lval -> bool

  (** list of pairs [s, e] where [e] is the set of lvals aliased to [v] after
      each statement [s] in function [kf]. *)
  val fundec_stmts : kf:kernel_function -> lval -> (stmt * LSet.t) list
end


(** [fold_fundec_stmts f acc kf v] folds [f acc s e] on the list of
    pairs [s, e] where [e] is the set of lval aliased to [v] after statement [s]
    in function [kf]. *)
val fold_fundec_stmts:
  ('a -> stmt -> lval -> 'a) -> 'a -> kernel_function -> lval -> 'a

(** [fold_vertex f acc kf s v] folds [f acc i lv] to all [lv] in [i], where [i] is
    the vertex that represents the equivalence class of [v] before statement [s] in function [kf]. *)
val fold_vertex:
  ('a -> v -> lval -> 'a) -> 'a  -> kernel_function -> stmt -> lval  -> 'a

(** [fold_vertex_closure f acc kf s v] is the transitive closure of function
    [fold_vertex]. *)
val fold_vertex_closure:
  ('a -> v -> lval -> 'a) -> 'a  -> kernel_function -> stmt -> lval  -> 'a


(** direct access to the abstract state. See Abstract_state.mli *)

module Abstract_state : sig
  (** Type denothing an abstract state of the analysis. It is a graph containing
      all aliases and points-to information. *)
  type t

  (** access to the points-to graph *)
  val get_graph: t -> G.t

  (** set of variables associated with given vertex *)
  val get_vars : v -> t -> VarSet.t

  (** set of lvals which can be used to refer to the given vertex
      Example graph: <a> → <b> -t→ <c>
      The lvals corresponding to the rightmost vertex are <c, b.t, a->t>:
      - c:    simply refers to a variable associated with the vertex.
      - b.t:  starting from the second vertex one can follow a field-edge
              labelled [t] to come upon the rightmost vertex.
      - a->t: Following a pointer edge from the leftmost vertex one obtains
              [*a]. Following the [t] field-edge one arrives at the rightmost
              vertex, corresponding to [( *a ).t] or [a->t]. *)
  val get_lval_set : v -> t -> LSet.t

  (** pretty printer; debug=true prints the graph, debug = false only
      prints aliased variables *)
  val pretty : ?debug:bool -> Format.formatter -> t -> unit

  (** dot printer; first argument is a file name *)
  val print_dot : string -> t -> unit

  (** finds the vertex corresponding to a lval.
      @raise Not_found if such a vertex does not exist
  *)
  val find_vertex : lval -> t -> v

  (* Combining [find_vertex] and [get_vars] finds the varset associated with
     the vertex of the given lval.
     Does not raise an exception but returns an empty set for inexisting lval. *)
  val find_vars : lval -> t -> VarSet.t

  (** Note: You probably want to use [alias_lvals] instead of this function.
      Combining [find_vertex] with [get_lval_set], this function yields all the
      different ways the vertex containing the given lval can be referred to.
      Example: <a> → <b,c>
      If [a] points to [b], then the vertex containing [b] can be referred to not
      only by [b] but also by [c] or [*a].
      Does not raise an exception but returns an empty set if the lval is not in
      the graph. *)
  val find_synonyms : lval -> t -> LSet.t

  (** all the variables that are aliases of the given lval:
      - variables sharing an equivalence class (or: vertex) with [lv]
      - variables from a neighbouring vertex, i.e. a vertex that shares a
        successor with the vertex of [lv].

      Example: <a,b> → <c> ← <d> ← <e>
      The aliases of [a] are <a,b,d>:
      - [b] shares a vertex with [a]
      - [d] is in a neighbouring vertex, pointing to [c] as does <a,b> *)
  val alias_vars : lval -> t -> VarSet.t

  (** Yields all lvals that are an alias of a given lval [lv]. This includes:
      - variables sharing an equivalence class (or: vertex) with [lv]
      - variables from a neighbouring vertex, i.e. a vertex that shares a
        successor with the vertex of [lv].
      - lvals reconstructed from the variables from the two previous sets.

      Example: <a,b> → <c> ← <d> ← <e>
      The aliases of [a] are <a,b,d,*e>:
      - [b] shares a vertex with [a]
      - [d] is in a neighbouring vertex, as it shares a successor with <a,b>
      - [*e] is obtained by following backwards the pointer edge from <d> to <e>. *)
  val alias_lvals : lval -> t -> LSet.t

  (** the set of all variables to which the given variable may point. *)
  val points_to_vars : lval -> t -> VarSet.t

  (** the set of all lvals to which the given variable may point including
      reconstructed lvals such as *p, a.t, c->s.
      For some pointer p it will always include *p. *)
  val points_to_lvals : lval -> t -> LSet.t

  (** all the alias sets of a given state
      Example: <a,b> → <c> ← <d> ← <e,f>
      The aliases sets are <<a,b,d>, <e,f>>
  *)
  val alias_sets_vars : t -> VarSet.t list

  (** all the alias sets of a given state, including reconstructed lvals
      Example: <a,b> → <c> ← <d> ← <e,f>
      The aliases sets are <<a,b,d,*e,*f>, <e,f>>
  *)
  val alias_sets_lvals : t -> LSet.t list

  (** alias_lvals, then recursively finds other sets of lvals. We
      have the property (if lval [lv] is in abstract state [x]) :
      List.hd (find_transitive_closure lv x) = (find_vertex lv x,
      find_aliases lv x).
      Only follows pointer edges, not field edges.
  *)
  val find_transitive_closure : lval -> t -> (v * LSet.t) list

  (** inclusion test; [is_included a1 a2] tests if, for any lvl
      present in a1 (associated to a vertex v1), that it is also
      present in a2 (associated to a vertex v2) and that
      get_lval_set(succ(v1) is included in get_lval_set(succ(v2)) *)
  val is_included : t -> t -> bool
end

(** [get_state_before_stmt f s] gets the abstract state computed after
    statement [s] in function [f]. Returns [None] if
    the abstract state is bottom or not computed. *)
val get_state_before_stmt :  kernel_function -> stmt -> Abstract_state.t option


(** [call_function a f Some(res) args] computes the abstract state
    after the instruction res=f(args) where res is a lval. [a] is the
    abstract state before the call. If function [f] returns no value,
    use [call_function a f None args] instead. Returns [None] if
    the abstract state [a] is bottom or not computed. *)
val call_function: Abstract_state.t -> kernel_function -> lval option -> exp list -> Abstract_state.t option


(** [simplify_lval lv] returns a lval where every index of an array is
    replaced by 0, every pointer arithmetic is simplified to an access
    to an array *)
val simplify_lval: lval -> lval
