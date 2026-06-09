(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Module Abstract_state *)

(** see API.Abstract_state for documentation *)

open Cil_types

module EdgeLabel : sig
  type t = Pointer | Field of fieldinfo

  val compare : t -> t -> int
  val default : t
  val is_pointer : t -> bool
  val is_field : t -> bool
  val pretty : Format.formatter -> t -> unit
end

module G: Graph.Sig.G with type V.t = int and type E.label = EdgeLabel.t

module LSet = Cil_datatype.LvalStructEq.Set
module VarSet = Cil_datatype.Varinfo.Set

type t
type v = G.V.t

val vid : v -> int
val get_graph: t -> G.t
val get_vars : v -> t -> VarSet.t
val get_lval_set : v -> t -> LSet.t
val pretty : ?debug:bool -> Format.formatter -> t -> unit
val print_dot : string -> t -> unit
val find_vertex : lval -> t -> v
val find_vars : lval -> t -> VarSet.t
val find_synonyms : lval -> t -> LSet.t

val alias_vars : lval -> t -> VarSet.t
val alias_lvals : lval -> t -> LSet.t

val points_to_vars : lval -> t -> VarSet.t
val points_to_lvals : lval -> t -> LSet.t
val alias_sets_vars : t -> VarSet.t list
val alias_sets_lvals : t -> LSet.t list

val find_transitive_closure : lval -> t -> (v * LSet.t) list
val is_included : t -> t -> bool

(** Functions for Steensgaard's algorithm, see the paper *)
val join : t -> v -> v -> t

(** transfer functions for different kinds of assignments *)
val assignment : t -> lval -> exp option -> t

(** transfer function for malloc calls *)
val assignment_x_allocate_y : t -> lval -> t

(** union of two abstract values ; ensures that if 2 lval are
    aliased in one of the two input graph (or in a points-to
    relationship), then they will also be aliased/points-to in the
    result *)
val union : t -> t -> t

(** empty graph *)
val empty : t

(** Type denoting summaries of functions *)
type summary

(** creates a summary from a state and a function *)
val make_summary : t -> kernel_function -> summary

(** pretty printer *)
val pretty_summary :  ?debug:bool -> Format.formatter -> summary -> unit

(** [call a res args s] computes the abstract state after the
    instruction res=f(args), with f summarized by [s]. [a] is the abstract state before the call *)
val call: t -> lval option -> exp list -> summary -> t

val node_counter : int ref
(** for debug purposes only *)
