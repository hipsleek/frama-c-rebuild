(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A state selection is a set of states with operations for easy handling of
    state dependencies.
    @since Carbon-20101201
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

(* ************************************************************************** *)
(** {2 Type declarations} *)
(* ************************************************************************** *)

type t
(** Type of a state selection.
    @since Carbon-20101201
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
*)

val ty: t Type.t
(** Type value representing {!t}.
    @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {2 Generic Builders} *)
(* ************************************************************************** *)

val empty: t
(** The empty selection.
    @since Carbon-20101201 *)

val full: t
(** The selection containing all the states.
    @since Carbon-20101201 *)

val singleton: State.t -> t
(** The selection containing only the given state.
    @since Carbon-20101201 *)

val of_list: State.t list -> t
(** The selection containing only the given list of states.
    @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {2 Generic Getters} *)
(* ************************************************************************** *)

val is_empty: t -> bool
(** @return [true] iff the selection is empty.
    @since Carbon-20101201 *)

val is_full: t -> bool
(** @return [true] iff the selection contains all the states.
    @since Carbon-20101201 *)

val mem: t -> State.t -> bool

(* ************************************************************************** *)
(** {2 Specific selections} *)
(* ************************************************************************** *)


(* ************************************************************************ *)
(** {3 Builders from dependencies} *)
(* ************************************************************************ *)

val with_dependencies: State.t -> t
(** The selection containing the given state and all its dependencies.
    @since Carbon-20101201
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val only_dependencies: State.t -> t
(** The selection containing all the dependencies of the given state (but not
    this state itself).
    @since Carbon-20101201
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val with_codependencies: State.t -> t
(** The selection containing the given state and all its co-dependencies.
    @since Carbon-20101201 *)

val only_codependencies: State.t -> t
(** The selection containing all the co-dependencies of the given state (but
    not this state itself).
    @since Carbon-20101201 *)

(* ************************************************************************ *)
(** {3 Builders by operations over sets} *)
(* ************************************************************************ *)

val union: t -> t -> t
(** Union of two selections.
    @since Carbon-20101201 *)

val list_union: t list -> t
(** Union of an arbitrary number of selection (0 gives an empty selection)
    @since Oxygen-20120901
*)

val diff: t -> t -> t
(** Difference between two selections.
    @since Carbon-20101201 *)

(* ************************************************************************ *)
(** {3 Specific Getters} *)
(* ************************************************************************ *)

val cardinal: t -> int
(** Size of a selection.
    @since Carbon-20101201 *)

val to_list: t -> State.t list
(** Convert a selection into a list of states.
    @since Fluorine-20130401 *)

val pretty: Format.formatter -> t -> unit
(** Display a selection.
    @since Carbon-20101201 *)

val pretty_witness: Format.formatter -> t -> unit
(** Display a selection in a more concise form. (Using the atomic operations
    that were used to create it.)
    @since Aluminium-20160501 *)


(** {3 Iterators} *)

val iter_succ: (State.t -> unit) -> t -> State.t -> unit
(** Iterate over the successor of a state in a selection.
    The order is unspecified.
    @since Carbon-20101201 *)

val fold_succ: (State.t -> 'a -> 'a) -> t -> State.t -> 'a -> 'a
(** Iterate over the successor of a state in a selection.
    The order is unspecified.
    @since Carbon-20101201 *)

val iter: (State.t -> unit) -> t -> unit
(** Iterate over a selection. The order is unspecified.
    @since Carbon-20101201 *)

val fold: (State.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over a selection. The order is unspecified.
    @since Carbon-20101201 *)

val iter_in_order: (State.t -> unit) -> t -> unit
(** Iterate over a selection in a topological ordering compliant with the
    State Dependency Graph. Less efficient that {!iter}.
    @since Carbon-20101201 *)

val fold_in_order: (State.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over a selection in a topological ordering compliant with the
    State Dependency Graph. Less efficient that {!iter}.
    @since Carbon-20101201 *)
