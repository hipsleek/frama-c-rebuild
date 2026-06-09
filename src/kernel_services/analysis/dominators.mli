(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Module to perform dominators and postdominators analyses. This module was
    completely redesigned and provides many new functions.

    A dominator [d] of [s] is a statement such that all paths from the entry
    point of a function to [s] must go through [d].

    A postdominator [p] of [s] is a statement such that all paths from [s] to
    the return point of the function must go through [p].

    By definition, a statement always (post)dominates itself (except if it is
    unreachable).

    An immediate (post)dominator (or i(post)dom) [d] is the unique
    (post)dominator of [s] that strictly (post)dominates [s] but is
    (post)dominated by all other (post)dominators of [s].

    A common ancestor (or child) of a list of statements is a (post)dominator
    that (post)dominates all the statements.

    @since 30.0-Zinc
*)

open Cil_types

val compute_dominators : kernel_function -> unit
(** Compute the Dominators analysis and register its result. *)

val compute_postdominators : kernel_function -> unit
(** Compute the Postdominators analysis and register its result. *)

val get_dominators : stmt -> Cil_datatype.Stmt.Hptset.t
(** Return the [set] of dominators of the given statement.
    The empty set means the statement is unreachable. *)

val get_postdominators : stmt -> Cil_datatype.Stmt.Hptset.t
(** Return the [set] of postdominators of the given statement.
    The empty set means the statement is unreachable. *)

val get_strict_dominators : stmt -> Cil_datatype.Stmt.Hptset.t
(** Same as [get_dominators] but exclude the statement itself. The empty set
    means the statement is unreachable or is only dominated by itself. *)

val get_strict_postdominators : stmt -> Cil_datatype.Stmt.Hptset.t
(** Same as [get_postdominators] but exclude the statement itself. The empty set
    means the statement is unreachable or is only post-dominated by itself. *)

val dominates : stmt -> stmt -> bool
(** [dominates a b] return [true] if [a] dominates [b]. *)

val postdominates : stmt -> stmt -> bool
(** [postdominates a b] return [true] if [a] postdominates [b]. *)

val strictly_dominates : stmt -> stmt -> bool
(** [strictly_dominates a b] return [true] if [a] strictly dominates [b]. *)

val strictly_postdominates : stmt -> stmt -> bool
(** [strictly_postdominates a b] return [true] if [a] strictly postdominates
    [b]. *)

val get_idom : stmt -> stmt option
(** Return the immediate dominator of the given statement. *)

val get_ipostdom : stmt -> stmt option
(** Return the immediate postdominator of the given statement. *)

val nearest_common_ancestor : stmt list -> stmt option
(** Return the closest common ancestor of the given statement list.
    @before 30.0-Zinc previous implementation always returned a statement and
    raised a failed assertion in case of unreachable statement.
*)

val nearest_common_child : stmt list -> stmt option
(** Return the closest common child of the given statement list. *)

val pretty_dominators : Format.formatter -> unit -> unit
(** Print the result of the domination analysis. Each statement is either
    dominated by a set of statements, or [Top] if unreachable. *)

val pretty_postdominators : Format.formatter -> unit -> unit
(** Print the result of the postdomination analysis. Each statement is either
    postdominated by a set of statements, or [Top] if unreachable. *)

val print_dot_dominators : string -> kernel_function -> unit
(** Print the domination graph in a file [basename.function_name.dot]. *)

val print_dot_postdominators : string -> kernel_function -> unit
(** Print the postdomination graph in a file [basename.function_name.dot]. *)
