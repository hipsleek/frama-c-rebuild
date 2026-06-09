(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Syntax extension for widening hints, used by Value. *)

open Cil_types

val dkey: Self.category

type hint_vars =
  | HintAllVars (* "all" vars: static hint *)
  | HintVar of varinfo (* static hint *)
  | HintMem of exp * offset (* dynamic hint *)

val pp_hvars : Format.formatter -> hint_vars -> unit

(** Type of widening hints: a special kind of lval
    for which the hints will apply and a list of names (e.g. global). *)
type hint_lval = {
  vars : hint_vars;
  names : string list;
  loc : Fileloc.t;
}

type t = hint_lval * term list

(** [get_stmt_widen_hint_terms s] returns the list of widen hints associated to
    [s]. *)
val get_stmt_widen_hint_terms : stmt -> t list

(** [is_global wh] returns true iff widening hint [wh] has a "global" prefix. *)
val is_global : t -> bool

(** [is_dynamic wh] returns true iff widening hint [wh] has a "dynamic" prefix. *)
val is_dynamic : t -> bool
