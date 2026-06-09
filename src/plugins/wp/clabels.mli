(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Normalized C-labels                                                       *)
(* -------------------------------------------------------------------------- *)

(**
    Structural representation of logic labels.
    Compatible with stdlib comparison and structural equality.
*)

type c_label

val is_here : c_label -> bool
val mem : c_label -> c_label list -> bool
val equal : c_label -> c_label -> bool

module T : sig type t = c_label val compare : t -> t -> int end
module LabelMap : Stdlib.Map.S with type key = c_label
module LabelSet : Stdlib.Set.S with type elt = c_label

val pre : c_label
val here : c_label
val next : c_label
val init : c_label
val post : c_label
val exit : c_label
val break : c_label
val continue : c_label
val default : c_label
val loopentry : c_label
val loopcurrent : c_label

val formal : string -> c_label

val case : int64 -> c_label
val stmt : Cil_types.stmt -> c_label
val stmt_post : Cil_types.stmt -> c_label
val loop_entry : Cil_types.stmt -> c_label
val loop_current : Cil_types.stmt -> c_label

val to_logic : c_label -> Cil_types.logic_label
val of_logic : Cil_types.logic_label -> c_label
(** Assumes the logic label only comes from normalized or non-ambiguous
    labels. Ambiguous labels are: Old, LoopEntry and LoopCurrent, since
    they point to different program points depending on the context. *)

val is_post : Cil_types.logic_label -> bool
(** Checks whether the logic-label is [Post] or [to_logic post] *)

val pretty : Format.formatter -> c_label -> unit

open Cil_types

val name : logic_label -> string
val lookup : (logic_label * logic_label) list -> string -> logic_label
(** [lookup bindings lparam] retrieves the actual label
    for the label in [bindings] for label parameter [lparam]. *)
