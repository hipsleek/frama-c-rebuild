(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Reachability for Smoke Tests *)
(* -------------------------------------------------------------------------- *)

open Cil_types

type reachability
(** control flow graph dedicated to smoke tests *)

val is_predicate : bool -> predicate -> bool
(** If returns [true] the predicate has always the given boolean value. *)

val is_dead_annot : code_annotation -> bool
(** False assertions and loop invariant.
    Hence, also includes completely unrolled loop. *)

val is_dead_code : stmt -> bool
(** Checks whether the stmt has a dead-code annotation. *)

val reachability : Kernel_function.t -> reachability
(** memoized reached cfg for function *)

val smoking : reachability -> Cil_types.stmt -> bool
(** Returns whether a stmt need a smoke tests to avoid being unreachable.
    This is restricted to assignments, returns and calls not dominated
    another smoking statement. *)

val dump : dir:Filepath.t -> Kernel_function.t -> reachability -> unit

val set_doomed : Emitter.t -> WpPropId.prop_id -> unit

val unreachable_proved : int ref
val unreachable_failed : int ref

val set_unreachable : WpPropId.prop_id -> unit

val emitter: Emitter.t

(* -------------------------------------------------------------------------- *)
