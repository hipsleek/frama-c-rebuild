(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Compute Kernel-Function & CFG Infos for WP *)

type t

module Cfg = Interpreted_automata

(** Memoized *)
val get : Kernel_function.t ->
  ?smoking:bool -> ?bhv:string list -> ?prop:string list ->
  unit -> t
val clear : unit -> unit

val body : t -> Cfg.automaton option
val annots : t -> bool
val doomed : t -> WpPropId.prop_id Bag.t
val calls : t -> Kernel_function.Set.t
val smoking : t -> Cil_types.stmt -> bool
val unreachable : t -> Cfg.vertex -> bool
val terminates_deps : t -> Property.Set.t

val is_entry_point : Kernel_function.t -> bool
(** @return true iff the given argument should always be considered as the main
            entry point, in particular: lib-entry is inactive.
    @since 28.0-Nickel
*)

val is_recursive : Kernel_function.t -> bool
val in_cluster : caller:Kernel_function.t -> Kernel_function.t -> bool

val trivial_terminates : int ref

(**************************************************************************)
