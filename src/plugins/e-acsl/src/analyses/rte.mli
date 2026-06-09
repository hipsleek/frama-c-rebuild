(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Accessing the RTE plug-in easily. *)

open Cil_types

val stmt: ?warn:bool -> kernel_function -> stmt -> code_annotation list
(** RTEs of a given stmt, as a list of code annotations. *)

val exp: ?warn:bool -> kernel_function -> stmt -> exp -> code_annotation list
(** RTEs of a given exp, as a list of code annotations. *)

val get_state_selection_with_dependencies: unit -> State_selection.t
(** Equivalent to [State_selection.with_dependencies RteGen.Generator.self]
    if the RTE plug-in is enabled, empty otherwise. *)
