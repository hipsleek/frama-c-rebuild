(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

(** Registers a builtin to be available in the environment of all projects.
    @since 30.0-Zinc
*)
val register: Cil_types.builtin_logic_info -> unit

(** Adds a logic builtin in the environment of the current project only. *)
val add: Cil_types.builtin_logic_info -> unit

(** Internal usage only: initializes kernel logic builtins. *)
val init: unit -> unit -> unit
