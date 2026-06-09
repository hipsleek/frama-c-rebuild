(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val has_requires: spec -> bool
val mark_invalid_initializers: unit -> unit
val mark_unreachable: unit -> unit
val mark_green_and_red: unit -> unit
val c_labels: kernel_function -> Callstack.t -> Eval_terms.labels_states
