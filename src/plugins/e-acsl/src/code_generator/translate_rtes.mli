(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Generate and translate RTE annotations. *)

val rte_annots:
  (Format.formatter -> 'a -> unit) ->
  'a ->
  kernel_function ->
  Env.t ->
  code_annotation list ->
  Env.t
(** Translate the given RTE annotations into runtime checks in the given
    environment. *)
