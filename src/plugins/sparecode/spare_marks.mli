(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Pdg_types

type proj
type fct

val select_useful_things :
  select_annot:bool -> select_slice_annot:bool -> kernel_function -> proj

val get_marks : proj -> kernel_function -> fct option

val key_visible : fct -> PdgIndex.Key.t -> bool

(** Useful mainly if there has been some Pdg.Top *)
val kf_visible : proj -> kernel_function -> bool
