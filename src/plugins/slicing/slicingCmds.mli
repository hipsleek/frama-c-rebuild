(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

open Pdg_types

(* TODO: This .mli exists mainly to avoid problems with 'make -j'. This API
   is too vast and must be simplified. For example, functions should not
   receive variables as names (ie. strings) but directly as zones, possibly
   with a hint to the function that does to conversion. Also, most functions
   are slightly modified in Register. This module and
   Register should be fused. *)

type set = SlicingTypes.Fct_user_crit.t Cil_datatype.Varinfo.Map.t

val get_select_kf : SlicingTypes.sl_select -> kernel_function

val topologic_propagation : unit -> unit

val select_pdg_nodes :
  set ->
  SlicingTypes.sl_mark ->
  PdgTypes.Node.t list ->
  kernel_function ->
  set

val select_stmt :
  set ->
  spare:bool ->
  stmt ->
  kernel_function ->
  set

val select_func_calls_to :
  set ->
  spare:bool ->
  Kernel_function.t ->
  set

val select_func_calls_into :
  set ->
  spare:bool ->
  Kernel_function.t ->
  set

val select_func_zone :
  set ->
  SlicingTypes.sl_mark ->
  Memory_zone.t ->
  kernel_function ->
  set

val select_func_return :
  set ->
  spare:bool ->
  Kernel_function.t ->
  set

val select_stmt_ctrl :
  set ->
  spare:bool ->
  stmt ->
  kernel_function ->
  set

val select_stmt_zone :
  set ->
  SlicingTypes.sl_mark ->
  Memory_zone.t ->
  before:bool ->
  stmt ->
  kernel_function ->
  set

val select_stmt_lval :
  set ->
  SlicingTypes.sl_mark ->
  Datatype.String.Set.t ->
  before:bool ->
  stmt ->
  eval:stmt ->
  kernel_function ->
  set

val select_stmt_lval_rw :
  set ->
  SlicingTypes.sl_mark ->
  rd:Datatype.String.Set.t ->
  wr:Datatype.String.Set.t ->
  stmt ->
  eval:stmt ->
  Kernel_function.t ->
  set

val select_stmt_pred :
  set ->
  SlicingTypes.sl_mark ->
  predicate ->
  stmt ->
  kernel_function ->
  set

val select_stmt_term :
  set ->
  SlicingTypes.sl_mark ->
  term ->
  stmt ->
  kernel_function ->
  set

val select_stmt_annot :
  set ->
  SlicingTypes.sl_mark ->
  spare:bool ->
  code_annotation ->
  stmt ->
  kernel_function ->
  set

val select_stmt_annots :
  set ->
  SlicingTypes.sl_mark ->
  spare:bool ->
  threat:bool ->
  user_assert:bool ->
  slicing_annot:bool ->
  loop_inv:bool ->
  loop_var:bool ->
  stmt ->
  kernel_function ->
  set

val select_func_annots :
  set ->
  SlicingTypes.sl_mark ->
  spare:bool ->
  threat:bool ->
  user_assert:bool ->
  slicing_annot:bool ->
  loop_inv:bool ->
  loop_var:bool ->
  kernel_function ->
  set

val select_func_lval :
  set ->
  SlicingTypes.sl_mark ->
  Datatype.String.Set.t ->
  Kernel_function.t ->
  set

val select_func_lval_rw :
  set ->
  SlicingTypes.sl_mark ->
  rd:Datatype.String.Set.t ->
  wr:Datatype.String.Set.t ->
  eval:stmt -> Kernel_function.t -> set

val add_selection : set -> unit
val add_persistent_selection :
  set -> unit
val add_persistent_cmdline : unit -> unit

val apply_all : propagate_to_callers:bool -> unit
val apply_all_actions : unit -> unit
val apply_next_action : unit -> unit
