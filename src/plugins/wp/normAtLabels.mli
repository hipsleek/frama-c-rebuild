(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Clabels

(* exception LabelError of logic_label *)
val catch_label_error : exn -> string -> string -> unit

type label_mapping

val labels_empty : label_mapping
val labels_fct_pre : label_mapping
val labels_fct_post : exit:bool -> label_mapping
val labels_fct_assigns : exit:bool -> label_mapping
val labels_assert : kf:kernel_function -> stmt -> label_mapping
val labels_loop : stmt -> label_mapping
val labels_stmt_pre : kf:kernel_function -> stmt -> label_mapping
val labels_stmt_post : kf:kernel_function -> stmt -> label_mapping
val labels_stmt_assigns : kf:kernel_function -> stmt -> label_mapping
val labels_stmt_post_l : kf:kernel_function -> stmt -> c_label option -> label_mapping
val labels_stmt_assigns_l : kf:kernel_function -> stmt -> c_label option -> label_mapping
val labels_predicate : (logic_label * logic_label) list -> label_mapping
val labels_axiom : label_mapping

val preproc_term : label_mapping -> term -> term
val preproc_annot : label_mapping -> predicate -> predicate
val preproc_assigns : label_mapping -> from list -> from list
val has_postassigns : assigns -> bool
