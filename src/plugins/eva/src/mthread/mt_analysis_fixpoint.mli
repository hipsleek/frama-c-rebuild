(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val pre_thread_analysis :
  Mt_thread.analysis_state -> Mt_thread.thread_state -> unit
val post_thread_analysis : Mt_thread.analysis_state -> unit
val post_iteration : Mt_thread.analysis_state -> unit
val mark_shared_nodes_kind : Mt_thread.analysis_state -> unit
