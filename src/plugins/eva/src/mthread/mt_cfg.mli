(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Mt_cfg_types
open Mt_thread


val make_cfg: thread_state -> cfg

(** Remove nodes without multi-thread contents in the automata given by
    the start node, and returns the new start node. Nodes that are concurrent
    according to keep and {!Mt_cfg_types.CfgNode.must_be_in_cfg}. *)
val remove_superfluous_nodes : keep:var_access_kind -> cfg -> cfg


val dot_fprint_graph:
  Format.formatter -> cfg -> (Cil_types.stmt -> string) -> unit


(** {1 Memory accesses in a cfg} *)

val cfg_accesses: thread -> cfg -> AccessesByZoneNode.map



(** {1 Dataflow on a cfg} *)




val update_cfg_contexts: analysis_state -> thread_state -> unit
