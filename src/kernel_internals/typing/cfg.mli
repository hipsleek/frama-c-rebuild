(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

(** Code to compute the control-flow graph of a function or file.
    This will fill in the [preds] and [succs] fields of {!Cil_types.stmt}

    This is nearly always automatically done by the kernel. You only need
    those functions if you build {!type:Cil_types.fundec} yourself. *)

open Cil_types

(** Compute the CFG for an entire file,
    by calling {!cfgFun} on each function. *)
val computeFileCFG: file -> unit

(** clear the sid (except when clear_id is explicitly set to false),
    succs, and preds fields of each statement. *)
val clearFileCFG: ?clear_id:bool -> file -> unit

(** Compute a control flow graph for fd.  Stmts in fd have preds and succs
    filled in *)
val cfgFun : fundec -> unit

(** clear the sid, succs, and preds fields of each statement in a function *)
val clearCFGinfo: ?clear_id:bool -> fundec -> unit


(* [VP] This function was initially in Cil, but now depends on stuff in
   Logic_utils. Put there to avoid circular dependencies. *)

(** This function converts all [Break], [Switch],
    [Default] and [Continue] {!Cil_types.stmtkind}s and {!Cil_types.label}s
    into [If]s and [Goto]s, giving the function body a very CFG-like character.
    This function modifies its argument in place. *)
val prepareCFG: ?keepSwitch:bool -> fundec -> unit


(**/**)
val clear_sid_info_ref: (unit -> unit) ref
