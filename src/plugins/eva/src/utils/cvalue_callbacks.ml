(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type state = Cvalue.Model.t

module Reset = Hook.Build (struct type t = unit end)

let register_at_start_hook f =
  Reset.extend f

let apply_at_start_hooks () =
  Reset.apply ()

type analysis_kind = [ `Builtin | `Spec | `Body | `Reuse ]

type call_hook =
  Callstack.t -> Cil_types.kernel_function -> state -> analysis_kind -> unit

module Call =
  Hook.Build
    (struct type t = Callstack.t * kernel_function * state * analysis_kind end)

let register_call_hook f =
  Call.extend (fun (callstack, kf, kind, state) -> f callstack kf kind state)

let apply_call_hooks callstack kf state kind =
  Call.apply (callstack, kf, state, kind)

type call_assigns = (Assigns.t * Memory_zone.t) option

type state_by_stmt = (state Cil_datatype.Stmt.Hashtbl.t) Lazy.t
type results = { before_stmts: state_by_stmt; after_stmts: state_by_stmt }

type call_results =
  [ `Builtin of state list * call_assigns
  | `Spec of state list
  | `Body of results * int
  | `Reuse of int
  ]

type call_results_hook =
  Callstack.t -> Cil_types.kernel_function -> state -> call_results -> unit

module Call_Results =
  Hook.Build
    (struct type t = Callstack.t * kernel_function * state * call_results end)

let register_call_results_hook f =
  Call_Results.extend
    (fun (callstack, kf, state, results) -> f callstack kf state results)

let apply_call_results_hooks callstack kf state call_results =
  Call_Results.apply (callstack, kf, state, call_results)


module Statement =
  Hook.Build (struct type t = Callstack.t * stmt * state list end)

let register_statement_hook f =
  Statement.extend (fun (callstack, stmt, states) -> f callstack stmt states)

let apply_statement_hooks callstack stmt states =
  Statement.apply (callstack, stmt, states)
