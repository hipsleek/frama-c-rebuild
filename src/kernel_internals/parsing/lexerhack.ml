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

(* We provide here a pointer to a function. It will be set by the lexer and
 * used by the parser. In Ocaml lexers depend on parsers, so we we have put
 * such functions in a separate module. *)
let add_identifier: (string -> unit) ref =
  ref (fun _ -> Kernel.fatal "Uninitialized add_identifier")

let add_type: (string -> unit) ref =
  ref (fun _ -> Kernel.fatal "Uninitialized add_type")

let push_context: (unit -> unit) ref =
  ref (fun _ -> Kernel.fatal "Uninitialized push_context")

let pop_context: (unit -> unit) ref =
  ref (fun _ -> Kernel.fatal "You called an uninitialized pop_context")

let is_typedef: (unit -> bool) ref = Extlib.mk_fun "is_typedef"

let reset_typedef: (unit -> unit) ref = Extlib.mk_fun "reset_typedef"

let set_typedef: (unit -> unit) ref = Extlib.mk_fun "set_typedef"
