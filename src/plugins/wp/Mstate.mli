(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang.F
open Memory
open Sigma

(* -------------------------------------------------------------------------- *)
(* --- L-Val Utility                                                      --- *)
(* -------------------------------------------------------------------------- *)

val index : s_lval -> term -> s_lval
val field : s_lval -> Cil_types.fieldinfo -> s_lval
val equal : s_lval -> s_lval -> bool

(* -------------------------------------------------------------------------- *)
(* --- Memory State Pretty Printing Information                           --- *)
(* -------------------------------------------------------------------------- *)

type state

val create : (module Memory.Model) -> sigma -> state

val lookup : state -> term -> mval
val apply : (term -> term) -> state -> state
val iter : (mval -> term -> unit) -> state -> unit
val updates : state sequence -> Vars.t -> update Bag.t
