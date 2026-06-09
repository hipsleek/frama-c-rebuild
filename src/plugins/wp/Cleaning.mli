(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Variables Cleaning                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Lang.F

type usage

val create : unit -> usage
val as_term : usage -> term -> unit
val as_atom : usage -> pred -> unit
val as_type : usage -> pred -> unit
val as_have : usage -> pred -> unit
val as_init : usage -> pred -> unit

val filter_type : usage -> pred -> pred
val filter_pred : usage -> pred -> pred
