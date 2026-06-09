(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Memory Theory                                                      --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

(** {2 Theory} *)

val t_malloc : tau (** allocation tables *)

val t_init : tau (** initialization tables *)

val t_mem : tau -> tau (** t_addr indexed array *)

val f_eqmem : lfun
val f_memcpy : lfun

val sconst : term -> pred
val scinit : term -> pred
val framed : term -> pred

(* -------------------------------------------------------------------------- *)

(** {2 Unsupported Union Fields} *)

val unsupported_union : model:string -> Cil_types.fieldinfo -> unit

(* -------------------------------------------------------------------------- *)
