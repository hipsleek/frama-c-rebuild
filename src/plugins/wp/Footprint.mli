(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Term Footprints *)
(* -------------------------------------------------------------------------- *)

open Lang.F

(** Width-first full iterator. *)
val iter : (term -> unit) -> term -> unit

(** Width-first once iterator. *)
val once : (term -> unit) -> term -> unit

(** Head only footprint *)
val head : term -> string

(** Generate head footprint up to size *)
val pattern : term -> string

(** Head match *)
val matches : string -> term -> bool

(** [k]-th occurrence of the footprint in a term *)
type occurrence = int * string

(** Locate the occurrence of [select] footprint inside a term. *)
val locate : select:term -> inside:term -> occurrence

(** Retrieve back the [k]-th occurrence of a footprint inside a term. *)
val lookup : occur:occurrence -> inside:term -> term

(* -------------------------------------------------------------------------- *)
