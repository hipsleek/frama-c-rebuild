(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* -------------------------------------------------------------------------- *)
(** {2 Logic Helpers} *)
(* -------------------------------------------------------------------------- *)

val addrof : ?loc:location -> lval -> term
val taddrof : ?loc:location -> term_lval -> term

val pnull :
  ?loc:location -> ?names:string list -> eq:bool ->
  term -> predicate

val pvalid :
  ?loc:location -> ?names:string list -> ?label:logic_label ->
  term -> predicate

val pvalid_read :
  ?loc:location -> ?names:string list -> ?label:logic_label ->
  term -> predicate

val pvalid_region :
  ?loc:location -> ?names:string list -> ?label:logic_label ->
  term -> predicate

(** [p] is [\null] or [\object_pointer(p)], or [\valid_function(p)]
    for function pointers *)
val pvalid_pointer :
  ?loc:location -> ?names:string list -> ?label:logic_label ->
  term -> predicate

val pinitialized :
  ?loc:location -> ?names:string list -> ?label:logic_label ->
  term -> predicate

val paligned :
  ?loc:location -> ?names:string list ->
  term -> typ -> predicate

val is_valid_region : logic_info -> bool

(* -------------------------------------------------------------------------- *)
(** {2 Kind of L-Values and Pointers} *)
(* -------------------------------------------------------------------------- *)

type lkind

val safe : lkind
val unsafe : lkind

val kind : exp -> lkind
val lkind : lval -> lkind
val hkind : lhost -> lkind
val term_kind : term -> lkind
val term_hkind : term_lhost -> lkind
val term_lkind : term_lval -> lkind
val safe_array_offset : typ -> offset -> bool
val safe_array_toffset : logic_type -> term_offset -> bool

(* -------------------------------------------------------------------------- *)
(** {2 Residual Conditions} *)
(* -------------------------------------------------------------------------- *)

(** The residual conditions are computed by assuming that all inner
    sub-expresisions or l-values are correct. *)

type residual = [ `Default | `True | `False ]

val rpath : lkind -> residual
val rvalid : ?writing:bool -> kinstr -> Memory.node -> lkind -> residual
val rinitialized : Memory.node -> lkind -> residual
val raligned : Memory.node -> bits:int -> ?default:bool -> lkind -> residual
val rallocated : kinstr -> varinfo -> residual

(* -------------------------------------------------------------------------- *)

val pp_kind : Format.formatter -> lkind -> unit
val pp_residual : Format.formatter -> residual -> unit

(* -------------------------------------------------------------------------- *)
