(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* This module serves as a library for the modules Interval and Widening, the
   functions it exports should not be used outside of those modules. Instead,
   use the functions exported in the module Interval *)

open Analyses_types
open Cil_types

val is_included : ival -> ival -> bool
val join : ival -> ival -> ival
val meet : ival -> ival -> ival
val is_singleton_int : ival -> bool
val bottom : ival
val top_ival : ival
val singleton : Z.t -> ival
val singleton_of_int : int -> ival
val ival : Z.t -> Z.t -> ival
val interv_of_unknown_block : ival lazy_t

val unify : ival -> ival
(** @return the smallest interval containing a disjoint union of intervals *)

val lift_unop : (Ival.t -> Ival.t) -> ival -> ival
(** lift a unary operation on [IVal.t] to the type [ival] *)

val lift_arith_binop : (Ival.t -> Ival.t -> Ival.t) -> ival -> ival -> ival
(** Lift a binary operation on [IVal.t] to the type [ival] *)

(** @return [Some ival] if argument is [Ival ival] *)
val extract_ival : ival -> Ival.t option

val ival_of_ikind: ikind -> Ival.t

val interv_of_typ : typ -> ival
(** @return the smallest interval which contains the given C type. *)

val extended_interv_of_typ : typ -> ival
(** @return the interval [n..m+1] when interv_of_typ returns [n..m].
    It is in particular useful for computing bounds of quantified variables. *)

val interv_of_logic_typ : logic_type -> ival
(** @return the smallest interval which contains the given logic type. *)

exception Not_representable_ival
(** raised by {!ikind_of_ival}.
    @since 28.0-Nickel
*)

val ikind_of_ival : Ival.t -> ikind
(** @return the smallest ikind that contains the given interval.
    @raise Not_representable_ival if the given interval does not fit into any C
    integral type. *)
