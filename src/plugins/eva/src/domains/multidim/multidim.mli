(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** [offset] is an abstraction for array indexes when these
    arrays are used as a representation of multidimensional arrays or
    structures. They have the form :

    o + d₁×[0,b₁]  + ... + dₙ×[0,bₙ]

    or, more formally

    \{ o + Σ dᵢ×xᵢ | ∀i 1≤i≤n ⇒ xᵢ ∊ [0, bᵢ] \}

    This is a generalisation of integers intervals with modulo implemented in
    Ival : o + d×[0, b]

    The list of dᵢ is sorted in descending order and we may add the constraint

    dᵢ×bᵢ < dᵢ₋₁

    which is verified for normal multidimensional arrays handling.
*)
type index = Z.t * (Z.t * Z.t) list (* o, [dᵢ,bᵢ]ᵢ *)

include Datatype.S with type t = index

(* Constructors *)

val zero : t
val of_int : int -> t
val of_integer : Z.t -> t
val of_ival : Ival.t -> t (* Raises Abstract_interp.Error_Bottom and Error_Top *)

(* Properties *)

val is_zero : t -> bool
val is_singleton : t -> bool
val hull : t -> Z.t * Z.t (* start * size *)

(* Decomposition *)

val first_dimension : t -> (Z.t * t) option

(* Arithmetic *)

val add : t -> t -> t
(* slightly faster than add since no normalization takes place *)
val add_int : t -> int -> t
val add_integer : t -> Z.t -> t
val sub_int : t -> int -> t
val sub_integer : t -> Z.t -> t

val mul : t -> t -> t
val mul_int : t -> int -> t
val mul_integer : t -> Z.t -> t

val mod_int : t -> int -> t
val mod_integer : t -> Z.t -> t

(* Conversion from Cil *)

val of_exp : (Cil_types.exp -> t) -> Cil_types.exp -> t (* improves over an oracle *)
val of_offset : (Cil_types.exp -> t) -> Cil_types.typ -> Cil_types.offset -> t
