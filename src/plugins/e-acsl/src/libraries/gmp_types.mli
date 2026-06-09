(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** GMP Values. *)

open Cil_types

val init: unit -> unit
(** Must be called before any use of GMP *)

val is_t: typ -> bool
(** @return true iff the given type is equivalent to one of the GMP type. *)

(**************************************************************************)
(******************************** Types ***********************************)
(**************************************************************************)

(** Signature of a GMP type *)
module type S = sig

  val t: unit -> typ
  (** @return the GMP type *)

  val t_as_ptr: unit -> typ
  (** type equivalent to [t] but seen as a pointer *)

  val is_now_referenced: unit -> unit
  (** Call this function when using this type for the first time. *)

  val is_t: typ -> bool
  (** @return true iff the given type is equivalent to the GMP type. *)

end

(** Representation of the unbounded integer type at runtime *)
module Z: S

(** Representation of the rational type at runtime *)
module Q: S

val bitcnt_t: unit -> typ
(** @return the C Type representing the count of bits of a multi-precision
    number at runtime *)
