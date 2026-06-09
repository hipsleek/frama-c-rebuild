(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Functions related to type conversions *)

val sizeof_lval_typ: typ -> Z_or_top.t
(** Size of the type of a lval, taking into account that the lval might have
    been a bitfield. *)


(** [offsetmap_matches_type t o] returns true if either:
    - [o] contains a single scalar binding, of the expected scalar type [t]
      (float or integer)
    - [o] contains multiple bindings, pointers, etc.
    - [t] is not a scalar type. *)
val offsetmap_matches_type: typ -> Cvalue.V_Offsetmap.t -> bool

val need_cast: typ -> typ -> bool
(** return [true] if the two types are statically distinct, and a cast
    from one to the other may have an effect on an abstract value. *)

(* [compatible_functions typ kfs] filters the list [kfs] to only keep functions
   compatible with the type [typ]. The returned boolean is true if some
   functions were incompatible. If a list of arguments [args] is provided, also
   removes functions incompatible with them. Used to verify a call through a
   function pointer is ok.
   In theory, we could only check that both types are compatible as defined by
   C99, 6.2.7. However, some industrial codes do not strictly follow the norm,
   and we must be more lenient. Thus, some functions are also kept when Eva can
   ignore more or less safely the incompatibility in the types (which is however
   reported in the returned boolean). *)
val compatible_functions:
  typ -> ?args:typ list -> Kernel_function.t list ->
  Kernel_function.t list * bool

(** Abstraction of an integer type, more convenient than an [ikind] because
    it can also be used for bitfields. *)
type integer_range = { i_bits: int; i_signed: bool }

module DatatypeIntegerRange: Datatype.S with type t = integer_range

val ik_range: ikind -> integer_range
val ik_attrs_range: ikind -> attributes -> integer_range
(** Range for an integer type with some attributes. The attribute
    {!Ast_attributes.bitfield_attribute_name} influences the width of the
    type. *)

val pointer_range: unit -> integer_range
(** Range for a pointer type. *)

val range_inclusion: integer_range -> integer_range -> bool
(** Checks inclusion of two integer ranges. *)

val range_lower_bound: integer_range -> Z.t
val range_upper_bound: integer_range -> Z.t

(** Abstraction of scalar types -- in particular, all those that can be involved
    in a cast. Enum and integers are coalesced. *)
type scalar_typ =
  | TSInt of integer_range
  | TSPtr of integer_range
  | TSFloat of fkind

(* Classifies a cil type as a scalar type; returns None for non-scalar types. *)
val classify_as_scalar: typ -> scalar_typ option

(* Returns the range of a cil integer type; returns None for non-integer types.
   Pointers are considered as integer types if [ptr] is true. *)
val integer_range: ptr:bool -> typ -> integer_range option
