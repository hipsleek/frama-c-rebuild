(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Some bit manipulations. *)

open Cil_types

val sizeofchar: unit -> Z.t
(** [sizeof(char)] in bits *)

val sizeofpointer: unit -> int
(** [sizeof(char* )] in bits *)

val sizeof: typ -> Z_or_top.t
(** [sizeof typ] is the size of [typ] in bits; it may return [`Top]. *)

val osizeof: typ -> Z_or_top.t
(** [osizeof typ] is the size of [typ] in bytes; it may return [`Top]. *)

exception Neither_Int_Nor_Enum_Nor_Pointer

val is_signed_int_enum_pointer: typ -> bool
(** [true] means that the type is signed.
    @raise Neither_Int_Nor_Enum_Nor_Pointer if the sign of the type is not
    meaningful. *)

val signof_typeof_lval: lval -> bool
(** @return the sign of type of the [lval]. [true] means that the type is
    signed. *)

val sizeof_vid: varinfo -> Z_or_top.t
(** @return the size of the type of the variable in bits. *)

val sizeof_lval: lval -> Z_or_top.t
(** @return the size of the type of the left value in bits. *)

val sizeof_pointed: typ -> Z_or_top.t
(** @return the size of the type pointed by a pointer or array type in bits.
    Never call it on a non pointer or non array type . *)

val osizeof_pointed: typ -> Z_or_top.t
(** @return the size of the type pointed by a pointer or array type in bytes.
    Never call it on a non pointer or array type. *)

val sizeof_pointed_lval: lval -> Z_or_top.t
(** @return the size of the type pointed by a pointer type of the [lval] in
    bits. Never call it on a non pointer type [lval]. *)

val max_bit_address : unit -> Z.t
(** @return the maximal possible offset in bits of a memory base. *)

val max_bit_size : unit -> Z.t
(** @return the maximal possible size in bits of a memory base. *)

val max_byte_address : unit -> Z.t
(** @return the maximal possible offset in bytes of a memory base.
    @since Aluminium-20160501 *)

val max_byte_size : unit -> Z.t
(** @return the maximal possible size in bytes of a memory base.
    @since Aluminium-20160501 *)

(** {2 Pretty printing} *)

val pretty_bits:
  typ ->
  use_align:bool ->
  align:Abstract_interp.Rel.t ->
  rh_size:Z.t ->
  start:Z.t ->
  stop:Z.t -> Format.formatter -> bool * typ option
(** Pretty prints a range of bits in a type for the user.
    Tries to find field names and array indexes, whenever possible. *)


(** {2 Mapping from numeric offsets to symbolic ones.} *)

(** Comparison of the shape of two types. Attributes are completely ignored. *)
val type_compatible: typ -> typ -> bool

(** We want to find a symbolic offset that corresponds to a numeric one, with
    one additional criterion: *)
type offset_match =
  | MatchType of typ (** Offset that has this type (modulo attributes) *)
  | MatchSize of Z.t (** Offset that has a type of this size *)
  | MatchFirst (** Return first symbolic offset that matches *)
  | MatchLast (** Return the longest offset that matches*)

exception NoMatchingOffset

(** [find_offset typ ~offset ~size] finds a subtype [t] of [typ] that describes
    the type of the bits [offset..offset+size-1] in [typ]. May return a subtype
    of [typ], or a type that is a sub-array of an array type in [typ].
    Also returns a {!Cil_types.offset} [off] that corresponds to [offset].
    (But we do not have the guarantee that [typeof(off) == typ], because of
    sub-arrays.)
    @raise NoMatchingOffset when no offset matches. *)
val find_offset:
  typ -> offset:Z.t -> offset_match -> Cil_types.offset * Cil_types.typ
