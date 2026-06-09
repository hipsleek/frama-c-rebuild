(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Ranges
open Cil_types

val bitsSizeOf : typ -> int
val bytesSizeOf : typ -> int

type domain
type field = fieldinfo range
type slice = Bits of int | Field of fieldinfo

val empty : domain
val singleton : fieldinfo -> domain
val union : domain -> domain -> domain
val iter : (compinfo -> unit) -> domain -> unit
val compare : field -> field -> int
val find_all : domain -> 'a range -> field list
val find : domain -> 'a range -> field option
val span : domain -> 'a range -> slice list

val pp_bits : Format.formatter -> int -> unit
val pp_slice : Format.formatter -> slice -> unit

val pretty : domain -> Format.formatter -> 'a range -> unit
val pslice : Format.formatter -> fields:domain -> offset:int -> length:int -> unit
