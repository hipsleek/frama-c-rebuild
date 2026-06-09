(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type t =
  | String of string
  | WString of int64 list

exception OutOfBounds
exception NotAscii of int64

val get_char : t -> int -> char
val get_wchar : t -> int -> int64
val sub_string : t -> int -> int -> string
