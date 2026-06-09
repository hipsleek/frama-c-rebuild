(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Format_types
open Cil_types

exception Type_not_found of string
exception Invalid_specifier

type arg_dir = [ `ArgIn
               | `ArgInArray of precision option (* for '%.*s' or '%.42s' *)
               | `ArgOut
               | `ArgOutArray ]

type typdef_finder = Logic_typing.type_namespace -> string -> Cil_types.typ

val type_f_specifier : ?find_typedef : typdef_finder -> f_conversion_specification -> typ
val type_s_specifier : ?find_typedef : typdef_finder -> s_conversion_specification -> typ
val type_f_format : ?find_typedef : typdef_finder ->  f_format -> (typ * arg_dir) list
val type_s_format : ?find_typedef : typdef_finder ->  s_format -> (typ * arg_dir) list
val type_format : ?find_typedef : typdef_finder -> format -> (typ * arg_dir) list
