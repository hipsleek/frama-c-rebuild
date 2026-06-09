(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Format_types

exception Type_not_found of string
exception Invalid_specifier

type arg_dir = [ `ArgIn
               | `ArgInArray of precision option (* for '%.*s' or '%.42s' *)
               | `ArgOut
               | `ArgOutArray ]

type typdef_finder = Logic_typing.type_namespace -> string -> Cil_types.typ


let get_typedef ?(find_typedef = Globals.Types.find_type) s =
  try
    find_typedef Logic_typing.Typedef s
  with Not_found ->
    raise (Type_not_found s)

let ptr = Cil_const.mk_tptr

let type_f_specifier ?find_typedef spec =
  match spec.f_conversion_specifier, spec.f_length_modifier with
  | #signed_specifier, None     -> Cil_const.intType
  | #signed_specifier, Some `hh -> Cil_const.scharType
  | #signed_specifier, Some `h  -> Cil_const.shortType
  | #signed_specifier, Some `l  -> Cil_const.longType
  | #signed_specifier, Some `ll -> Cil_const.longLongType
  | #signed_specifier, Some `j  -> get_typedef ?find_typedef "intmax_t"
  | #signed_specifier, Some `z  -> get_typedef ?find_typedef "size_t"
  | #signed_specifier, Some `t  -> get_typedef ?find_typedef "ptrdiff_t"
  | #unsigned_specifier, None     -> Cil_const.uintType
  | #unsigned_specifier, Some `hh -> Cil_const.ucharType
  | #unsigned_specifier, Some `h  -> Cil_const.ushortType
  | #unsigned_specifier, Some `l  -> Cil_const.ulongType
  | #unsigned_specifier, Some `ll -> Cil_const.ulongLongType
  | #unsigned_specifier, Some `j  -> get_typedef ?find_typedef "uintmax_t"
  | #unsigned_specifier, Some `z  -> get_typedef ?find_typedef "size_t"
  | #unsigned_specifier, Some `t  -> get_typedef ?find_typedef "ptrdiff_t"
  | #float_specifier, None    -> Cil_const.doubleType
  | #float_specifier, Some `l -> Cil_const.doubleType
  | #float_specifier, Some `L -> Cil_const.longDoubleType
  | `c, None    -> Cil_const.intType
  | `c, Some `l -> get_typedef ?find_typedef "wint_t"
  | `s, None    -> Cil_const.charPtrType
  | `s, Some `l -> ptr (get_typedef ?find_typedef "wchar_t")
  | `p, None    -> Cil_const.voidPtrType
  | `n, None     -> ptr Cil_const.intType
  | `n, Some `hh -> ptr Cil_const.scharType
  | `n, Some `h  -> ptr Cil_const.shortType
  | `n, Some `l  -> ptr Cil_const.longType
  | `n, Some `ll -> ptr Cil_const.longLongType
  | `n, Some `j  -> ptr (get_typedef ?find_typedef "intmax_t")
  | `n, Some `z  -> ptr (get_typedef ?find_typedef "size_t")
  | `n, Some `t  -> ptr (get_typedef ?find_typedef "ptrdiff_t")
  | _, _ -> raise Invalid_specifier

let type_s_specifier ?find_typedef spec =
  match spec.s_conversion_specifier, spec.s_length_modifier with
  | #signed_specifier, None     -> ptr Cil_const.intType
  | #signed_specifier, Some `hh -> ptr Cil_const.scharType
  | #signed_specifier, Some `h  -> ptr Cil_const.shortType
  | #signed_specifier, Some `l  -> ptr Cil_const.longType
  | #signed_specifier, Some `ll -> ptr Cil_const.longLongType
  | #signed_specifier, Some `j  -> ptr (get_typedef ?find_typedef "intmax_t")
  | #signed_specifier, Some `z  -> ptr (get_typedef ?find_typedef "size_t")
  | #signed_specifier, Some `t  -> ptr (get_typedef ?find_typedef "ptrdiff_t")
  | #unsigned_specifier, None     -> ptr Cil_const.uintType
  | #unsigned_specifier, Some `hh -> ptr Cil_const.ucharType
  | #unsigned_specifier, Some `h  -> ptr Cil_const.ushortType
  | #unsigned_specifier, Some `l  -> ptr Cil_const.ulongType
  | #unsigned_specifier, Some `ll -> ptr Cil_const.ulongLongType
  | #unsigned_specifier, Some `j  -> ptr (get_typedef ?find_typedef "uintmax_t")
  | #unsigned_specifier, Some `z  -> ptr (get_typedef ?find_typedef "size_t")
  | #unsigned_specifier, Some `t  -> ptr (get_typedef ?find_typedef "ptrdiff_t")
  | #float_specifier, None    -> ptr (Cil_const.floatType)
  | #float_specifier, Some `l -> ptr (Cil_const.doubleType)
  | #float_specifier, Some `L -> ptr (Cil_const.longDoubleType)
  | (`c | `s | `Brackets _), None    -> Cil_const.charPtrType
  | (`c | `s | `Brackets _), Some `l -> ptr (get_typedef ?find_typedef "wchar_t")
  | `p, None    -> ptr (Cil_const.voidPtrType)
  | `n, None     -> ptr Cil_const.intType
  | `n, Some `hh -> ptr Cil_const.scharType
  | `n, Some `h  -> ptr Cil_const.shortType
  | `n, Some `l  -> ptr Cil_const.longType
  | `n, Some `ll -> ptr Cil_const.longLongType
  | `n, Some `j  -> ptr (get_typedef ?find_typedef "intmax_t")
  | `n, Some `z  -> ptr (get_typedef ?find_typedef "size_t")
  | `n, Some `t  -> ptr (get_typedef ?find_typedef "ptrdiff_t")
  | _, _ -> raise Invalid_specifier


let type_f_format ?find_typedef format =
  let r = ref [] in
  let add_types spec =
    match spec with
    | Char _ -> ()
    | Specification s ->
      if s.f_field_width = Some `FWStar then
        r := (Cil_const.intType, `ArgIn) :: !r;
      if s.f_precision = Some PStar then
        r := (Cil_const.intType, `ArgIn) :: !r;
      let dir = match s.f_conversion_specifier with
        | `s -> `ArgInArray s.f_precision
        | `n -> `ArgOut
        | _ ->  `ArgIn
      in
      r := (type_f_specifier ?find_typedef s, dir) :: !r;
  in
  List.iter add_types format;
  List.rev !r

let type_s_format ?find_typedef format =
  let r = ref [] in
  let add_types spec =
    match spec with
    | Char _ -> ()
    | Specification s ->
      let dir = match s.s_conversion_specifier with
        | `s -> `ArgOutArray
        | _ -> `ArgOut
      in
      if not s.s_assignment_suppression then
        r := (type_s_specifier ?find_typedef s, dir) :: !r;
  in
  List.iter add_types format;
  List.rev !r


let type_format ?find_typedef = function
  | FFormat f -> type_f_format ?find_typedef f
  | SFormat s -> type_s_format ?find_typedef s
