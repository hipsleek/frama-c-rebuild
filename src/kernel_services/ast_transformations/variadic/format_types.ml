(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** See C11, 7.21.6 *)

type flag = FMinus | FPlus | FSpace | FSharp | FZero
type flags = flag list

type f_field_width = [ `FWStar | `FWInt of int (** positive integer *)]
type s_field_width = [ `FWInt of int ]
type any_field_width = [ f_field_width | s_field_width ]

type precision = PStar | PInt of int

type length_modifier = [ `hh | `h | `l | `ll | `j | `z | `t | `L ]

type signed_specifier = [ `d | `i ]
type unsigned_specifier = [ `b | `o | `u | `x ]
type integer_specifier = [ signed_specifier | unsigned_specifier ]
type float_specifier = [ `f | `e | `g | `a  | `f32 | `f64 ]
type numeric_specifier = [ integer_specifier | float_specifier ]
type capitalizable = [ `b | `x | `f | `e | `g | `a  ]
type has_alternative_form = [ `b | `o | `x | `f | `e | `g | `a  ]

type f_conversion_specifier =
  [ numeric_specifier | `c | `s | `p | `n ]
type s_conversion_specifier =
  [ f_conversion_specifier | `Brackets of string ]
type any_conversion_specifier =
  [ s_conversion_specifier | f_conversion_specifier ]

type f_conversion_specification = {
  mutable f_flags: flags;
  mutable f_field_width: f_field_width option;
  mutable f_precision: precision option;
  mutable f_length_modifier: length_modifier option;
  mutable f_conversion_specifier: f_conversion_specifier;
  mutable f_capitalize: bool;
}

type s_conversion_specification = {
  mutable s_assignment_suppression: bool;
  mutable s_field_width: s_field_width option;
  mutable s_length_modifier: length_modifier option;
  mutable s_conversion_specifier: s_conversion_specifier;
}

(** A format element is either a character or a conversion specification. *)
type 'spec token =
  | Char of char
  | Specification of 'spec

type f_format = f_conversion_specification token list
type s_format = s_conversion_specification token list

type format = FFormat of f_format | SFormat of s_format
type format_kind = PrintfLike | ScanfLike
