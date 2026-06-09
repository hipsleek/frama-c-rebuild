(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Types used by E-ACSL analyses *)

open Cil_types

type lscope_var =
  | Lvs_let of logic_var * term (* the expression to which the lv is binded *)
  | Lvs_quantif of term * relation * logic_var * relation * term
  | Lvs_formal of logic_var * logic_info (* the logic definition *)
  | Lvs_global of logic_var * term (* same as Lvs_let *)

type lscope = lscope_var list

type pred_or_term =
  | PoT_pred of predicate
  | PoT_term of term

(** Type uniquely representing a [predicate] or [term] with an associated
    [label], and the necessary information for its translation. *)
type at_data = {
  kf: kernel_function;
  (** [kernel_function] englobing the [pred_or_term]. *)

  kinstr: kinstr;
  (** [kinstr] where the [pred_or_term] is used. *)

  lscope: lscope;
  (** Current state of the [lscope] for the [pred_or_term]. *)

  pot: pred_or_term;
  (** [pred_or_term] to translate. *)

  label: logic_label;
  (** Label of the [pred_or_term]. *)

  error: exn option
  (** Error raised during the pre-analysis.
      This field does not contribute to the equality and comparison between two
      [at_data]. *)
}

type annotation_kind =
  | Assertion
  | Precondition
  | Postcondition
  | Invariant
  | Variant
  | RTE

(** Type of intervals inferred by the interval inference *)
type ival =
  | Ival of Ival.t
  | Float of fkind * float option (* a float constant, if any *)
  | Rational
  | Real
  | Nan

let pp_ival fmt = function
  | Ival i -> Ival.pretty fmt i
  | Float _ -> Format.fprintf fmt "F"
  | Rational -> Format.fprintf fmt "Q"
  | Real -> Format.fprintf fmt "R"
  | Nan -> Format.fprintf fmt "Nan"

(** Type of types inferred by the type inference for types representing
    numbers *)
type number_ty =
  | C_integer of ikind
  | C_float of fkind
  | Gmpz
  | Rational
  | Real
  | Nan

let pp_number_ty fmt = function
  | C_integer ikind -> Format.fprintf fmt "C_integer %a" Printer.pp_ikind ikind
  | C_float fkind -> Format.fprintf fmt "C_float %a" Printer.pp_fkind fkind
  | Gmpz -> Format.pp_print_string fmt "Gmpz"
  | Rational -> Format.pp_print_string fmt "Rational"
  | Real -> Format.pp_print_string fmt "Real"
  | Nan -> Format.pp_print_string fmt "Nan"

(** Type of a string that represents a number.
    Used when a string is required to encode a constant number because it is not
    representable in any C type  *)
type strnum =
  | Str_Z         (* integers *)
  | Str_R         (* reals *)
  | C_number      (* integers and floats included *)

let pp_strnum fmt = function
  | Str_Z -> Format.pp_print_string fmt "Str_Z"
  | Str_R -> Format.pp_print_string fmt "Str_R"
  | C_number -> Format.pp_print_string fmt "C_number"
