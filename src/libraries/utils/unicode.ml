(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type printer = Format.formatter -> unit

let use_utf8_unicode = ref true

let use_unicode b = use_utf8_unicode := b

(* Before OCaml 5.4, an UTF-8 character is seen as several characters, leading
   pretty-printers to split lines that do not overflow the right margin.
   To avoid this issue (which caused test oracle changes with OCaml 5.4),
   we print such characters as if they were of length 1 — which they are. *)
let pretty utf8 ascii = fun fmt ->
  if !use_utf8_unicode
  then Format.pp_print_as fmt 1 utf8
  else Format.pp_print_string fmt ascii

(* Set operations. *)

let pp_in_set =    pretty Utf8_logic.inset "IN"
let pp_empty_set = pretty Utf8_logic.emptyset "EMPTY_SET"
let pp_union =     pretty Utf8_logic.union "U"
let pp_top =       pretty Utf8_logic.top "TOP"
let pp_bottom =    pretty Utf8_logic.bottom "BOTTOM"

(* Relations. *)

let pp_le =  pretty Utf8_logic.le "<="
let pp_ge =  pretty Utf8_logic.ge ">="
let pp_eq =  pretty Utf8_logic.eq "=="
let pp_neq = pretty Utf8_logic.neq "!="

(* Logic operators. *)

let pp_not = pretty Utf8_logic.neg "!"
let pp_and = pretty Utf8_logic.conj "&&"
let pp_or =  pretty Utf8_logic.disj "||"
let pp_xor = pretty Utf8_logic.x_or "^^"

let pp_implies = pretty Utf8_logic.implies "==>"
let pp_iff =     pretty Utf8_logic.iff "<==>"

let pp_in_acsl = pretty Utf8_logic.inset "\\in"
let pp_forall = pretty Utf8_logic.forall "\\forall"
let pp_exists = pretty Utf8_logic.exists "\\exists"

(* Logic types. *)

let pp_boolean = pretty Utf8_logic.boolean "boolean"
let pp_integer = pretty Utf8_logic.integer "integer"
let pp_real =    pretty Utf8_logic.real "real"

(* Greek letters. *)

let pp_alpha = pretty "α" "\\alpha"
let pp_pi = pretty Utf8_logic.pi "\\pi"
let pp_lambda = pretty "λ" "\\lambda"
let pp_mu = pretty "µ" "\\mu"
let pp_theta = pretty "θ" "\\theta"

module Capital = struct
  let pp_theta = pretty "Θ" "\\Theta"
end

(* Superscript/subscript *)
let super_digits = [| "⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹"; "⁻" |]
let sub_digits = [| "₀"; "₁"; "₂"; "₃"; "₄"; "₅"; "₆"; "₇"; "₈"; "₉"; "₋" |]

let pp_digit_char digits fmt c =
  let s =
    match c with
    | '0' .. '9' -> digits.(int_of_char c - int_of_char '0')
    | '-' -> digits.(10)
    | _ ->
      invalid_arg (Format.asprintf "no version of '%c' in %a"
                     c (Array.pretty Format.pp_print_string) digits)
  in
  Format.pp_print_as fmt 1 s

let pp_super_char = pp_digit_char super_digits
let pp_sub_char = pp_digit_char sub_digits

let pp_super_int fmt value =
  if !use_utf8_unicode then
    Int.to_string value |> String.iter (pp_super_char fmt)
  else
    Format.fprintf fmt "^%d" value

let pp_sub_int fmt value =
  if !use_utf8_unicode then
    Int.to_string value |> String.iter (pp_sub_char fmt)
  else
    Format.fprintf fmt "_%d" value

(* Other symbols. *)

let pp_right_arrow = pretty "→" "->"
let pp_maps_to = pretty "↦" "->"
let pp_plus_minus = pretty "±" "+/-"
let pp_times = pretty "×" "x"
let pp_multiplication_dot = pretty "⋅" "."
let pp_ellipsis = pretty "…" "..."

let pp_lceil = pretty "⌈" "ceil("
let pp_rceil = pretty "⌉" ")"
let pp_lfloor = pretty "⌊" "floor("
let pp_rfloor = pretty "⌋" ")"

let pp_ceil pp fmt elt = Format.fprintf fmt "%t%a%t" pp_lceil pp elt pp_rceil
let pp_floor pp fmt elt = Format.fprintf fmt "%t%a%t" pp_lfloor pp elt pp_rfloor

let pp_string fmt s =
  Format.pp_print_as fmt (String.utf8_length s) s
