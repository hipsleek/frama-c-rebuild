(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type float_display =
  | Default
  | NormDec
  | NormHex

let float_print_mode = ref Default

let set_float_display kind = float_print_mode := kind

type rounding =
  | Nearest_even
  | Upward
  | Downward
  | Toward_zero

external set_rounding_mode : rounding -> unit = "frama_c_set_round_mode" [@@noalloc]
external get_rounding_mode : unit -> rounding = "frama_c_get_round_mode" [@@noalloc]

external round_to_single_precision : float -> float = "round_to_single"

type fkind =
  | FFloat
  | FFloat32
  | FFloat64
  | FDouble
  | FLongDouble

let round_if_single_precision = function
  | FFloat | FFloat32 -> round_to_single_precision
  | FDouble | FFloat64 | FLongDouble -> Fun.id


type truncated_to_integer =
  | Integer of Z.t
  | Underflow
  | Overflow

let min_64_float = -9.22337203685477581e+18
let max_64_float = +9.22337203685477478e+18

(* If the argument [x] is not in the range [min_64_float, 2*max_64_float],
   raise Float_Non_representable_as_Int64. This is the most reasonable as
   a floating-point number may represent an exponentially large integer. *)
let truncate_to_integer f =
  let convert f = Z.of_int64 (Int64.of_float f) in
  let shift n = Z.(add (two_power_of_int 63)) n in
  let unsigned x = convert (x +. min_64_float) |> shift in
  if min_64_float <= f then
    if f <= max_64_float then Integer (convert f)
    else if f <= 2. *. max_64_float then Integer (unsigned f)
    else Overflow
  else Underflow


let is_finite f =
  match classify_float f with
  | FP_infinite | FP_nan -> false
  | _ -> true

let is_infinite f =
  match classify_float f with
  | FP_infinite -> true
  | _ -> false

let is_nan f =
  match classify_float f with
  | FP_nan -> true
  | _ -> false


(* Floats should be printed with the nearest-even rounding mode. This function
   temporarily changes this mode, prints and then switches it back. *)
let pretty_rounding_mode pretty =
  let old_mode = get_rounding_mode () in
  let finally () = set_rounding_mode old_mode in
  let work () = set_rounding_mode Nearest_even; pretty () in
  Fun.protect ~finally work

let pretty_normal ~use_hex fmt f =
  let open Stdlib in
  let double_norm = Int64.shift_left 1L 52 in
  let double_mask = Int64.pred double_norm in
  let i = Int64.bits_of_float f in
  let s = 0L <> (Int64.logand Int64.min_int i) in
  let i = Int64.logand Int64.max_int i in
  let exp = Int64.to_int (Int64.shift_right_logical i 52) in
  let man = Int64.logand i double_mask in
  let s = if s then "-" else "" in
  if exp = 2047 then
    Format.(if man = 0L then fprintf fmt "%sinf" s else fprintf fmt "NaN")
  else
    let firstdigit = if exp <> 0 then 1 else 0 in
    let exp = if exp <> 0 then exp - 1023 else if f = 0. then 0 else -1022 in
    if not use_hex then
      let in_bound = 0 < exp && exp <= 12 in
      let doubled_man = Int64.logor man double_norm in
      let shifted = Int64.shift_right_logical doubled_man (52 - exp) in
      let firstdigit = if in_bound then Int64.to_int shifted else firstdigit in
      let doubled = Int64.(logand (shift_left man exp) double_mask) in
      let man = if in_bound then doubled else man in
      let exp = if in_bound then 0 else exp in
      let d = Int64.(float_of_bits (logor 0x3ff0000000000000L man)) in
      let re = if d >= 1.5 then 5000000000000000L else 0L in
      let shift = if d >= 1.5 then 1.5 else 1.0 in
      let d = (d -. shift) *. 1e16 in
      let decdigits = Int64.add re (Int64.of_float d) in
      if exp = 0 || (firstdigit = 0 && decdigits = 0L && exp = -1022)
      then Format.fprintf fmt "%s%d.%016Ld" s firstdigit decdigits
      else Format.fprintf fmt "%s%d.%016Ld*2^%d" s firstdigit decdigits exp
    else Format.fprintf fmt "%s0x%d.%013Lxp%d" s firstdigit man exp

let pretty_normal ~use_hex fmt f =
  pretty_rounding_mode (fun () -> pretty_normal ~use_hex fmt f)

let pretty fmt f =
  match !float_print_mode with
  | Default ->
    let r = Format.sprintf "%.*g" 12 f in
    let contains = String.contains r in
    let is_not_integer = contains '.' || contains 'e' || contains 'E' in
    let dot = if is_not_integer || not (is_finite f) then "" else "." in
    Format.fprintf fmt "%s%s" r dot
  | NormDec -> pretty_normal ~use_hex:false fmt f
  | NormHex -> pretty_normal ~use_hex:true fmt f

let pretty fmt f =
  pretty_rounding_mode (fun () -> pretty fmt f)

let suffix_of_fkind = function
  | FFloat   -> "F"
  | FFloat32 -> "F32"
  | FFloat64 -> "F64"
  | FDouble  -> "D"
  | FLongDouble -> "L"

let has_suffix fkind literal =
  let suffix = suffix_of_fkind fkind in
  let literal_upper = String.uppercase_ascii literal in
  String.ends_with ~suffix literal_upper

let extract_single_letter_suffix s len =
  let last = String.sub s (len - 1) 1 in
  match last with
  | "f" | "F" -> Some (String.sub s 0 (len - 1), last, FFloat)
  (* Note: 'd' is accepted as a GCC extension, but also always
     accepted in ACSL *)
  | "d" | "D" -> Some (String.sub s 0 (len - 1), last, FDouble)
  | "l" | "L" -> Some (String.sub s 0 (len - 1), last, FLongDouble)
  | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "." ->
    Some (s, "", FDouble)
  | _ -> None

let extract_suffix s =
  let len = String.length s in
  if len = 0 then Some (s, "", FDouble)
  else if len <= 3 then
    extract_single_letter_suffix s len
  else
    (* look for a 3-letter suffix, then for a 1-letter suffix *)
    let last3 = String.sub s (len - 3) 3 in
    match last3 with
    | "f32" | "F32" -> Some (String.sub s 0 (len - 3), last3, FFloat32)
    | "f64" | "F64" -> Some (String.sub s 0 (len - 3), last3, FFloat64)
    | _             -> extract_single_letter_suffix s len

type format = Single | Double

let sig_size = function Single -> 24 | Double -> 53
let exp_size = function Single ->  8 | Double -> 11

let largest_finite_float_of format =
  let exponent format = Int.shift_left 1 (exp_size format - 1) - 1 in
  let base format = 2.0 -. ldexp 1.0 (1 - sig_size format) in
  ldexp (base format) (exponent format)

let finite_range_of format =
  let upper = largest_finite_float_of format in Float.neg upper, upper

let smallest_normal_float_of format=
  let exponent format = 2 - Int.shift_left 1 (exp_size format - 1) in
  ldexp 1.0 (exponent format)

let smallest_denormal_float_of = function
  | Single -> Int32.float_of_bits 1l
  | Double -> Int64.float_of_bits 1L

let unit_in_the_last_place_of format = ldexp 1.0 (- sig_size format)

(* Only compute 2^7 or 2^10 below, so no overflow. *)
let two_power n = Z.(to_int (two_power_of_int n))

let minimal_exponent_of format = 2 - two_power (exp_size format - 1)
let maximal_exponent_of format = two_power (exp_size format - 1) - 1
