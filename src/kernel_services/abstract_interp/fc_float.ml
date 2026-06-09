(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Float_sig



let convert_rounding = function
  | Float_sig.Up   -> Floating_point.Upward
  | Float_sig.Down -> Floating_point.Downward
  | Float_sig.Near -> Floating_point.Nearest_even
  | Float_sig.Zero -> Floating_point.Toward_zero

(* Apply [computation arg] with rounding mode [rounding]. *)
let with_rounding_mode rounding computation arg =
  let new_rounding = convert_rounding rounding in
  let old_rounding = Floating_point.get_rounding_mode () in
  let change_rounding = old_rounding <> new_rounding in
  if change_rounding then Floating_point.set_rounding_mode new_rounding ;
  let result = computation arg in
  if change_rounding then Floating_point.set_rounding_mode old_rounding ;
  result

type operation = RoundingMode
let ( let<> ) rounding_mode f = with_rounding_mode rounding_mode f RoundingMode



type t = float
let hash = Hashtbl.hash
let packed_descr = Structural_descr.p_float
let pretty = Floating_point.pretty

let is_exact = function
  | Single | Double -> true
  | Long_Double | Real -> false

let round_to_precision round prec t =
  if prec = Single
  then
    let<> RoundingMode = round in
    Floating_point.round_to_single_precision t
  else t

let of_float = round_to_precision

let to_float f = f



(** NOTE: all floating-point comparisons using OCaml's standard operators
    do NOT distinguish between -0.0 and 0.0.
    Whenever floats are compared using them, it implies that negative zeroes
    are also considered, e.g. "if x < 0.0" is equivalent to "if x < -0.0",
    which is also equivalent to "F.compare x (-0.0) < 0".
    This 'total_compare' operator distinguishes -0. and 0. *)
external total_compare : float -> float -> int = "float_compare_total" [@@noalloc]

let cmp_ieee = (compare: float -> float -> int)
let compare = total_compare

let is_nan a = classify_float a = FP_nan
let is_infinite f = classify_float f = FP_infinite

let is_finite f =
  match classify_float f with
  | FP_nan | FP_infinite -> false
  | FP_normal | FP_subnormal | FP_zero -> true

external is_negative : float -> bool = "float_is_negative" [@@noalloc]



(* Wrong for [next -0.] and [prev 0.], as this function uses the binary
   representation of floating-point values, which is not continuous at 0. *)
let next_previous int64fup int64fdown float =
  let r = Int64.bits_of_float float in
  let f = if r >= 0L then int64fup else int64fdown in
  let f = Int64.float_of_bits (f r) in
  match classify_float f with
  | FP_nan -> float (* can only be produced from an infinity or a nan *)
  | FP_infinite | FP_normal | FP_subnormal | FP_zero -> f

let next_float prec f =
  if is_exact prec then
    if compare f (-0.) <> 0 then
      let f = next_previous Int64.succ Int64.pred f in
      round_to_precision Up prec f
    else 0.
  else f

let prev_float prec f =
  if is_exact prec then
    if compare f 0. <> 0 then
      let f = next_previous Int64.pred Int64.succ f in
      round_to_precision Down prec f
    else -0.
  else f

let widen_up ?(hint = Datatype.Float.Set.empty) prec f =
  let is_upper e = total_compare e f >= 0 in
  let threshold_opt = Datatype.Float.Set.find_first_opt is_upper hint in
  let default = if f <= max_float then max_float else infinity in
  let result = Option.value ~default threshold_opt in
  round_to_precision Up prec result

let widen_down ?(hint = Datatype.Float.Set.empty) prec f =
  let is_lower e = total_compare e f <= 0 in
  let threshold_opt = Datatype.Float.Set.find_last_opt is_lower hint in
  let default = if f >= (-.max_float) then -.max_float else neg_infinity in
  let result = Option.value ~default threshold_opt in
  round_to_precision Down prec result



let neg = (~-.)
let abs = abs_float
let floor = floor
let ceil  = ceil

let trunc  f = Typed_float.(double f |> trunc |> to_float)
let fround f = Typed_float.(single f |> round |> to_float)

type format = Format : 'f Typed_float.format -> format

let format_of_prec = function
  | Single -> Format Single
  | _      -> Format Double

type 'f number = 'f Typed_float.t
type unary = { compute : 'f. 'f number -> 'f number }
type binary = { compute : 'f. 'f number -> 'f number -> 'f number }

let unary (op : unary) rounding prec x =
  let<> RoundingMode = rounding in
  let Format fmt = format_of_prec prec in
  let x = Typed_float.represents ~float:x ~in_format:fmt in
  Typed_float.(op.compute x |> to_float)

let binary (op : binary) rounding prec x y =
  let<> RoundingMode = rounding in
  let Format fmt = format_of_prec prec in
  let x = Typed_float.represents ~float:x ~in_format:fmt in
  let y = Typed_float.represents ~float:y ~in_format:fmt in
  Typed_float.(op.compute x y |> to_float)

let add  = binary { compute = Typed_float.( + ) }
let sub  = binary { compute = Typed_float.( - ) }
let mul  = binary { compute = Typed_float.( * ) }
let div  = binary { compute = Typed_float.( / ) }
let fmod = binary { compute = Typed_float.fmod }

let exp   = unary { compute = Typed_float.exp }
let log   = unary { compute = Typed_float.log }
let log10 = unary { compute = Typed_float.log10 }
let sqrt  = unary { compute = Typed_float.sqrt }
let pow   = binary { compute = Typed_float.pow }

let cos   = unary { compute = Typed_float.cos }
let sin   = unary { compute = Typed_float.sin }
let tan   = unary { compute = Typed_float.tan }
let acos  = unary { compute = Typed_float.acos }
let asin  = unary { compute = Typed_float.asin }
let atan  = unary { compute = Typed_float.atan }
let atan2 = binary { compute = Typed_float.atan2 }
