(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Z

(* Deprecated module *)

type 'a formatter = Format.formatter -> 'a -> unit

let two = 2z
let four = 4z
let eight = 8z
let sixteen = 16z
let thirtytwo = 32z
let onethousand = 1000z
let billion_one = 1_000_000_001_z
let two_power_32 = two_power_of_int 32
let two_power_64 = two_power_of_int 64

let max_int64 = of_int64 Int64.max_int
let min_int64 = of_int64 Int64.min_int

let le = leq
let ge = geq

let two_power_of_int k = two_power_of_int k
let two_power n = two_power n

let shift_left = shift_left_z
let shift_right = shift_right_z

let e_div = ediv
let e_rem = erem
let e_div_rem = ediv_rem
let c_div = div
let c_rem = rem
let c_div_rem = div_rem

let pgcd = gcd
let ppcm = lcm

(* These functions can raise Z.Overflow, so we make it explicit. *)
let to_int_exn = to_int
let to_int32_exn = to_int32
let to_int64_exn = to_int64

let power_int_positive_int_opt n e =
  try Some (Big_int_Z.power_int_positive_int n e)
  with Invalid_argument _ -> None
