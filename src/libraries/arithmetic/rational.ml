(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Datatype.Rational
type scalar = t

let zero = Q.zero
let one = Q.one
let two = Q.of_int 2
let ten = 10z
let pos_inf = Q.inf
let neg_inf = Q.minus_inf
let of_float = Q.of_float
let to_float = Q.to_float
let of_int = Q.of_int

let represents ~scalar ~in_format =
  let f = Typed_float.represents ~float:(to_float scalar) ~in_format in
  Q.of_float (Typed_float.to_float f)

let is_valid q =
  match Q.classify q with
  | Q.UNDEF -> false
  | _ -> true

let pow10 e = Z.(pow ten e) |> Q.of_bigint

let split_significant_exponent s =
  match String.split_on_char 'e' s with
  | [] | _ :: _ :: _ :: _ -> assert false
  | [ s ] -> s, 0
  | [ m ; e ] -> m, int_of_string e

let significant_of_string significant =
  match String.split_on_char '.' significant with
  | [] | _ :: _ :: _ :: _ -> assert false
  | [ m ] -> Q.of_string m
  | [ integer ; fractional ] ->
    let shift = pow10 (String.length fractional) in
    Q.(of_string (integer ^ fractional) / shift)

let remove_float_suffix str len =
  if String.ends_with ~suffix:"f" str
  || String.ends_with ~suffix:"l" str then String.sub str 0 (len - 1)
  else if String.ends_with ~suffix:"f32" str
       || String.ends_with ~suffix:"f64" str then
    String.sub str 0 (len - 3)
  else str

let of_string str =
  let str = String.lowercase_ascii str in
  let length = String.length str in
  let str = remove_float_suffix str length in
  let significant, e = split_significant_exponent str in
  let significant = significant_of_string significant in
  let shift = if e >= 0 then pow10 e else Q.inv (pow10 ~-e) in
  Q.(significant * shift)

let pow2 e =
  let scaling = Q.(mul_2exp one (Stdlib.abs e)) in
  if e >= 0 then scaling else Q.inv scaling

(* We want to compute ⌊log₂ q⌋ ≤ q ≤ ⌈log₂ q⌉, where q = a / b is a rational.
   This operation is not provided by Zarith so we cannot compute directly
   either ⌊log₂ (a / b)⌋ or ⌈log₂ (a / b)⌉. However, we can compute
   n = ⌊log₂ a⌋ and m = ⌊log₂ b⌋ using Zarith. Those equalities
   mean that n ≤ log₂ a < n + 1 and m ≤ log₂ b < m + 1, and thus
   we obtain n - m - 1 < log₂ a - log₂ b < n - m + 1, which is equivalent
   to 2 ^ (n - m - 1) < a / b < 2 ^ (n - m + 1). However, those bounds are
   not optimal. Indeed, we necessarily have one of the following :
   - n - m - 1 < n - m ≤ log₂ a - log₂ b < n - m + 1
   - n - m - 1 < log₂ a - log₂ b ≤ n - m < n - m + 1

   Testing which one is true comes down to check if 2 ^ (n - m) ≤ (a / b). *)
let log2 q =
  if Q.(q <= zero) then raise (Invalid_argument (Q.to_string q)) ;
  let middle = (Q.num q |> Z.log2) - (Q.den q |> Z.log2) in
  if Q.(pow2 middle <= q)
  then Field.{ lower = middle ; upper = middle + 1 }
  else Field.{ lower = middle - 1 ; upper = middle }

let neg = Q.neg
let abs = Q.abs
let min = Q.min
let max = Q.max

let sqrt q =
  if Q.sign q = 1 then
    let num = Q.num q and den = Q.den q in
    let acceptable_delta = Q.inv (Q.of_bigint @@ Z.pow ten 32) in
    let rec approx_starting_at_scaling scaling =
      let lower = Z.(sqrt (num * den * scaling * scaling)) in
      let upper = Z.(lower + one) in
      let denominator = Z.(den * scaling) in
      let lower = Q.(make lower denominator) in
      let upper = Q.(make upper denominator) in
      let delta = Q.(upper - lower) in
      if Q.(delta <= acceptable_delta) then Field.{ lower ; upper }
      else approx_starting_at_scaling Z.(scaling * scaling)
    in approx_starting_at_scaling ten
  else { lower = neg_inf ; upper = pos_inf }

let ( + ) = Q.( + )
let ( - ) = Q.( - )
let ( * ) = Q.( * )
let ( / ) = Q.( / )

let ( =  ) l r = Q.equal l r
let ( != ) l r = not (l = r)
let ( <= ) = Q.( <= )
let ( <  ) = Q.( <  )
let ( >= ) = Q.( >= )
let ( >  ) = Q.( >  )
