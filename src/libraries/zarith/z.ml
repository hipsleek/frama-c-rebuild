(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Fc_internal_z [@@alert "-fc_internal_z"]

(* ----------- *)
(* Conversions *)
(* ----------- *)

let wrap to_int i = try Some (to_int i) with Overflow -> None
let to_int_opt = wrap to_int
let to_int64_opt = wrap to_int64
let to_int32_opt = wrap to_int32

(* ------------------------- *)
(* Basic functions and utils *)
(* ------------------------- *)

let pow_exponent_limit = ref 99999

let set_pow_exponent_limit x = pow_exponent_limit := x

let pow ?(limit = !pow_exponent_limit) b e =
  if e > limit && gt (abs b) one
  then raise Overflow
  else pow b e

let two_power_of_int ?(limit = !pow_exponent_limit) k =
  if k > limit then
    raise Overflow
  else
    shift_left one k

let two_power ?limit n =
  two_power_of_int ?limit (to_int n)

(* We redefine shifts to operate on t instead of int. *)
let shift_left_z x y = shift_left x (to_int y)
let shift_right_z x y = shift_right x (to_int y)
let shift_right_logical x y = (* no meaning for negative value of x *)
  if (lt x zero)
  then raise (Invalid_argument "Z.shift_right_logical")
  else shift_right_z x y

let is_zero = equal zero
let is_one  = equal one

let length u v = succ (sub v u)

let round_down_to_zero v modu =
  mul (ediv v modu) modu

let round_up_to_r ~min:m ~r ~modu =
  add (add (round_down_to_zero (pred (sub m r)) modu) r) modu

let round_down_to_r ~max:m ~r ~modu =
  add (round_down_to_zero (sub m r) modu) r

(* -------- *)
(* Printers *)
(* -------- *)

let bdigits = [|
  "0000" ; (* 0 *)
  "0001" ; (* 1 *)
  "0010" ; (* 2 *)
  "0011" ; (* 3 *)
  "0100" ; (* 4 *)
  "0101" ; (* 5 *)
  "0110" ; (* 6 *)
  "0111" ; (* 7 *)
  "1000" ; (* 8 *)
  "1001" ; (* 9 *)
  "1010" ; (* 10 *)
  "1011" ; (* 11 *)
  "1100" ; (* 12 *)
  "1101" ; (* 13 *)
  "1110" ; (* 14 *)
  "1111" ; (* 15 *)
|]

let pp_bin_pos fmt r = Format.pp_print_string fmt bdigits.(r)

let pp_bin_neg fmt r = Format.pp_print_string fmt Stdlib.(bdigits.(15-r))

let pp_hex_pos fmt r = Format.fprintf fmt "%04X" r
let pp_hex_neg fmt r = Format.fprintf fmt "%04X" Stdlib.(0xFFFF-r)

let bmask_bin = 0xFz    (* 4 bits mask *)
let bmask_hex = 0xFFFFz (* 64 bits mask *)

type digits = {
  nbits : int ; (* max number of bits *)
  bsize : int ; (* bits in each bloc *)
  bmask : t ; (* block mask, must be (1 << bsize) - 1 *)
  sep : string ;
  pp : int Pretty_utils.formatter ; (* print one block *)
}

let rec pp_digits d fmt n v =
  if gt v zero || n < d.nbits then
    begin
      let r = to_int (logand v d.bmask) in
      let k = d.bsize in
      pp_digits d fmt Stdlib.(n + k) (shift_right_trunc v k) ;
      if gt v d.bmask || Stdlib.(n + k) < d.nbits
      then Format.pp_print_string fmt d.sep ;
      d.pp fmt r ;
    end

let pp_aux ~is_bin ~bsize ~bmask ~pp_pos ~pp_neg ?(nbits=1) ?(sep="") fmt v =
  let sz, so = if is_bin then "0b", "1b" else "0x", "1x" in
  let nbits = if nbits <= 0 then 1 else nbits in
  if leq zero v then begin
    Format.pp_print_string fmt sz ;
    pp_digits { nbits; sep; bsize; bmask; pp = pp_pos } fmt 0 v
  end
  else begin
    Format.pp_print_string fmt so ;
    pp_digits { nbits; sep; bsize; bmask; pp = pp_neg } fmt 0 (lognot v)
  end

let pp_bin =
  pp_aux ~is_bin:true ~bsize:4 ~bmask:bmask_bin ~pp_pos:pp_bin_pos
    ~pp_neg:pp_bin_neg

let pp_hex =
  pp_aux ~is_bin:false ~bsize:16 ~bmask:bmask_hex ~pp_pos:pp_hex_pos
    ~pp_neg:pp_hex_neg

let pretty_hex fmt v =
  let two_power_60 = two_power_of_int 60 in
  let rec aux v =
    if gt v two_power_60 then
      let quo, rem = ediv_rem v two_power_60 in
      aux quo;
      Format.fprintf fmt "%015LX" (to_int64 rem)
    else
      Format.fprintf fmt "%LX" (to_int64 v)
  in
  if is_zero v then Format.pp_print_string fmt "0"
  else if gt v zero then (Format.pp_print_string fmt "0x"; aux v)
  else (Format.pp_print_string fmt "-0x"; aux (neg v))

let print_big_ints_hex = ref (-1)

let set_big_ints_hex x = print_big_ints_hex := x

let pretty fmt v =
  let pp () = Format.pp_print_string fmt (to_string v) in
  if !print_big_ints_hex < 0 then pp ()
  else
    let max = of_int !print_big_ints_hex in
    if gt (abs v) max then pretty_hex fmt v
    else pp ()

let pp = pp_print

(* ------------- *)
(* Miscellaneous *)
(* ------------- *)

let cast ~size ~signed ~value =
  if (not signed) then
    let factor = two_power size in
    logand value (pred factor)
  else
    let mask = two_power (sub size one) in
    let p_mask = pred mask in
    if is_zero (logand mask value) then
      logand value p_mask
    else
      logor (lognot p_mask) value

let extract_bits ~start ~stop v =
  assert (geq start zero && geq stop start);
  extract v (to_int start) (to_int (length start stop))

(* --------- *)
(* Operators *)
(* --------- *)

(* Operators are at toplevel but we want to be able to have them without
   opening everything, so we create an additional module. *)
module Operators = struct
  include Compare
  let ( ~- ) = ( ~- )
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( mod )  = ( mod )
  let ( land ) = ( land )
  let ( lor )  = ( lor )
  let ( lxor ) = ( lxor )
  let ( ~! )  = ( ~! )
  let ( lsl ) = ( lsl )
  let ( asr ) = ( asr )
  let ( ~$ ) = ( ~$ )
  let ( ** ) b e = pow b e
end

(* We also want relational operators at top level. *)
include Compare

(* -------- *)
(* Datatype *)
(* -------- *)

include Datatype.Make_with_collections (struct
    type nonrec t = t
    let name = "Zarith.Z"
    let reprs = [ zero ]
    let structural_descr = Structural_descr.t_abstract
    let equal = equal
    let compare = compare
    let hash = hash
    let rehash = Datatype.identity
    let copy = Datatype.identity
    let pretty = pretty
    let mem_project = Datatype.never_any_project
  end)
let integer = ty
