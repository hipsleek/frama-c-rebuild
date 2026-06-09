(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Dive_types

type t = node_range

let fkind_limits =
  let max_single = float_of_string "0x1.fffffep+127"
  and max_double = float_of_string "0x1.fffffffffffffp+1023" in
  function
  | Cil_types.FFloat | FFloat32 -> max_single
  | FDouble | FFloat64 | FLongDouble -> max_double

let ikind_limits ikind =
  let open Cil in
  let bits = bitsSizeOfInt ikind in
  if isSigned ikind then
    (min_signed_number bits, max_signed_number bits)
  else
    (Z.zero, max_unsigned_number bits)

let logscale x limit =
  Float.(to_int (min 100. (100. *. log (max x 1.) /. log limit)))

let log2scale x limit =
  assert (x > 0.);
  assert (limit > 0.);
  Float.(logscale (log x) (log limit))

let cardinal_range cardinal limit =
  let s = log2scale (Z.to_float cardinal) (Z.to_float limit) in
  if s > 98 then Wide else Normal s

let integer_range cardinal ikind =
  let _, limit = ikind_limits ikind in
  cardinal_range cardinal limit

let float_range fkind l u =
  let limit = fkind_limits fkind
  and l = Fval.F.to_float l
  and u = Fval.F.to_float u in
  let range =
    if (l < 0.0) = (u < 0.0) then (* if bounds have same sign *)
      u -. l
    else
      Float.(max (abs u) (abs l))
  in
  let s = log2scale range limit in
  if s > 98 then Wide else Normal s

let evaluate cvalue typ =
  let cardinal = Cvalue.V.cardinal cvalue in
  match typ.Cil_types.tnode, cardinal with
  | _, Some card when Z.is_zero card -> Empty
  | _, Some card when Z.is_one card -> Singleton
  | TInt ikind, Some cardinal -> integer_range cardinal ikind
  | TFloat fkind, _ ->
    begin match Ival.min_and_max_float (Cvalue.V.project_ival cvalue) with
      | Some (l, u), _can_be_nan -> float_range fkind l u
      | _, _ -> Wide
      | exception Cvalue.V.Not_based_on_null -> Wide
      | exception Abstract_interp.Error_Bottom -> Empty
    end
  | _, Some cardinal -> cardinal_range cardinal 100z (* arbitrary limit for pointers *)
  | _, None -> Wide

let upper_bound r1 r2 =
  match r1, r2 with
  | Empty, r | r, Empty -> r
  | Singleton, r | r, Singleton -> r
  | Normal i1, Normal i2 -> Normal (max i1 i2)
  | Wide, _ | _, Wide -> Wide
