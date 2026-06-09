(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Analyses_types
open Analyses_datatype

let is_included i1 i2 = match i1, i2 with
  | Ival i1, Ival i2 -> Ival.is_included i1 i2
  | Float(k1, f1), Float(k2, f2) ->
    Stdlib.compare k1 k2 <= 0
    && (match f1, f2 with
        | None, None | Some _, None -> true
        | None, Some _ -> false
        | Some f1, Some f2 -> f1 = f2)
  | (Ival _ | Float _ | Rational), (Rational | Real)
  | Real, Real
  | Nan, Nan ->
    true
  (* floats and integer are not comparable: *)
  | Ival _, Float _ | Float _, Ival _
  (* nan is comparable to noone, but itself: *)
  | (Ival _ | Float _ | Rational | Real), Nan
  | Nan, (Ival _ | Float _ | Rational | Real)
  (* cases for reals and rationals: *)
  | Real, (Ival _ | Float _ | Rational)
  | Rational, (Ival _ | Float _) ->
    false

let unify = function
  | Ival iv ->
    let min, max = Ival.min_and_max iv in
    Ival (Ival.inject_range min max)
  | Float _ | Rational | Real | Nan as i -> i

let lift_unop f = function
  | Ival iv -> Ival (f iv)
  | Float _ ->
    (* any unary operator over a float generates a rational
       TODO: actually, certainly possible to generate a float *)
    Rational
  | Rational | Real | Nan as i ->
    i

let lift_arith_binop f i1 i2 = match i1, i2 with
  | Ival i1, Ival i2 ->
    Ival (f i1 i2)
  | (Ival _ | Float _), Float _
  | Float _, Ival _
  | (Ival _ | Float _ | Rational), Rational
  | Rational, (Ival _ | Float _) ->
    Rational
  | (Ival _ | Float _ | Rational | Real), Real
  | Real, (Ival _ | Float _ | Rational) ->
    Real
  | (Ival _ | Float _ | Rational | Real | Nan), Nan
  | Nan, (Ival _ | Float _ | Rational | Real) ->
    Nan

let join i1 i2 = match i1, i2 with
  | Ival iv, i when Ival.is_bottom iv -> i
  | i, Ival iv when Ival.is_bottom iv -> i
  | Ival i1, Ival i2 ->
    Ival (Ival.join i1 i2)
  | Float(k1, _), Float(k2, _) ->
    let k = if Cil.frank k1 >= Cil.frank k2 then k1 else k2 in
    Float(k, None (* lost value, if any before *))
  | Ival iv, Float(k, _)
  | Float(k, _), Ival iv ->
    begin
      match Ival.min_and_max iv with
      | None, None ->
        (* unbounded integers *)
        Rational
      | Some min, Some max ->
        (* if the interval of integers fits into the float types, then return
           this float type; otherwise return Rational *)
        (try
           let to_float n = Int64.to_float (Z.to_int64 n) in
           let mini, maxi = to_float min, to_float max in
           let minf, maxf = match k with
             | FFloat  | FFloat32 -> Floating_point.finite_range_of Single
             | FDouble | FFloat64 -> Floating_point.finite_range_of Double
             | FLongDouble -> raise Exit
           in
           if mini >= minf && maxi <= maxf then Float(k, None) else Rational
         with Z.Overflow | Exit ->
           Rational)
      | None, Some _ | Some _, None ->
        assert false
    end
  | (Ival _ | Float _ | Rational), (Float _ | Rational)
  | Rational, Ival _ ->
    Rational
  | (Ival _ | Float _ | Rational | Real), Real
  | Real, (Ival _ | Float _ | Rational) ->
    Real
  | (Ival _ | Float _ | Rational | Real | Nan), Nan
  | Nan, (Ival _ | Float _ | Rational | Real) ->
    Nan

let meet i1 i2 = match i1, i2 with
  | Ival iv, _ when Ival.is_bottom iv -> Ival iv
  | _, Ival iv when Ival.is_bottom iv -> Ival iv
  | Ival i1, Ival i2 ->
    Ival (Ival.meet i1 i2)
  | Float(k1, Some f1), Float(k2, Some f2) ->
    if Float.equal f1 f2 then
      let k = if Cil.frank k1 >= Cil.frank k2 then k2 else k1 in
      Float (k, Some f1)
    else Ival Ival.bottom
  | Float(k, Some f), Float(k', None)
  | Float(k',None), Float(k, Some f) ->
    let f_in_k' = match k' with
      | FFloat | FFloat32 ->
        let minf, maxf = Floating_point.finite_range_of Single in
        minf <= f && f <= maxf
      | FDouble | FFloat64
      | FLongDouble ->
        true
    in if f_in_k' then Float(k, Some f) else Ival Ival.bottom
  | Float(k1, None), Float(k2, None) ->
    let k = if Cil.frank k1 >= Cil.frank k2 then k2 else k1 in
    Float(k, None)
  | Float(k, Some f), Ival iv
  | Ival iv, Float(k, Some f) ->
    begin
      match Ival.min_and_max iv with
      | None, None ->
        (* unbounded integers *)
        Float(k, Some f)
      | Some min, Some max ->
        (* if the float type fits into the interval of integers, then return
           this float type; otherwise return Rational *)
        (try
           let to_float n = Int64.to_float (Z.to_int64 n) in
           let mini, maxi = to_float min, to_float max in
           if mini <= f && maxi >= f then Float(k, Some f) else Ival Ival.bottom
         with Z.Overflow | Exit ->
           Rational)
      | None, Some _ | Some _, None ->
        assert false
    end
  | Ival iv, Float(k, None)
  | Float(k, None), Ival iv ->
    begin
      match Ival.min_and_max iv with
      | None, None ->
        (* unbounded integers *)
        Float(k, None)
      | Some min, Some max ->
        (* if the float type fits into the interval of integers, then return
           this float type; otherwise return Rational *)
        (try
           let to_float n = Int64.to_float (Z.to_int64 n) in
           let mini, maxi = to_float min, to_float max in
           let minf, maxf = match k with
             | FFloat  | FFloat32 -> Floating_point.finite_range_of Single
             | FDouble | FFloat64 -> Floating_point.finite_range_of Double
             | FLongDouble -> raise Exit
           in
           if mini <= minf && maxi >= maxf then Float(k, None) else Rational
         with Z.Overflow | Exit ->
           Rational)
      | None, Some _ | Some _, None ->
        assert false
    end
  | (Ival _ | Float _ | Rational), (Float _ | Rational)
  | Rational, Ival _ ->
    Rational
  | (Ival _ | Float _ | Rational | Real), Real
  | Real, (Ival _ | Float _ | Rational) ->
    Real
  | (Ival _ | Float _ | Rational | Real | Nan), Nan
  | Nan, (Ival _ | Float _ | Rational | Real) ->
    Nan

let () = Logic_env.ival_meet_ref := meet

let is_singleton_int = function
  | Ival iv -> Ival.is_singleton_int iv
  | Float _ | Rational | Real | Nan -> false


(* ********************************************************************* *)
(* constructors and destructors *)
(* ********************************************************************* *)

let extract_ival = function
  | Ival iv -> Some iv
  | _ -> None

let bottom = Ival Ival.bottom
let top_ival = Ival (Ival.inject_range None None)
let singleton n = Ival (Ival.inject_singleton n)
let singleton_of_int n = singleton (Z.of_int n)
let ival min max = Ival (Ival.inject_range (Some min) (Some max))

let interv_of_unknown_block =
  (* since we have no idea of the size of this block, we take the largest
     possible one which is unfortunately quite large *)
  lazy (ival Z.zero (Bit_utils.max_byte_address ()))

let ival_of_ikind ik =
  let n = Cil.bitsSizeOf (Cil_const.mk_tint ik) in
  let l, u =
    if Cil.isSigned ik then Cil.min_signed_number n, Cil.max_signed_number n
    else Z.zero, Cil.max_unsigned_number n
  in
  Ival.inject_range (Some l) (Some u)

(* The boolean indicates whether we have real numbers *)
let rec interv_of_typ ty = match Ast_types.unroll_node ty with
  | TInt k ->
    Ival (ival_of_ikind k)
  | TEnum enuminfo ->
    interv_of_typ (Cil_const.mk_tint enuminfo.ekind)
  | _ when Gmp_types.Z.is_t ty ->
    top_ival
  | TFloat k ->
    Float (k, None)
  | _ when Gmp_types.Q.is_t ty ->
    Rational (* only rationals are implemented *)
  | TVoid | TPtr _ | TArray _ | TFun _ | TComp _ | TBuiltin_va_list ->
    Nan
  | TNamed _ ->
    assert false

let extended_interv_of_typ ty = match interv_of_typ ty with
  | Ival iv ->
    let l,u = Ival.min_int iv, Ival.max_int iv in
    let u = match u with
      | Some u -> Some (Z.succ u)
      | None -> None
    in
    Ival (Ival.inject_range l u);
  | Rational | Real | Nan | Float (_,_) as i
    -> i

let interv_of_logic_typ = function
  | Ctype ty -> interv_of_typ ty
  | Linteger -> top_ival
  | Lreal -> Real
  | Lboolean -> Error.not_yet "boolean"
  | Ltype _ -> Error.not_yet "user-defined logic type"
  | Lvar _ -> Error.not_yet "type variable"
  | Larrow _ -> Nan

exception Not_representable_ival
let ikind_of_ival iv =
  if Ival.is_bottom iv then IInt
  else match Ival.min_and_max iv with
    | Some l, Some u ->
      begin
        try
          let is_pos = Z.geq l Z.zero in
          let lkind = Cil.intKindForValue l is_pos in
          let ukind = Cil.intKindForValue u is_pos in
          (* kind corresponding to the interval *)
          let kind = if Cil.intTypeIncluded lkind ukind then ukind else lkind in
          (* convert the kind to [IInt] whenever smaller. *)
          if Cil.intTypeIncluded kind IInt then IInt else kind
        with Cil.Not_representable ->
          raise Not_representable_ival
      end
    | None, None -> raise Not_representable_ival (* GMP *)
    (* TODO: do not raise an exception, but returns a value instead *)
    | None, Some _ | Some _, None ->
      (* Semi-open interval that can happen when computing the interval of shift
         operations if the computation overflows *)
      (* TODO: do not raise an exception, but returns a value instead *)
      raise Not_representable_ival (* GMP *)
