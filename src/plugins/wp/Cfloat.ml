(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Floats Arithmetic Model                                            --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Qed
open Lang
open Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Library                                                            --- *)
(* -------------------------------------------------------------------------- *)

let library = "cfloat"

let f32 = datatype ~library "f32"
let f64 = datatype ~library "f64"

let t32 = Lang.(t_datatype f32 [])
let t64 = Lang.(t_datatype f64 [])

let ftau = function
  | Float32 -> t32
  | Float64 -> t64

let ft_suffix = function Float32 -> "f32" | Float64 -> "f64"
let pp_suffix fmt ft = Format.pp_print_string fmt (ft_suffix ft)

let link phi = Qed.Engine.F_call phi

(* Qed exact representations, linked to f32/f64 *)
let fq32 = extern_f ~library ~result:t32 ~link:(link "to_f32") "q32"
let fq64 = extern_f ~library ~result:t64 ~link:(link "to_f64") "q64"

let f_model ft = extern_f ~library ~result:(ftau ft) "model_%a" pp_suffix ft
let f_delta ft = extern_f ~library ~result:(ftau ft) "delta_%a" pp_suffix ft
let f_epsilon ft = extern_f ~library ~result:(ftau ft) "epsilon_%a" pp_suffix ft
let f_sqrt ft = extern_f ~library ~result:(ftau ft) "sqrt_%a" pp_suffix ft

(* -------------------------------------------------------------------------- *)
(* --- Model Setting                                                      --- *)
(* -------------------------------------------------------------------------- *)

type model = Real | Float

let model = Context.create ~default:Float "Cfloat.model"

let tau_of_float f =
  match Context.get model with
  | Real -> Logic.Real
  | Float -> ftau f

let float_name = function
  | Float32 -> "float"
  | Float64 -> "double"

let model_name = function
  | Float -> "Float"
  | Real  -> "Real"

(* -------------------------------------------------------------------------- *)
(* --- Operators                                                          --- *)
(* -------------------------------------------------------------------------- *)

type op =
  | LT
  | EQ
  | LE
  | NE
  | NEG
  | ADD
  | SUB
  | MUL
  | DIV
  | REAL
  | ROUND
  | EXACT

[@@@ warning "-32"]
let op_name = function
  | LT -> "lt"
  | EQ -> "eq"
  | LE -> "le"
  | NE -> "ne"
  | NEG -> "neg"
  | ADD -> "add"
  | SUB -> "sub"
  | MUL -> "mul"
  | DIV -> "div"
  | REAL -> "of"
  | ROUND -> "to"
  | EXACT -> "exact"
[@@@ warning "+32"]

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

module REGISTRY = WpContext.Static
    (struct
      type key = lfun
      type data = op * c_float
      let name = "Wp.Cfloat.REGISTRY"
      include Lang.Fun
    end)


let find = REGISTRY.find

let () = Context.register
    begin fun () ->
      REGISTRY.define fq32 (EXACT,Float32) ;
      REGISTRY.define fq64 (EXACT,Float64) ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Literals                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rfloat = Floating_point.round_to_single_precision

let fmake ulp value = match ulp with
  | Float32 -> F.e_fun ~result:t32 fq32 [F.e_float (rfloat value)]
  | Float64 -> F.e_fun ~result:t64 fq64 [F.e_float value]

let qmake ulp q = fmake ulp (Q.to_float q)

let re_float = Str.regexp ".*[fldFLD]$"
let re_mantissa = Str.regexp "[-+]?[0-9]*"
let re_decimal = Str.regexp "\\.[0-9]*"
let re_exponent = Str.regexp "[eE][-+]?[0-9]*"

let parse_literal ~model v r =
  try
    if model = Float && Str.string_match re_float r 0 then Q.of_float v
    else
      begin
        if not @@ Str.string_match re_mantissa r 0 then
          raise Not_found ;
        let mantissa = Str.matched_string r in
        let p = Str.match_end () in (* length mantissa = p *)
        let q, decimal =
          if Str.string_match re_decimal r p then
            let q = Str.match_end () in
            q, String.sub r (p + 1) (q - p - 1)
          else
            p, "" in
        let n, exponent =
          if Str.string_match re_exponent r q then
            let n = Str.match_end () in
            n, String.sub r (q + 1) (n - q - 1)
          else
            q, "0" in
        if n <> String.length r && not @@ Str.string_match re_float r n then
          raise Not_found ;
        let digits = Z.of_string @@ mantissa ^ decimal in
        let decades = int_of_string exponent - String.length decimal in
        let power n =
          let s = Bytes.make (n+1) '0' in Bytes.set s 0 '1' ;
          Z.of_string (String.of_bytes s) in
        if decades < 0 then
          Q.make digits @@ power (-decades)
        else
        if decades > 0 then
          Q.of_bigint (Z.mul digits @@ power decades)
        else
          Q.of_bigint digits
      end
  with Failure _ | Not_found | Invalid_argument _ ->
    Warning.error "Unexpected constant literal %S" r

let acsl_lit l =
  let open Cil_types in
  F.e_real (parse_literal ~model:(Context.get model) l.r_nearest l.r_literal)

let code_lit ulp value original =
  match Context.get model , ulp , original with
  | Float , Float32 , _ -> F.e_fun ~result:t32 fq32 [F.e_float value]
  | Float , Float64 , _ -> F.e_fun ~result:t64 fq64 [F.e_float value]
  | Real , _ , None -> F.e_float value
  | Real , _ , Some r -> F.e_real (parse_literal ~model:Real value r)

(* -------------------------------------------------------------------------- *)
(* --- Literal Output                                                     --- *)
(* -------------------------------------------------------------------------- *)

let printers = [
  Printf.sprintf "%.0g" ;
  Printf.sprintf "%.1g" ;
  Printf.sprintf "%.2g" ;
  Printf.sprintf "%.3g" ;
  Printf.sprintf "%.4g" ;
  Printf.sprintf "%.5g" ;
  Printf.sprintf "%.6g" ;
  Printf.sprintf "%.9g" ;
  Printf.sprintf "%.12g" ;
  Printf.sprintf "%.15g" ;
  Printf.sprintf "%.18g" ;
  Printf.sprintf "%.21g" ;
  Printf.sprintf "%.32g" ;
  Printf.sprintf "%.64g" ;
]

let re_int_float = Str.regexp "\\(-?[0-9]+\\)\\(e[+-]?[0-9]+\\)?$"

let force_float r =
  if Str.string_match re_int_float r 0
  then
    let group n r = try Str.matched_group n r with Not_found -> ""
    in group 1 r ^ ".0" ^ group 2 r
  else r

let float_lit ulp (q : Q.t) =
  let v = match ulp with
    | Float32 -> rfloat @@ Q.to_float q
    | Float64 -> Q.to_float q in
  let reparse ulp r =
    match ulp with
    | Float32 -> rfloat @@ float_of_string r
    | Float64 -> float_of_string r
  in
  let rec lookup ulp v = function
    | [] -> Pretty_utils.to_string Floating_point.pretty v
    | pp::pps ->
      let r = force_float @@ pp v in
      if reparse ulp r = v then r else
        lookup ulp v pps
  in lookup ulp v printers

(* -------------------------------------------------------------------------- *)
(* --- Finites                                                            --- *)
(* -------------------------------------------------------------------------- *)

let fclass value _args =
  match Context.get model with
  | Real -> F.e_bool value
  | Float -> raise Not_found

let () = Context.register
    begin fun () ->
      LogicBuiltins.hack "\\is_finite"         (fclass true) ;
      LogicBuiltins.hack "\\is_NaN"            (fclass false) ;
      LogicBuiltins.hack "\\is_infinite"       (fclass false) ;
      LogicBuiltins.hack "\\is_plus_infinity"  (fclass false) ;
      LogicBuiltins.hack "\\is_minus_infinity" (fclass false) ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Computations                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rec exact e =
  match F.repr e with
  | Qed.Logic.Kreal r -> r
  | Qed.Logic.Kint z -> Q.of_bigint z
  | Qed.Logic.Fun( f , [ q ] ) when f == fq32 || f == fq64 -> exact q
  | _ -> raise Not_found

let round ulp e =
  match F.repr e with
  | Qed.Logic.Fun( f , [ b ] ) ->
    begin
      match find f with
      | REAL , ulp2 when ulp2 = ulp -> b
      | _ -> qmake ulp (exact e )
    end
  | _ -> qmake ulp (exact e)

let compute_float op ulp xs =
  match op , xs with
  | NEG , [ x ] -> qmake ulp (Q.neg (exact x))
  | ADD , [ x ; y ] -> qmake ulp (Q.add (exact x) (exact y))
  | SUB , [ x ; y ] -> qmake ulp (Q.sub (exact x) (exact y))
  | MUL , [ x ; y ] -> qmake ulp (Q.mul (exact x) (exact y))
  | DIV , [ x ; y ] ->
    let res = match Q.div (exact x) (exact y) with
      | x when Q.classify x = Q.NZERO -> x
      | _ -> raise Not_found (* let Why3 take care of the division*)
    in qmake ulp res
  | ROUND , [ x ] -> round ulp x
  | REAL , [ x ] -> F.e_real (exact x)
  | LE , [ x ; y ] -> F.e_bool (Q.leq (exact x) (exact y))
  | LT , [ x ; y ] -> F.e_bool (Q.lt (exact x) (exact y))
  | EQ , [ x ; y ] -> F.e_bool (Q.equal (exact x) (exact y))
  | NE , [ x ; y ] -> F.e_bool (not (Q.equal (exact x) (exact y)))
  | _ -> raise Not_found

let compute_real op xs =
  match op , xs with
  | NEG , [ x ] -> F.e_opp x
  | ADD , [ x ; y ] -> F.e_add x y
  | SUB , [ x ; y ] -> F.e_sub x y
  | MUL , [ x ; y ] -> F.e_mul x y
  | DIV , [ x ; y ] -> F.e_div x y
  | (ROUND|REAL) , [ x ] -> x
  | LE , [ x ; y ] -> F.e_leq x y
  | LT , [ x ; y ] -> F.e_lt x y
  | EQ , [ x ; y ] -> F.e_eq x y
  | NE , [ x ; y ] -> F.e_neq x y
  | _ -> raise Not_found

let return_type ft = function
  | REAL -> Logic.Real
  | _ -> ftau ft

module Compute = WpContext.StaticGenerator
    (struct
      type t = model * c_float * op

      let compare = Stdlib.compare

      let pretty fmt (m, ft, op) =
        Format.fprintf fmt "%s_%a_%s" (model_name m) pp_suffix ft (op_name op)
    end)
    (struct
      let name = "Wp.Cfloat.Compute"
      type key = model * c_float * op
      type data = lfun * (term list -> term)

      let compile (m, ft, op) =
        let impl = match m with
          | Real  -> compute_real op
          | Float -> compute_float op ft
        in
        let name = op_name op in
        let phi = match op with
          | LT | EQ | LE | NE ->
            let prop = Format.asprintf "%s_%a" name pp_suffix ft in
            let bool = Format.asprintf "%s_%ab" name pp_suffix ft in
            extern_p ~library ~bool ~prop ()
          | _ ->
            let result = return_type ft op in
            extern_f ~library ~result "%s_%a" name pp_suffix ft
        in
        Lang.F.set_builtin phi impl ;
        REGISTRY.define phi (op, ft) ;
        (phi, impl)
    end)

(* -------------------------------------------------------------------------- *)
(* --- Operations                                                         --- *)
(* -------------------------------------------------------------------------- *)

let flt_eq ft = Compute.get (Context.get model, ft, EQ) |> fst
let flt_neq ft = Compute.get (Context.get model, ft, NE) |> fst
let flt_le ft = Compute.get (Context.get model, ft, LE) |> fst
let flt_lt ft = Compute.get (Context.get model, ft, LT) |> fst
let flt_neg ft = Compute.get (Context.get model, ft, NEG) |> fst
let flt_add ft = Compute.get (Context.get model, ft, ADD) |> fst
let flt_sub ft = Compute.get (Context.get model, ft, SUB) |> fst
let flt_mul ft = Compute.get (Context.get model, ft, MUL) |> fst
let flt_div ft = Compute.get (Context.get model, ft, DIV) |> fst
let flt_of_real ft = Compute.get (Context.get model, ft, ROUND) |> fst
let real_of_flt ft = Compute.get (Context.get model, ft, REAL) |> fst

(* -------------------------------------------------------------------------- *)
(* --- Builtins                                                           --- *)
(* -------------------------------------------------------------------------- *)

let builtin kind ft op xs =
  let phi, impl = Compute.get ((Context.get model), ft, op) in
  let xs = (if kind=`ReV then List.rev xs else xs) in
  try impl xs with Not_found ->
    let result = match kind with
      | `Binop | `Unop -> ftau ft
      | `Rel | `ReV -> Logic.Bool
    in F.e_fun ~result phi xs

let register_builtins ft =
  begin
    let suffix = float_name ft in
    let register (prefix,kind,op) =
      LogicBuiltins.hack
        (Printf.sprintf "\\%s_%s" prefix suffix)
        (builtin kind ft op)
    in List.iter register [
      "eq",`Rel,EQ ;
      "ne",`Rel,NE ;
      "lt",`Rel,LT ;
      "gt",`ReV,LT ;
      "le",`Rel,LE ;
      "ge",`ReV,LE ;
      "neg",`Unop,NEG ;
      "add",`Binop,ADD ;
      "sub",`Binop,SUB ;
      "mul",`Binop,MUL ;
      "div",`Binop,DIV ;
    ] ;
  end

let () = Context.register
    begin fun () ->
      register_builtins Float32 ;
      register_builtins Float64 ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Models                                                             --- *)
(* -------------------------------------------------------------------------- *)

let hack_sqrt_builtin ft =
  let choose xs =
    match Context.get model with
    | Real -> F.e_fun ~result:t_real Cmath.f_sqrt xs
    | Float -> F.e_fun ~result:(ftau ft) (f_sqrt ft) xs
  in
  let name = match ft with
    | Float32 -> "sqrtf"
    | Float64 -> "sqrt"
  in
  LogicBuiltins.hack name choose

let () =
  let open LogicBuiltins in
  let register_builtin ft =
    add_builtin "\\model" [F ft] (f_model ft) ;
    add_builtin "\\delta" [F ft] (f_delta ft) ;
    add_builtin "\\epsilon" [F ft] (f_epsilon ft) ;
    hack_sqrt_builtin ft
  in
  register_builtin Float32 ;
  register_builtin Float64

(* -------------------------------------------------------------------------- *)
(* --- Conversion Symbols                                                 --- *)
(* -------------------------------------------------------------------------- *)

let real_of_float f a =
  match Context.get model with
  | Real -> a
  | Float -> e_fun ~result:Logic.Real (real_of_flt f) [a]

let float_of_real f a =
  match Context.get model with
  | Real -> a
  | Float -> e_fun ~result:(ftau f) (flt_of_real f) [a]

let float_of_int f a = float_of_real f (Cmath.real_of_int a)

(* -------------------------------------------------------------------------- *)
(* --- Float Arithmetics                                                  --- *)
(* -------------------------------------------------------------------------- *)

let fbinop rop fop f x y =
  match Context.get model with
  | Real -> rop x y
  | Float -> e_fun ~result:(ftau f) (fop f) [x;y]

let fcmp rop fop f x y =
  match Context.get model with
  | Real -> rop x y
  | Float -> p_call (fop f) [x;y]

let fadd = fbinop e_add flt_add
let fsub = fbinop e_sub flt_sub
let fmul = fbinop e_mul flt_mul
let fdiv = fbinop e_div flt_div
let fopp f x =
  match Context.get model with
  | Real -> e_opp x
  | Float -> e_fun ~result:(ftau f) (flt_neg f) [x]

let flt = fcmp p_lt flt_lt
let fle = fcmp p_leq flt_le
let feq = fcmp p_equal flt_eq
let fneq = fcmp p_neq flt_neq

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

let configure m =
  begin
    let orig_model = Context.push model m in
    let orig_floats = Context.push Lang.floats tau_of_float in
    let rollback () =
      Context.pop model orig_model ;
      Context.pop Lang.floats orig_floats
    in
    rollback
  end

(* -------------------------------------------------------------------------- *)
