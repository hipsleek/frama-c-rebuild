(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang
open Qed.Logic

(* Only integer patterns *)
type pattern =
  | IMUL_K of Z.t * F.term
  | IDIV_K of F.term * Z.t
  | QDIV of F.term * F.term
  | Ival of F.term * Z.t option
  | Rval of F.term

let pattern e =
  match F.repr e with
  | Kint n -> Ival(e,Some n)
  | Times(k,e) when F.is_int e -> IMUL_K(k,e)
  | Div(a,b) when not (F.is_int e) -> QDIV(a,b)
  | Div(a,b) when F.is_int e ->
    begin match F.repr b with
      | Kint k ->
        if Z.(equal k zero) then raise Not_found ;
        IDIV_K(a,k)
      | _ -> Ival(e,None)
    end
  | _ ->
    if F.is_int e then Ival(e,None) else
    if F.is_real e then Rval e else
      raise Not_found
(*
let pp_pattern fmt = function
  | Ival(_,Some z) -> Format.fprintf fmt "(%s : constant)" (Z.to_string z)
  | Ival(e,None) -> Format.fprintf fmt "@[<hov 2>(%a : int)@]" F.pp_term e
  | Rval e -> Format.fprintf fmt "@[<hov 2>(%a : real)@]" F.pp_term e
  | IMUL_K(k,e) -> Format.fprintf fmt "@[<hov 2>%s.(%a : int)@]" (Z.to_string k) F.pp_term e
  | IDIV_K(e,k) -> Format.fprintf fmt "@[<hov 2>(%a : int)/%s@]" F.pp_term e (Z.to_string k)
  | QDIV(a,b) -> Format.fprintf fmt "@[<hov 2>(%a : real)@,/(%a : real)@]" F.pp_term a F.pp_term b
*)

let to_term = function
  | IMUL_K(k,a) -> F.e_times k a
  | IDIV_K(a,k) -> F.e_div a (F.e_zint k)
  | QDIV(a,b) -> F.e_div a b
  | Ival(e,_) | Rval e -> e

let pdiv a b = let k = Z.div a b in Ival(F.e_zint k,Some k)

let nzero x = F.p_neq F.e_zero x
let positive x = F.p_lt F.e_zero x
let negative x = F.p_lt x F.e_zero

type cmp = LEQ | LT | EQ

let icmp cmp a b = match cmp with
  | LEQ -> Z.leq a b
  | LT -> Z.lt a b
  | EQ -> Z.equal a b

let fcmp cmp a b = match cmp with
  | LEQ -> F.p_leq a b
  | LT -> F.p_lt a b
  | EQ -> F.p_equal a b

let compare_ratio cmp a u b v =
  let x = F.e_mul a v in
  let y = F.e_mul b v in
  let pu = positive u in
  let nu = negative u in
  let pv = positive v in
  let nv = negative v in
  F.p_conj [ nzero u ; nzero v ;
             F.p_hyps [pu;pv] (fcmp cmp x y) ;
             F.p_hyps [nu;pv] (fcmp cmp y x) ;
             F.p_hyps [pu;nv] (fcmp cmp y x) ;
             F.p_hyps [nu;nv] (fcmp cmp x y) ]

let compare_div cmp a b g =
  let ra = F.e_mod a g in
  let rb = F.e_mod b g in
  fcmp cmp (F.e_sub a ra) (F.e_sub b rb)

let rec compare cmp a b =
  match a, b with
  | IMUL_K( k,a ) , Ival(_,Some n) ->
    if Z.(lt zero k) then compare cmp (pattern a) (pdiv n k) else
    if Z.(lt k zero) then compare cmp (pdiv n k) (pattern a) else
    if icmp cmp Z.zero n then F.p_true else F.p_false
  | Ival(_,Some n) , IMUL_K( k,a ) ->
    if Z.(lt zero k) then compare cmp (pdiv n k) (pattern a) else
    if Z.(lt k zero) then compare cmp (pattern a) (pdiv n k) else
    if icmp cmp Z.zero n then F.p_true else F.p_false
  | IDIV_K( a,k ) , Ival(b,_) ->
    if Z.(lt zero k) then
      let c = F.e_times k (F.e_add b F.e_one) in
      fcmp cmp a c
    else
    if Z.(lt k zero) then
      let c = F.e_times k (F.e_sub b F.e_one) in
      fcmp cmp c a
    else
      raise Not_found
  | Ival(a,_) , IDIV_K( b,k ) ->
    if Z.(lt zero k) then
      let c = F.e_times k (F.e_sub a F.e_one) in
      fcmp cmp c b
    else
    if Z.(lt k zero) then
      let c = F.e_times k (F.e_add a F.e_one) in
      fcmp cmp b c
    else
      raise Not_found
  | IDIV_K( a,p ) , IDIV_K( b,q ) when
      not Z.(equal p zero) &&
      not Z.(equal q zero) ->
    let g = Z.gcd (Z.abs p) (Z.abs q) in
    let ka = Z.ediv p g in
    let kb = Z.ediv q g in
    compare_div cmp (F.e_times ka a) (F.e_times kb b) (F.e_zint g)

  | QDIV(a,u) , QDIV(b,v) -> compare_ratio cmp a u b v
  | QDIV(a,u) , (Ival(b,_) | Rval b) -> compare_ratio cmp a u b F.e_one
  | (Ival(a,_) | Rval a) , QDIV(b,v) -> compare_ratio cmp a F.e_one b v
  | _ ->
    raise Not_found

let eq_ratio eq a u b v =
  F.p_conj [ nzero u ; nzero v ; eq (F.e_mul a v) (F.e_mul b u) ]

let rec equal eq a b =
  match a , b with
  | IMUL_K( k,a ) , Ival(_,Some n)
  | Ival(_,Some n) , IMUL_K( k,a ) ->
    let r = Z.rem k n in
    if Z.is_zero r then
      equal eq (pattern a) (pdiv n k)
    else
      eq F.e_one F.e_zero
  | IMUL_K( k,a ) , IMUL_K( k',b ) ->
    let r = Z.gcd k k' in
    eq (F.e_times (Z.div k r) a)
      (F.e_times (Z.div k' r) b)

  | IDIV_K( a,p ) , IDIV_K( b,q ) when
      not Z.(equal p zero) &&
      not Z.(equal q zero) ->
    let g = Z.gcd (Z.abs p) (Z.abs q) in
    let ka = Z.ediv p g in
    let kb = Z.ediv q g in
    compare_div EQ (F.e_times ka a) (F.e_times kb b) (F.e_zint g)

  | QDIV(a,u) , QDIV(b,v) -> eq_ratio eq a u b v
  | QDIV(a,u) , (Ival(b,_) | Rval b) -> eq_ratio eq a u b F.e_one
  | (Ival(a,_) | Rval a) , QDIV(b,v) -> eq_ratio eq a F.e_one b v
  | _ -> eq (to_term a) (to_term b)

let select goal =
  match F.repr (F.e_prop goal) with
  | Leq(a,b) -> compare LEQ (pattern a) (pattern b)
  | Lt(a,b) -> compare LT (pattern a) (pattern b)
  | Eq(a,b) -> equal F.p_equal (pattern a) (pattern b)
  | Neq(a,b) -> equal F.p_neq (pattern a) (pattern b)
  | _ -> raise Not_found

class congruence =
  object
    inherit Tactical.make
        ~id:"Wp.congruence"
        ~title:"Congruence"
        ~descr:"Resolve congruences with euclidian divisions."
        ~params:[]

    method select _feedback = function
      | Tactical.Clause(Tactical.Goal p) ->
        let q = select p in
        if q != p
        then Tactical.Applicable(fun seq -> ["congruence" , (fst seq , q)])
        else Tactical.Not_applicable
      | _ -> Tactical.Not_applicable

  end

let tactical = Tactical.export (new congruence)
let strategy = Strategy.make tactical ~arguments:[]

(* -------------------------------------------------------------------------- *)
(* --- Auto Congruence                                                    --- *)
(* -------------------------------------------------------------------------- *)

class autodiv =
  object

    method id = "wp:congruence"
    method title = "Auto Congruence"
    method descr = "Resolve divisions and multiplications."
    method search push (seq : Conditions.sequent) =
      try
        let p = snd seq in
        let q = select p in
        if q != p then push (strategy Tactical.(Clause (Goal p)))
      with Not_found -> ()

  end

let () = Strategy.register (new autodiv)
