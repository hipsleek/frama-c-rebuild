(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang
open Tactical
open Conditions

module L = Qed.Logic

(* -------------------------------------------------------------------------- *)
(* --- Havoc                                                              --- *)
(* -------------------------------------------------------------------------- *)

let lookup_havoc e =
  match F.repr e with
  | L.Aget( m , p ) ->
    begin
      match F.repr m with
      | L.Fun( f , [m_sep;a;m_undef;b;n] )
        when f == MemMemory.f_memcpy ->
        if F.equal a b then Some( m_sep , m_undef , a , n , p )
        else None
      | _ -> None
    end
  | _ -> None

class havoc =
  object
    inherit Tactical.make ~id:"Wp.havoc"
        ~title:"Havoc"
        ~descr:"Go through assigns."
        ~params:[]

    method select _feedback sel =
      let at = Tactical.at sel in
      let e = Tactical.selected sel in
      match lookup_havoc e with
      | None -> Not_applicable
      | Some(m0,mr,a,n,p) ->
        let separated =
          F.p_call MemAddr.p_separated
            [ p ; F.e_int 1 ; a ; n ] in
        let process = Tactical.rewrite ?at [
            "Unassigned" , separated , e , F.e_get m0 p ;
            "Assigned" , F.p_not separated , e , F.e_get mr p  ;
          ] in
        Applicable process
  end

(* -------------------------------------------------------------------------- *)
(* --- Separated                                                          --- *)
(* -------------------------------------------------------------------------- *)

let separated ?at property =
  match F.e_expr property with
  | L.Fun( f , [p;n;q;m] ) when f == MemAddr.p_separated ->
    let base_p = MemAddr.base p in
    let ofs_p = MemAddr.offset p in
    let base_q = MemAddr.base q in
    let ofs_q = MemAddr.offset q in
    let eq_base = F.p_equal base_p base_q in
    let on_left = F.p_leq (F.e_add ofs_p n) ofs_q in
    let on_right = F.p_leq (F.e_add ofs_q m) ofs_p in
    let overlap = F.p_not (F.p_and on_left on_right) in
    let pattern = F.e_prop property in
    let cases =
      [ "WrongBase" , F.p_neq base_p base_q , pattern , F.e_true ;
        "OnLeft" , F.p_and eq_base on_left , pattern , F.e_true ;
        "OnRight" , F.p_and eq_base on_right , pattern , F.e_true ;
        "OverLap" , F.p_and eq_base overlap , pattern , F.e_false ]
    in
    Applicable (Tactical.rewrite ?at cases)
  | _ -> Not_applicable

class separated =
  object
    inherit Tactical.make ~id:"Wp.separated"
        ~title:"Separated"
        ~descr:"Split over separated and overlapping cases."
        ~params:[]

    method select _feedback sel =
      match sel with
      | Clause (Goal p) -> separated p
      | Clause (Step s) -> separated ~at:s.id (Conditions.head s)
      | Inside (_,p) when F.is_prop p ->
        separated ?at:(Tactical.at sel) (F.p_bool p)
      | _ -> Not_applicable
  end

(* -------------------------------------------------------------------------- *)
(* --- Included, Validity, Invalidity                                     --- *)
(* -------------------------------------------------------------------------- *)

let invalid m p n =
  let base = MemAddr.base p in
  let offset = MemAddr.offset p in
  let malloc = F.e_get m base in
  "Invalid",
  F.p_imply
    (F.p_lt F.e_zero n)
    (F.p_or
       (F.p_leq malloc offset)
       (F.p_leq (F.e_add offset n) F.e_zero))

let valid_rd m p n =
  let base = MemAddr.base p in
  let offset = MemAddr.offset p in
  let malloc = F.e_get m base in
  "Valid (Read)",
  F.p_imply
    (F.p_lt F.e_zero n)
    (F.p_and
       (F.p_leq F.e_zero offset)
       (F.p_leq (F.e_add offset n) malloc))

let valid_rw m p n =
  let base = MemAddr.base p in
  let offset = MemAddr.offset p in
  let malloc = F.e_get m base in
  "Valid (Read & Write)",
  F.p_imply
    (F.p_lt F.e_zero n)
    (F.p_conj [
        F.p_lt F.e_zero base ;
        F.p_leq F.e_zero offset ;
        F.p_leq (F.e_add offset n) malloc ;
      ])

let included p a q b =
  let p_base = MemAddr.base p in
  let q_base = MemAddr.base q in
  let p_offset = MemAddr.offset p in
  let q_offset = MemAddr.offset q in
  "Included",
  F.p_imply
    (F.p_lt F.e_zero a)
    (F.p_imply
       (F.p_leq F.e_zero b)
       (F.p_conj [
           F.p_equal p_base q_base ;
           F.p_leq q_offset p_offset ;
           F.p_leq (F.e_add p_offset a) (F.e_add q_offset b) ;
         ]))

let lookup f = function
  | [p;a;q;b] when f == MemAddr.p_included -> included p a q b
  | [m;p;n] when MemAddr.is_p_invalid f -> invalid m p n
  | [m;p;n] when MemAddr.is_p_valid_rd f -> valid_rd m p n
  | [m;p;n] when MemAddr.is_p_valid_rw f -> valid_rw m p n
  | _ -> raise Not_found

let unfold ?at e f es =
  let descr,q = lookup f es in
  Applicable (Tactical.rewrite ?at [descr,F.p_true,e,F.e_prop q])

class validity =
  object

    inherit Tactical.make ~id:"Wp.valid"
        ~title:"Validity Range"
        ~descr:"Unfold validity and range definitions."
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      let at = Tactical.at s in
      let e = Tactical.selected s in
      match F.repr e with
      | Qed.Logic.Fun(f,es) -> unfold ?at e f es
      | _ -> Not_applicable

  end


(* -------------------------------------------------------------------------- *)
(* --- Exported API                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Havoc =
struct
  let tactical = Tactical.export (new havoc)
  let strategy ?(priority=1.0) havoc =
    Strategy.{
      priority ;
      tactical ;
      selection = havoc ;
      arguments = [] ;
    }
end

module Separated =
struct
  let tactical = Tactical.export (new separated)
  let strategy = Strategy.make tactical ~arguments:[]
end

module Validity =
struct
  let tactical = Tactical.export (new validity)
  let strategy = Strategy.make tactical ~arguments:[]
end

(* -------------------------------------------------------------------------- *)
