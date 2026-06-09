(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang
open Lang.F
open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Range Tactical                                                     --- *)
(* -------------------------------------------------------------------------- *)

let rec range pool e a b =
  if a <= b then
    let descr = Printf.sprintf "Value %d" b in
    let cond = p_equal e (e_int b) in
    range ((descr,cond)::pool) e a (pred b)
  else pool

let enum ?at e a b sequent =
  let lower = Printf.sprintf "Lower %d" a , p_lt e (e_int a) in
  let upper = Printf.sprintf "Upper %d" b , p_lt (e_int b) e in
  let cases = lower :: range [upper] e a b in
  Tactical.insert ?at cases sequent

let vmin,pmin = Tactical.spinner ~id:"inf"
    ~title:"Inf" ~descr:"Range lower bound (inclusive)." ()
let vmax,pmax = Tactical.spinner ~id:"sup"
    ~title:"Sup" ~descr:"Range upper bound (inclusive)." ()

class range =
  object(self)
    inherit Tactical.make ~id:"Wp.range"
        ~title:"Range"
        ~descr:"Enumerate a range of values for an integer term."
        ~params:[pmin;pmax]

    method select feedback (s : Tactical.selection) =
      let at = Tactical.at s in
      let e = Tactical.selected s in
      if F.is_int e then
        begin
          let a = self#get_field vmin in
          let b = self#get_field vmax in
          if not (a <= b) then
            ( feedback#set_error "Empty Range (shall have Inf <= Sup)" ;
              Not_configured )
          else
            ( feedback#set_title "Range (%d-%d)" a b ;
              feedback#set_descr "Enumerate lower, range %d-%d and upper." a b ;
              Applicable(enum ?at e a b) )
        end
      else
        Not_applicable

  end

let tactical = Tactical.export (new range)
let strategy ?(priority=1.0) selection ~vmin:a ~vmax:b =
  Strategy.{
    priority ; tactical ; selection ;
    arguments = [ arg vmin a ; arg vmax b ] ;
  }
