(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang
open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Compute Term                                                       --- *)
(* -------------------------------------------------------------------------- *)

exception Split of F.term * F.term * F.term
exception Unfold of F.term * Lang.lfun * F.term

let head e =
  if F.lc_closed e && not @@ F.is_primitive e then
    match F.repr e with
    | If(c,a,b) -> raise (Split(c,a,b))
    | Fun(f,es) -> raise (Unfold(e,f,TacUnfold.unfold f es))
    | _ -> ()

let process (fd : Tactical.feedback) ?at e =
  try
    let q = Queue.create () in
    let m = ref F.Tset.empty in
    let lookup e =
      head e ;
      m := F.Tset.add e !m ;
      F.lc_iter (fun e -> Queue.push e q) e
    in
    lookup e ;
    while not @@ Queue.is_empty q do
      let e = Queue.pop q in
      if not @@ F.Tset.mem e !m then lookup e ;
    done ;
    Not_applicable
  with
  | Split(a,b,c) ->
    let p = F.p_bool c in
    let q = F.p_not p in
    fd#set_title "Compute (split)" ;
    fd#set_descr "Split on if-then-else condition." ;
    Applicable (Tactical.rewrite ?at ["If-Then",p,e,a;"If-Else",q,e,b])
  | Unfold(e,f,d) ->
    fd#set_title "Compute (def)" ;
    fd#set_descr "Unfold definition of '%s'." (Lang.name_of_lfun f) ;
    Applicable (Tactical.rewrite ?at ["Definition",F.p_true,e,d])

class compute : Tactical.tactical =
  object
    inherit Tactical.make ~id:"Wp.compute"
        ~title:"Compute"
        ~descr:"Unfold definitions and split on conditions."
        ~params:[]
    method select feedback (s : Tactical.selection) =
      process feedback ?at:(Tactical.at s) (Tactical.selected s)
  end

let tactical = Tactical.export (new compute)
let strategy = Strategy.make tactical ~arguments:[]

(* -------------------------------------------------------------------------- *)
