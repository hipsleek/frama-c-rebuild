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
(* --- Array Tactical                                                     --- *)
(* -------------------------------------------------------------------------- *)

(* Detects a[i->e][j] pattern *)
let access_update_pattern e =
  let open Qed.Logic in
  match F.repr e with
  | Aget(u,j) ->
    begin match F.repr u with
      | Aset(a,i,e) -> Some(a,i,e,j)
      | _ -> None
    end
  | _ -> None

class array =
  object
    inherit Tactical.make ~id:"Wp.array"
        ~title:"Array"
        ~descr:"Decompose access-update patterns"
        ~params:[]

    method select feedback (s : Tactical.selection) =
      let e = Tactical.selected s in
      match access_update_pattern e with
      | None -> Not_applicable
      | Some(a,i,v,j) ->
        ignore feedback ;
        let at = Tactical.at s in
        let cases = [
          "Same Indices" , F.p_equal i j , e , v ;
          "Diff Indices" , F.p_neq i j , e , F.e_get a j ;
        ] in
        Applicable (Tactical.rewrite ?at cases)

  end

let tactical = Tactical.export (new array)
let strategy = Strategy.make tactical ~arguments:[]
