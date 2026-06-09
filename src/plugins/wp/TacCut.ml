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

(* -------------------------------------------------------------------------- *)
(* --- Cut Tactical                                                       --- *)
(* -------------------------------------------------------------------------- *)

let fclause,pclause =
  Tactical.composer
    ~id:"clause"
    ~title:"Clause"
    ~descr:"Clause to cut with."
    ~filter:F.is_prop
    ()

type mode = CASES | MODUS

let fmode,pmode =
  Tactical.selector
    ~id:"case"
    ~title:"Mode"
    ~descr:"Select how the clause is used."
    ~default:MODUS
    ~options:Tactical.[
        { title="Case Analysis" ;
          descr="Consider P->Q and !P->Q." ;
          vid="CASES" ; value=CASES } ;
        { title="Modus Ponens" ;
          descr="Consider P and P->Q." ;
          vid="MODUS" ; value=MODUS } ;
      ] ()

class cut =
  object(self)
    inherit Tactical.make ~id:"Wp.cut"
        ~title:"Cut"
        ~descr:"Use intermerdiate hypothesis."
        ~params:[pmode;pclause]

    method select feedback sel =
      let mode =
        match sel with
        | Clause(Goal p) when p != F.p_false ->
          feedback#update_field ~enabled:false fmode ; CASES
        | _ ->
          feedback#update_field ~enabled:true fmode ;
          self#get_field fmode in
      let cut = self#get_field fclause in
      if Tactical.is_empty cut then
        Not_configured
      else
        match mode with
        | MODUS ->
          feedback#set_descr "Prove then insert the clause." ;
          let clause = F.p_bool (Tactical.selected cut) in
          let step = Conditions.step ~descr:"Cut" (Have clause) in
          let at = Tactical.at sel in
          Applicable
            begin fun sequent ->
              let assume = Conditions.insert ?at step sequent in
              [ "Clause" , (fst sequent,clause) ;
                "Assume" , (fst assume,snd sequent) ]
            end
        | CASES ->
          feedback#set_descr "Proof by case in the clause." ;
          let positive = F.p_bool (Tactical.selected cut) in
          let negative = F.p_not positive in
          Applicable
            begin fun (hs,goal) ->
              [ "Positive" , (hs,F.p_imply positive goal) ;
                "Negative" , (hs,F.p_imply negative goal) ]
            end
  end

let tactical = Tactical.export (new cut)

let strategy ?(priority=1.0) ?(modus=true) selection =
  Strategy.{
    priority ;
    tactical ;
    selection ;
    arguments = [ arg fmode (if modus then MODUS else CASES) ] ;
  }
