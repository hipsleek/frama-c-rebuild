(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Filter Tactical                                                    --- *)
(* -------------------------------------------------------------------------- *)

let vanti,panti =
  Tactical.checkbox ~id:"anti"
    ~title:"Absurd"
    ~descr:"Find contradiction in extra hypotheses."
    ~default:false ()

class filter =
  object(self)
    inherit Tactical.make ~id:"Wp.filter"
        ~title:"Filter"
        ~descr:"Eliminates extra hypotheses."
        ~params:[panti]

    method select feedback _sel =
      let anti = self#get_field vanti in
      let process seq = ["Filter",Filtering.compute ~anti seq] in
      feedback#set_title (if anti then "Filter (absurd)" else "Filter") ;
      Applicable process

  end

let tactical = Tactical.export (new filter)

let strategy ?(priority=1.0) ?(anti=false) () =
  Strategy.{
    priority ; tactical ;
    selection = Empty ;
    arguments = [arg vanti anti] ;
  }
