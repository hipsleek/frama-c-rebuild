(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang
open Conditions
open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Choice Tactical                                                    --- *)
(* -------------------------------------------------------------------------- *)

class choice =
  object
    inherit Tactical.make
        ~id:"Wp.choice"
        ~title:"Choice"
        ~descr:"Select one alternative to be proved."
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      match s with
      | Inside(Goal p,q) ->
        begin
          match F.e_expr p with
          | Qed.Logic.Or qs when List.memq q qs ->
            Applicable (fun (hs,_) -> ["Choice",(hs,F.p_bool q)])
          | _ -> Not_applicable
        end
      | Empty | Compose _ | Clause _ | Inside(Step _,_) | Multi _ ->
        Not_applicable
  end

class absurd =
  object
    inherit Tactical.make
        ~id:"Wp.absurd"
        ~title:"Absurd"
        ~descr:"Contradict the selected clause."
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      match s with
      | Empty | Compose _ | Inside _ | Multi _
        -> Not_applicable
      | Clause(Goal _) ->
        let absurd (seq,goal) =
          let goal = F.p_not goal in
          let step = Conditions.step ~descr:"Absurd" (When goal) in
          let hyps = Conditions.list seq in
          let hyps = Conditions.sequence (hyps @ [step]) in
          [ "Absurd", (hyps , F.p_false) ]
        in Applicable absurd
      | Clause(Step s) ->
        begin
          match s.condition with
          | Have p | When p | Core p | Init p | Type p ->
            let absurd seq =
              let emp = Conditions.(step (Have F.p_true)) in
              let seq = Conditions.replace ~at:s.id emp seq in
              [ "Absurd" , (fst seq , F.p_not p) ]
            in Applicable absurd
          | Branch _ | Either _ | State _ | Probe _ ->
            Not_applicable
        end
  end

class contrapose =
  object
    inherit Tactical.make
        ~id:"Wp.contrapose"
        ~title:"Contrapose"
        ~descr:"Swap hypothesis with conclusion."
        ~params:[]

    method select _feedback (s : Tactical.selection) =
      match s with
      | Empty | Compose _ | Inside _ | Clause(Goal _) | Multi _
        -> Not_applicable
      | Clause(Step s) ->
        begin
          match s.condition with
          | Have p | When p | Core p | Init p | Type p ->
            let contrapose (hs,goal) =
              let descr = "Contrapose the goal." in
              let goal = F.p_not goal in
              let goal = Conditions.(step ~descr (Have goal)) in
              let hs = Conditions.replace ~at:s.id goal (hs , F.p_false) in
              [ "Contrapose" , (fst hs , F.p_not p) ]
            in Applicable contrapose
          | Branch _ | Either _ | State _ | Probe _ ->
            Not_applicable
        end

  end

module Choice =
struct
  let tactical = Tactical.export (new choice)
  let strategy = Strategy.make tactical ~arguments:[]
end

module Absurd =
struct
  let tactical = Tactical.export (new absurd)
  let strategy = Strategy.make tactical ~arguments:[]
end

module Contrapose =
struct
  let tactical = Tactical.export (new contrapose)
  let strategy = Strategy.make tactical ~arguments:[]
end
