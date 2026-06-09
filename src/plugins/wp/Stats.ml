(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Performance Reporting                                              --- *)
(* -------------------------------------------------------------------------- *)

open VCS

type pstats = {
  tmin : float ;
  tval : float ;
  tmax : float ;
  tnbr : float ;
  time : float ;
  success : float ;
}

type stats = {
  best : VCS.verdict ;
  provers : (Prover.t * pstats) list ;
  tactics : int ;
  proved : int ;
  timeout : int ;
  unknown : int ;
  noresult : int ;
  failed : int ;
  cached : int ;
  cacheable : int ;
}

(* -------------------------------------------------------------------------- *)
(* --- Prover Stats                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Plist = Qed.Listmap.Make
    (struct
      type t = Prover.t
      let equal a b = a==b || (Prover.compare a b = 0)
      let compare = Prover.compare
    end)

let pzero = {
  tmin = max_float ;
  tval = 0.0 ;
  tmax = min_float ;
  tnbr = 0.0 ;
  time = 0.0 ;
  success = 0.0 ;
}

let padd a b =
  if a == pzero then b else
  if b == pzero then a else
    {
      tmin = min a.tmin b.tmin ;
      tmax = max a.tmax b.tmax ;
      tval = a.tval +. b.tval ;
      time = a.time +. b.time ;
      tnbr = a.tnbr +. b.tnbr ;
      success = a.success +. b.success ;
    }

let pmerge = Plist.union (fun _ a b -> padd a b)

let ptime t valid =
  if t < Rformat.epsilon then
    { pzero with tnbr = 1.0 ; success = if valid then 1.0 else 0.0 }
  else
    { tmin = t ; tval = t ; tmax = t ; time = t ; tnbr = 1.0 ;
      success = if valid then 1.0 else 0.0 }

let psolver r = ptime r.solver_time false
let pqed r = if VCS.is_valid r then ptime r.solver_time true else pzero
let presult r = if VCS.is_valid r then ptime r.prover_time true else pzero
let qsmoked r = if VCS.is_valid r then ptime r.solver_time false else ptime 0.0 true
let psmoked r = if VCS.is_valid r then ptime r.prover_time false else ptime 0.0 true
let vsmoked v = if VCS.is_proved ~smoke:true v then VCS.Valid else VCS.Failed

(* -------------------------------------------------------------------------- *)
(* --- Global Stats                                                       --- *)
(* -------------------------------------------------------------------------- *)

let empty = {
  best = NoResult;
  provers = [];
  tactics = 0;
  proved = 0;
  timeout = 0;
  unknown = 0 ;
  noresult = 0 ;
  failed = 0 ;
  cached = 0 ;
  cacheable = 0 ;
}

let cacheable p r =
  p <> Prover.Qed && Prover.is_auto p && VCS.is_cacheable r

let add_cacheable n (p,r) = if cacheable p r then succ n else n
let add_cached n (p,r) = if cacheable p r && r.cached then succ n else n

let results ~smoke results =
  let (p,r) = VCS.best results in
  let verdict = if smoke then vsmoked r.verdict else r.verdict in
  let is v = if verdict == v then 1 else 0 in
  let proved = is Valid in
  let timeout = is Timeout + is Stepout in
  let failed = is Failed in
  let unknown = is Unknown in
  let noresult = 1 - proved - timeout - failed - unknown in
  (* ENSURES: noresult <= 0 && subgoals result == 1 *)
  { (* returns verdict of provers, not verdict for the (smoked) goal *)
    best = r.verdict ; tactics = 0 ;
    proved ; timeout ; unknown ; failed ; noresult ;
    cached = List.fold_left add_cached 0 results ;
    cacheable = List.fold_left add_cacheable 0 results ;
    provers =
      if p = Qed then [Qed,if smoke then qsmoked r else pqed r]
      else
        pmerge [Qed,psolver r] [p,if smoke then psmoked r else presult r] ;
  }

let add a b =
  if a == empty then b else
  if b == empty then a else
    {
      best = VCS.conjunction a.best b.best ;
      provers = pmerge a.provers b.provers ;
      tactics = a.tactics + b.tactics ;
      proved = a.proved + b.proved ;
      timeout = a.timeout + b.timeout ;
      unknown = a.unknown + b.unknown ;
      noresult = a.noresult + b.noresult ;
      failed = a.failed + b.failed ;
      cached = a.cached + b.cached ;
      cacheable = a.cacheable + b.cacheable ;
    }

let tactical ~qed children =
  let valid = List.for_all (fun c -> c.best = Valid) children in
  let qed_only = children = [] in
  let best = if valid then Valid else Unknown in
  let provers = [Prover.Qed,ptime qed qed_only] in
  List.fold_left add { empty with best ; provers ; tactics = 1 } children

let script stats =
  let cached = stats.cached = stats.cacheable in
  let solver = List.fold_left
      (fun t (p,s) ->
         if p = Prover.Qed then t +. s.time else t) 0.0 stats.provers in
  let time = List.fold_left
      (fun t (p,s) ->
         if p <> Prover.Qed then t +. s.time else t) 0.0 stats.provers in
  VCS.result ~cached ~solver ~time stats.best

(* -------------------------------------------------------------------------- *)
(* --- Utils                                                              --- *)
(* -------------------------------------------------------------------------- *)

let subgoals s = s.proved + s.timeout + s.unknown + s.noresult + s.failed
let complete s = s.proved = subgoals s

let pp_pstats fmt p =
  if p.tnbr > 0.0 &&
     p.tmax > Rformat.epsilon &&
     not (Wp_parameters.has_dkey Prover.dkey_shell)
  then
    let mean = p.tval /. p.tnbr in
    let epsilon = 0.05 *. mean in
    let delta = p.tmax -. p.tmin in
    if delta < epsilon then
      Format.fprintf fmt " (%a)" Rformat.pp_time mean
    else
      let middle = (p.tmin +. p.tmax) *. 0.5 in
      if abs_float (middle -. mean) < epsilon then
        Format.fprintf fmt " (%a-%a)"
          Rformat.pp_time p.tmin
          Rformat.pp_time p.tmax
      else
        Format.fprintf fmt " (%a-%a-%a)"
          Rformat.pp_time p.tmin
          Rformat.pp_time mean
          Rformat.pp_time p.tmax

let pp_stats ~shell ~cache fmt s =
  let total = subgoals s in
  if s.tactics > 1 then
    Format.fprintf fmt " (Tactics %d)" s.tactics
  else if s.tactics = 1 then
    Format.fprintf fmt " (Tactic)" ;
  let updating = Cache.is_updating cache in
  let qed_only = match s.provers with [Qed,_] -> true | _ -> false in
  let print_cache = not qed_only && Cache.is_active cache in
  let cachemiss = s.cacheable - s.cached in
  List.iter
    (fun (p,pr) ->
       let success = truncate pr.success in
       let print_proofs = success > 0 && total > 1 in
       let print_perfo =
         pr.time > Rformat.epsilon &&
         (not shell || (not updating && cachemiss > 0))
       in
       if p != Prover.Qed || qed_only || print_perfo || print_proofs
       then
         begin
           let title = Prover.title ~version:false p in
           Format.fprintf fmt " (%s" title ;
           if print_proofs then
             Format.fprintf fmt " %d/%d" success total ;
           if print_perfo then
             Format.fprintf fmt " %a" Rformat.pp_time pr.time ;
           Format.fprintf fmt ")"
         end
    ) s.provers ;
  if shell && cachemiss > 0 && not updating then
    Format.fprintf fmt " (Cache miss %d)" cachemiss
  else
  if print_cache then
    if s.cacheable = 0 then
      if s.best = Valid then
        Format.pp_print_string fmt " (Trivial)"
      else
        Format.pp_print_string fmt " (No Cache)"
    else
    if updating || cachemiss = 0 then
      Format.pp_print_string fmt " (Cached)"
    else
      Format.fprintf fmt " (Cached %d/%d)" s.cached s.cacheable

let pretty = pp_stats ~shell:false ~cache:NoCache

(* -------------------------------------------------------------------------- *)
