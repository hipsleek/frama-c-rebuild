(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Memory

module Make
    ( M : Memory.Model )
    ( L : Memory.LogicSemantics with module M = M ) =
struct

  module M = M
  module L = L
  module D = Sigma.Domain

  (* -------------------------------------------------------------------------- *)
  (* --- Domain                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let dsloc obj = function
    | Sloc l | Sdescr(_,l,_) -> M.domain obj l
    | Srange(l,obj,_,_) | Sarray(l,obj,_) -> M.domain obj l

  let domain (r: L.loc Memory.region) =
    List.fold_left
      (fun d (obj,sloc) -> D.union d (dsloc obj sloc)) D.empty r

  (* -------------------------------------------------------------------------- *)
  (* --- Assignation                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let rec assigned_seq hs s = function
    | [] -> Bag.concat (Sigma.assigned ~pre:s.pre ~post:s.post D.empty) hs

    | [obj,sloc] ->
      let eq_sloc = M.assigned s obj sloc in
      let hs_sloc = Bag.list (List.map Cvalues.equation eq_sloc) in
      let hs_sdom = Sigma.assigned ~pre:s.pre ~post:s.post (dsloc obj sloc) in
      Bag.concat (Bag.concat hs_sloc hs_sdom) hs

    | (obj,sloc)::tail ->
      let sigma = Sigma.havoc s.post (dsloc obj sloc) in
      let s_local = { pre = sigma ; post = s.post } in
      let s_other = { pre = s.pre ; post = sigma } in
      let eq_sloc = M.assigned s_local obj sloc in
      let hs_sloc = Bag.list (List.map Cvalues.equation eq_sloc) in
      assigned_seq (Bag.concat hs_sloc hs) s_other tail

  let apply_assigns (s: Sigma.sigma sequence) (r: M.loc Memory.region) =
    Bag.elements (assigned_seq Bag.empty s r)

end
