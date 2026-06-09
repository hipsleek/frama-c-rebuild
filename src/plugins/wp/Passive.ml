(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Passive Forms                                                      --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

type binding =
  | Bind of var * var (* fresh , bound *)
  | Join of var * var (* left, right *)
type t = binding list

let empty = []
let is_empty n = n = []
let union = List.append

let bind ~fresh ~bound bs = Bind(fresh,bound) :: bs
let join x y bs =
  if Var.equal x y then bs else Join(x,y) :: bs

let eq x y = F.p_equal (e_var x) (e_var y)

let rec collect phi hs = function
  | [] -> hs
  | Bind(x,y)::bs -> collect phi (if phi y then eq x y :: hs else hs) bs
  | Join(x,y)::bs -> collect phi (if phi x || phi y then eq x y :: hs else hs) bs

let apply bindings p =
  let xs = varsp p in
  let hs = collect (fun x -> Vars.mem x xs) [] bindings in
  p_conj (p::hs)

let conditions bindings phi = collect phi [] bindings

let iter = List.iter

let pretty fmt =
  List.iter
    begin function
      | Bind(x,y) ->
        Format.fprintf fmt "@ @[%a:=%a@]"
          F.pp_var x F.pp_var y
      | Join(x,y) ->
        Format.fprintf fmt "@ @[%a==%a@]"
          F.pp_var x F.pp_var y
    end
