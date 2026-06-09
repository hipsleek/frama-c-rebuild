(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let gcd = Int.gcd
let (%.) = Int.gcd

(* -------------------------------------------------------------------------- *)
(* --- Range Maps                                                         --- *)
(* -------------------------------------------------------------------------- *)

type 'a range = {
  offset: int ;
  length: int ;
  data: 'a ;
}

let pp_range fmt r =
  Format.fprintf fmt "%04d..%04d" r.offset (r.offset + r.length - 1)

let pp_offset fmt r =
  Format.fprintf fmt "%04d:%04d" r.offset r.length

type 'a t = R of 'a range list (* sorted, no-overlap *)

let empty = R []

let singleton r =
  if not (0 <= r.offset && 0 < r.length) then
    invalid_arg "Region.Ranges.singleton" [@coverage off] ;
  R [r]

let range ?(offset=0) ?(length=1) data = singleton { offset ; length ; data }

let rec find (k: int) = function
  | [] -> raise Not_found
  | ({ offset ; length } as r) :: rs ->
    if offset <= k && k < offset + length then r else find k rs

let find k (R rs) = find k rs

let rec merge f ra rb =
  match ra, rb with
  | [], rs | rs, [] -> rs
  | a :: wa, b :: wb ->
    let a' = a.offset + a.length in
    let b' = b.offset + b.length in
    if a' <= b.offset then a :: merge f wa rb else
    if b' <= a.offset then b :: merge f ra wb else
      let offset = min a.offset b.offset in
      let length = max a' b' - offset in
      let data = f a b in
      let r = { offset ; length ; data } in
      if a' < b' then merge f wa (r::wb) else
      if b' < a' then merge f (r::wa) wb else
        r :: merge f wa wb

let merge f (R x) (R y) = R (merge f x y)

let iteri f (R xs) = List.iter f xs
let foldi f w (R xs) = List.fold_left f w xs
let iter f (R xs) = List.iter (fun r -> f r.data) xs
let fold f w (R xs) = List.fold_left (fun w r -> f w r.data) w xs
let mapi f (R xs) = R (List.map f xs)
let map f (R xs) = R (List.map (fun r -> { r with data = f r.data }) xs)

(* -------------------------------------------------------------------------- *)
