(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Minimal = struct
  type 'a t = 'a list
  let return   x = [ x ]
  let map   f xs = Stdlib.List.map f xs
  let flatten xs = Stdlib.List.flatten xs
  let product ls rs =
    let pair l = map (fun r -> (l, r)) rs in
    Stdlib.List.concat_map pair ls
end

(** {2 Monad } *)

include Monad.Make_based_on_map_with_product (Minimal)

include Stdlib.List

(** {2 Datatype functions } *)

let compare cmp_elt l1 l2 =
  if l1 == l2 then 0
  else compare cmp_elt l1 l2

let hash hash_elt =
  Hash.hash_iter iter hash_elt

let pretty
    ?(format=format_of_string "[ %t ]")
    ?(item=format_of_string "%a")
    ?(sep=format_of_string ";@ ")
    ?(last=sep)
    ?(empty=format_of_string "[]")
    pp_elt fmt l =
  Pretty.pretty_seq ~format ~item ~sep ~last ~empty pp_elt fmt (to_seq l)

let pretty_text
    ?(format=format_of_string "%t")
    ?(item=format_of_string "%a")
    ?(sep=format_of_string ",@ ")
    ?(last=format_of_string "@ and@ ")
    ?(empty=format_of_string "<empty>")
    pp_elt fmt l =
  Pretty.pretty_seq ~format ~item ~sep ~last ~empty pp_elt fmt (to_seq l)


(** {2 Iterators } *)

let find_index f l =
  let rec aux i = function
      [] -> None
    | x::l -> if f x then Some i else aux (i+1) l
  in aux 0 l

let mapi2 f l1 l2 =
  let i = ref 0 in
  map2 (fun x y -> let r = f !i x y in incr i; r) l1 l2

(* used by [map_no_copy] and [concat_map_no_copy] *)
let rev_until i l =
  let rec aux acc =
    function
    | [] -> acc
    | i'::_ when i' == i -> acc
    | i'::l -> aux (i'::acc) l
  in aux [] l

let map_no_copy (f: 'a -> 'a) orig =
  let rec aux ((acc,has_changed) as res) l =
    match l with
    | [] -> if has_changed then rev acc else orig
    | i :: resti ->
      let i' = f i in
      if has_changed then
        aux (i'::acc,true) resti
      else if i' != i then
        aux (i'::rev_until i orig,true) resti
      else
        aux res resti
  in aux ([],false) orig

let concat_map_no_copy (f: 'a -> 'a list) orig =
  let rec aux ((acc,has_changed) as res) l =
    match l with
    | [] -> if has_changed then rev acc else orig
    | i :: resti ->
      let l' = f i in
      if has_changed then
        aux (rev_append l' acc,true) resti
      else
        (match l' with
         | [i'] when i' == i -> aux res resti
         | _ -> aux (rev_append l' (rev_until i orig), true) resti)
  in aux ([],false) orig


(** {2 Accessors } *)

let as_singleton = function
  | [a] -> a
  | _ -> invalid_arg "List.as_singleton"

let rec last = function
  | [] -> invalid_arg "List.last"
  | [a] -> a
  | _ :: l -> last l

let[@tail_mod_cons] rec take n = function
  | h :: t when n > 0 -> h :: take (n-1) t
  | _ -> []

let rec drop n = function
  | _h :: t when n > 0 -> drop (n-1) t
  | l -> l

let rec break n l =
  if n <= 0 then ([], l)
  else match l with
    | [] -> ([], [])
    | a :: l ->
      let l1, l2 = break (n - 1) l in
      (a :: l1, l2)

let slice ?(first = 0) ?last l =
  let len = lazy (length l) in
  let normalize i =
    (* normalize negative values *)
    if i >= 0
    then i
    else
      let n = Lazy.force len in
      if i + n >= 0 then i + n else 0
  in
  (* Remove first elements *)
  let first = normalize first in
  let l = drop first l in
  (* Remove last elements *)
  match last with
  | None -> l
  | Some n -> take (normalize n - first) l


(** {2 Mutators } *)

let replace cmp x l =
  let rec aux = function
    | [] -> [x]
    | y::l -> if cmp x y then x::l else y :: aux l
  in aux l


(** {2 Product of lists } *)

let product_fold f acc e1 e2 =
  fold_left
    (fun acc e1 -> fold_left (fun acc e2 -> f acc e1 e2) acc e2)
    acc e1

let product_map f e1 e2 =
  product_fold (fun acc e1 e2 -> f e1 e2 :: acc) [] e1 e2


(** {2 Conversion } *)

let to_option =
  function
  | [] -> None
  | [a] -> Some a
  | _ -> raise (Invalid_argument "List.to_option")


(** {2 Combinations } *)

let combinations k l =
  let rec aux k l len =
    if k = 0 then [[]]
    else if len < k then []
    else if len = k then [l]
    else
      match l with
      | h :: t ->
        let l1 = map (fun sl -> h :: sl) (aux (k-1) t (len-1)) in
        let l2 = aux k t (len-1)
        in l1 @ l2
      | [] -> assert false
  in aux k l (length l)



module Make_monadic_iterators (M : Monad.S) = struct
  type 'a iterable = 'a list
  type 'a monad = 'a M.t

  let fold f acc xs =
    let f acc x = M.bind (fun acc -> f acc x) acc in
    fold_left f (M.return acc) xs

  let map f xs =
    let f rs x = M.map (fun r -> r :: rs) (f x) in
    M.map rev (fold f [] xs)

  let iter f xs =
    fold (fun () -> f) () xs

end
