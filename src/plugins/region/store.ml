(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- UnionFind Store with explicit integer keys                         --- *)
(* -------------------------------------------------------------------------- *)

module Imap = Map.Make(Int)

module S =
struct
  type 'a rref = int
  type 'a store = {
    mutable rid : int ;
    mutable map : 'a Imap.t ;
  }

  let new_store () = { rid = 0 ; map = Imap.empty  }
  let copy r = { rid = r.rid ; map = r.map }

  let make s v =
    let k = succ s.rid in
    s.rid <- k ; s.map <- Imap.add k v s.map ; k

  let get s k = Imap.find k s.map
  let set s k v = s.map <- Imap.add k v s.map

  let eq _s i j = (i == j)

end

module Ufind :
sig
  type 'a rref
  type 'a store
  val id : 'a rref -> int
  val new_store : unit -> 'a store
  val get : 'a store -> 'a rref -> 'a
  val set : 'a store -> 'a rref -> 'a -> unit
  val make : 'a store -> 'a -> 'a rref
  val find : 'a store -> 'a rref -> 'a rref
  val merge : 'a store -> ('a -> 'a -> 'a) -> 'a rref -> 'a rref -> 'a rref
end =
struct
  include UnionFind.Make(S)
  let id = Fun.id
end

module type NodeData =
sig
  type 'a t
  val get_id : 'a t -> int
  val set_id : 'a t -> int -> unit
end


module Make(D : NodeData) =
struct

  type store = {
    values : data Ufind.store ;
    keymap : (int,node) Hashtbl.t ;
  }
  and data = node D.t
  and node = { rref : data Ufind.rref ; store : store }

  let check ~fn (m : store) (m' : store) =
    if m == m' then m else
      invalid_arg
        (Printf.sprintf "Region.Store.%s (inconsistent maps)" fn)
      [@ coverage off]

  let checklock ~fn store =
    if Hashtbl.length store.keymap > 0 then
      invalid_arg
        (Printf.sprintf "Region.Store.%s (locked map)" fn)
      [@ coverage off]

  let create () = { values = Ufind.new_store () ; keymap = Hashtbl.create 0 }

  let ufind n = Ufind.find n.store.values n.rref
  let find n = { n with rref = ufind n }

  let by_rank a b = Int.compare (Ufind.id a.rref) (Ufind.id b.rref)
  let find_all ns = List.sort_uniq by_rank @@ List.map find ns

  let find_all2 xs ys =
    let rec bag xs ys =
      match xs , ys with
      | [] , w | w , [] -> List.map find w
      | x::xs , y::ys ->
        if x.rref == y.rref then
          find x :: bag xs ys
        else
          find x :: find y :: bag xs ys
    in List.sort_uniq by_rank (bag xs ys)

  let store a = a.store
  let get a = Ufind.get a.store.values a.rref
  let set a v = Ufind.set a.store.values a.rref v
  let any a b = if Ufind.id a.rref <= Ufind.id b.rref then a else b

  let fresh store v =
    checklock ~fn:"fresh" store ;
    { rref = Ufind.make store.values v ; store }

  let eq a b =
    let store = check ~fn:"eq" a.store b.store in
    S.eq store a.rref b.rref

  let merge f a b =
    let store = check ~fn:"merge" a.store b.store in
    checklock ~fn:"merge" store ;
    { rref = Ufind.merge store.values f a.rref b.rref ; store }

  let noid = (-1)
  let is_locked store = Hashtbl.length store.keymap > 0

  let lock a : bool =
    let d = get a in
    0 < D.get_id d ||
    begin
      let uid = succ @@ Hashtbl.length a.store.keymap in
      Hashtbl.add a.store.keymap uid a ;
      D.set_id d uid ; false
    end

  let id a = checklock ~fn:"id" a.store ; D.get_id @@ get a
  let of_id store k =
    try Hashtbl.find store.keymap k
    with Not_found -> invalid_arg "Region.Store.of_id" [@ coverage off]

  let pretty fmt a =
    if is_locked a.store then
      Format.fprintf fmt "R%04x" (D.get_id @@ get a)
    else
      Format.fprintf fmt "#%04X" (Ufind.id a.rref)

  type marks = Z.t ref
  let marks () = ref Z.zero

  let marked m n =
    let uid = D.get_id @@ get n in
    Z.testbit !m uid

  let test_and_mark m n =
    let uid = D.get_id @@ get n in
    Z.testbit !m uid ||
    ( m := Z.(!m lor (one lsl uid)) ; false )

  let once f =
    let m = ref Z.zero in
    fun n ->
      let uid = D.get_id @@ get n in
      Z.testbit !m uid ||
      ( m := Z.(!m lor (one lsl uid)) ; f n ; false )

end
