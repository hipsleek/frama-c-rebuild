(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  Jean-Christophe Filliatre                                             *)
(*  Modified by                                                           *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


(*s A trie is a tree-like structure to implement dictionaries over
    keys which have list-like structures. The idea is that each node
    branches on an element of the list and stores the value associated
    to the path from the root, if any. Therefore, a trie can be
    defined as soon as a map over the elements of the list is
    given. *)

module type S = sig
  type key
  type +'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t ->  'b t -> 'c t
  val union :
    (key -> 'a -> 'a -> 'a option) -> 'a t ->  'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val to_seq : 'a t -> (key * 'a) Seq.t
end

module Make(M : S) =
struct

  (*s Then a trie is just a tree-like structure, where a possible
      information is stored at the node (['a option]) and where the sons
      are given by a map from type [key] to sub-tries, so of type
      ['a t M.t]. The empty trie is just the empty map. *)

  type key = M.key list

  type 'a t = Node of 'a option * 'a t M.t

  let empty = Node (None, M.empty)

  (*s To find a mapping in a trie is easy: when all the elements of the
    key have been read, we just inspect the optional info at the
    current node; otherwise, we descend in the appropriate sub-trie
    using [M.find]. *)

  let rec find l t = match (l,t) with
    | [], Node (None,_)   -> raise Not_found
    | [], Node (Some v,_) -> v
    | x::r, Node (_,m)    -> find r (M.find x m)

  let rec find_opt l t = match (l,t) with
    | [], Node (None,_)   -> None
    | [], Node (Some v,_) -> Some v
    | x::r, Node (_,m)    -> Option.bind (find_opt r) (M.find_opt x m)

  let rec mem l t = match (l,t) with
    | [], Node (None,_)   -> false
    | [], Node (Some _,_) -> true
    | x::r, Node (_,m)    -> try mem r (M.find x m) with Not_found -> false

  (*s Insertion is more subtle. When the final node is reached, we just
    put the information ([Some v]). Otherwise, we have to insert the
    binding in the appropriate sub-trie [t']. But it may not exists,
    and in that case [t'] is bound to an empty trie. Then we get a new
    sub-trie [t''] by a recursive insertion and we modify the
    branching, so that it now points to [t''], with [M.add]. *)

  let add l v t =
    let rec ins = function
      | [], Node (_,m) -> Node (Some v,m)
      | x::r, Node (v,m) ->
        let t' = try M.find x m with Not_found -> empty in
        let t'' = ins (r,t') in
        Node (v, M.add x t'' m)
    in
    ins (l,t)

  (*s When removing a binding, we take care of not leaving bindings to empty
      sub-tries in the nodes. Therefore, we test whether the result [t'] of
      the recursive call is the empty trie [empty]: if so, we just remove
      the branching with [M.remove]; otherwise, we modify it with [M.add]. *)

  let rec remove l t = match (l,t) with
    | [], Node (_,m) -> Node (None,m)
    | x::r, Node (v,m) ->
      try
        let t' = remove r (M.find x m) in
        Node (v, if t' = empty then M.remove x m else M.add x t' m)
      with Not_found ->
        t

  (*s The iterators [map], [mapi], [iter] and [fold] are implemented in
      a straightforward way using the corresponding iterators [M.map],
      [M.mapi], [M.iter] and [M.fold]. For the last three of them,
      we have to remember the path from the root, as an extra argument
      [revp]. Since elements are pushed in reverse order in [revp],
      we have to reverse it with [List.rev] when the actual binding
      has to be passed to function [f]. *)

  let rec map f = function
    | Node (None,m)   -> Node (None, M.map (map f) m)
    | Node (Some v,m) -> Node (Some (f v), M.map (map f) m)

  let mapi f t =
    let rec maprec revp = function
      | Node (None,m) ->
        Node (None, M.mapi (fun x -> maprec (x::revp)) m)
      | Node (Some v,m) ->
        Node (Some (f revp v), M.mapi (fun x -> maprec (x::revp)) m)
    in
    maprec [] t

  let iter f t =
    let rec traverse revp = function
      | Node (None,m) ->
        M.iter (fun x -> traverse (x::revp)) m
      | Node (Some v,m) ->
        f revp v; M.iter (fun x t -> traverse (x::revp) t) m
    in
    traverse [] t

  let fold f t acc =
    let rec traverse revp t acc = match t with
      | Node (None,m) ->
        M.fold (fun x -> traverse (x::revp)) m acc
      | Node (Some v,m) ->
        f revp v (M.fold (fun x -> traverse (x::revp)) m acc)
    in
    traverse [] t acc

  let exists f t =
    let rec traverse revp t = match t with
      | Node (None,m) ->
        M.exists (fun x -> traverse (x::revp)) m
      | Node (Some v,m) ->
        f revp v || M.exists (fun x -> traverse (x::revp)) m
    in
    traverse [] t

  let compare cmp a b =
    let rec comp a b = match a,b with
      | Node (Some _, _), Node (None, _) -> 1
      | Node (None, _), Node (Some _, _) -> -1
      | Node (None, m1), Node (None, m2) ->
        M.compare comp m1 m2
      | Node (Some a, m1), Node (Some b, m2) ->
        let c = cmp a b in
        if c <> 0 then c else M.compare comp m1 m2
    in
    comp a b

  let equal eq a b =
    let rec comp a b = match a,b with
      | Node (None, m1), Node (None, m2) ->
        M.equal comp m1 m2
      | Node (Some a, m1), Node (Some b, m2) ->
        eq a b && M.equal comp m1 m2
      | _ ->
        false
    in
    comp a b

  (* The base case is rather stupid, but constructable *)
  let is_empty = function
    | Node (None, m1) -> M.is_empty m1
    | Node (Some _, _) -> false

  let merge f t1 t2 =
    let rec aux revp t1 t2 =
      let v1, m1 = match t1 with
        | None -> None, M.empty
        | Some (Node (v1, m1)) -> v1, m1
      and v2, m2 = match t2 with
        | None -> None, M.empty
        | Some (Node (v2, m2)) -> v2, m2
      in
      let v = f revp v1 v2
      and m = M.merge (fun x t1 t2 -> Some (aux (x :: revp) t1 t2)) m1 m2 in
      Node (v, m)
    in
    aux [] (Some t1) (Some t2)

  let union f t1 t2 =
    let rec aux revp t1 t2 =
      let Node (v1, m1) = t1 and Node (v2, m2) = t2 in
      let v = match v1, v2 with
        | None, None -> None
        | (Some _ as v), None | None, (Some _ as v) -> v
        | Some v1, Some v2 -> f revp v1 v2
      and m = M.union (fun x t1 t2 -> Some (aux (x :: revp) t1 t2)) m1 m2 in
      Node (v, m)
    in
    aux [] t1 t2

  let to_seq t =
    let rec aux revp t =
      let Node (v, m) = t in
      Seq.append
        (Seq.map (fun v -> revp, v) (Option.to_seq v))
        (Seq.flat_map (fun (x, t) -> aux (x :: revp) t) (M.to_seq m))
    in
    aux [] t

  let add_prefix k m = Node (None, M.add k m M.empty)

  let select_prefix k t =
    let Node(_, m) = t in
    M.find k m

  let prefixes_seq (Node (_, map)) =
    M.to_seq map
end
