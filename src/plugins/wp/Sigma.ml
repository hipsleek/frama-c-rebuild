(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module F = Lang.F

(* -------------------------------------------------------------------------- *)
(* --- Generic Chunk Type                                                 --- *)
(* -------------------------------------------------------------------------- *)

module type ChunkType =
sig
  type t
  val self : string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
  val tau_of_chunk : t -> F.tau
  val basename_of_chunk : t -> string
  val is_init : t -> bool
  val is_primary : t -> bool
  val is_framed : t -> bool
end

(* -------------------------------------------------------------------------- *)
(* --- Generic Sigma Factory                                              --- *)
(* -------------------------------------------------------------------------- *)

type ckind = ..

module type Tag =
sig
  val tag : int
  include ChunkType with type t := ckind
end

(* memory chunks *)
type chunk = { tag : (module Tag) ; ckind : ckind }
let ckind c = c.ckind

module Chunk : ChunkType with type t = chunk =
struct
  type t = chunk
  let self = "Core"
  let hash c =
    let module T = (val c.tag) in Qed.Hcons.hash_pair T.tag @@ T.hash c.ckind
  let equal a b =
    let module A = (val a.tag) in
    let module B = (val b.tag) in
    A.tag = B.tag && A.equal a.ckind b.ckind
  let compare a b =
    let module A = (val a.tag) in
    let module B = (val b.tag) in
    match A.is_primary a.ckind, B.is_primary b.ckind with
    | true, false -> (-1)
    | false, true -> (+1)
    | true, true | false, false ->
      let cmp = Int.compare A.tag B.tag in
      if cmp <> 0 then cmp else A.compare a.ckind b.ckind

  let pretty fmt c =
    let module T = (val c.tag) in T.pretty fmt c.ckind
  let tau_of_chunk c =
    let module T = (val c.tag) in T.tau_of_chunk c.ckind
  let basename_of_chunk c =
    let module T = (val c.tag) in T.basename_of_chunk c.ckind
  let is_init c =
    let module T = (val c.tag) in T.is_init c.ckind
  let is_primary c =
    let module T = (val c.tag) in T.is_primary c.ckind
  let is_framed c =
    let module T = (val c.tag) in T.is_framed c.ckind
end

module Heap = Qed.Collection.Make(Chunk)
module Domain = Heap.Set

(* -------------------------------------------------------------------------- *)
(* --- Domain                                                             --- *)
(* -------------------------------------------------------------------------- *)

type domain = Domain.t
let empty = Domain.empty
let union = Domain.union

(* -------------------------------------------------------------------------- *)
(* --- State                                                              --- *)
(* -------------------------------------------------------------------------- *)

type state = chunk F.Tmap.t

let apply f (s:state) : state =
  F.Tmap.fold (fun t c s -> F.Tmap.add (f t) c s) s F.Tmap.empty

(* -------------------------------------------------------------------------- *)
(* --- Sigma                                                              --- *)
(* -------------------------------------------------------------------------- *)

type sigma = F.var Heap.Map.t ref

let create () = ref Heap.Map.empty
let copy sigma = ref !sigma
let newchunk c =
  let module T = (val c.tag) in
  let basename = T.basename_of_chunk c.ckind in
  let tau = T.tau_of_chunk c.ckind in
  Lang.freshvar ~basename tau

let mem (sigma : sigma) c = Heap.Map.mem c !sigma
let domain (sigma : sigma) : domain = Heap.Map.domain !sigma
let state (sigma : sigma) : state =
  let s = ref F.Tmap.empty in
  Heap.Map.iter (fun c x -> s := F.Tmap.add (F.e_var x) c !s) !sigma ; !s

let get sigma c =
  let s = !sigma in
  try Heap.Map.find c s
  with Not_found ->
    let x = newchunk c in
    sigma := Heap.Map.add c x s ; x

let value sigma c = F.e_var @@ get sigma c

let merge a b =
  let pa = ref Passive.empty in
  let pb = ref Passive.empty in
  let merge_chunk c x y =
    if F.Var.equal x y then x else
      let z = newchunk c in
      pa := Passive.bind ~fresh:z ~bound:x !pa ;
      pb := Passive.bind ~fresh:z ~bound:y !pb ;
      z in
  let merged = Heap.Map.union merge_chunk !a !b in
  ref merged , !pa, !pb

let choose a b =
  let merge_chunk _ x y = if F.Var.compare x y <= 0 then x else y in
  ref @@ Heap.Map.union merge_chunk !a !b

let join a b =
  if a == b then Passive.empty else
    let p = ref Passive.empty in
    Heap.Map.iter2
      (fun chunk x y ->
         match x,y with
         | Some x , Some y -> p := Passive.join x y !p
         | Some x , None -> b := Heap.Map.add chunk x !b
         | None , Some y -> a := Heap.Map.add chunk y !a
         | None , None -> ())
      !a !b ; !p

let havoc sigma domain =
  let ys = Domain.mapping newchunk domain in
  ref @@ Heap.Map.union (fun _v _old y -> y) !sigma ys

let remove_chunks sigma domain =
  ref @@ Heap.Map.filter (fun c _ -> not @@ Domain.mem c domain) !sigma

let havoc_chunk sigma c =
  let x = newchunk c in
  ref @@ Heap.Map.add c x !sigma

let is_init c =
  let module T = (val c.tag) in T.is_init c.ckind

let is_framed c =
  let module T = (val c.tag) in T.is_framed c.ckind

let havoc_any ~call sigma =
  let framer c x = if call && is_framed c then x else newchunk c in
  ref @@ Heap.Map.mapi framer !sigma

(* Merge All *)

type usage = Used of F.var | Unused

let merge_list (ls : sigma list) : sigma * Passive.t list =
  (* Get a map of the chunks (the data is not important) *)
  let domain =
    Heap.Map.map (fun _ -> []) @@
    let dunion _ c _ = c in
    List.fold_left
      (fun acc s -> Heap.Map.merge dunion acc !s)
      Heap.Map.empty ls in
  (* Accumulate usage for each c, preserving the order of the list *)
  let usage =
    let umerge _ c e =
      match c, e with
      | Some c, Some x -> Some (Used x :: c)
      | Some c, None -> Some (Unused :: c)
      | None, _ -> assert false in
    List.fold_left
      (fun acc s -> Heap.Map.merge umerge acc !s)
      domain (List.rev ls) in
  (* Build the passive for each sigma of the list, and the final sigma *)
  let passives = ref @@ List.map (fun _ -> Passive.empty) ls in
  let choose c uses =
    let used = function Used _ -> true | Unused -> false in
    let compatible x = function Used y -> F.Var.equal x y | Unused -> true in
    match List.filter used uses with
    | [] -> assert false
    | Used x :: others when List.for_all (compatible x) others -> x
    | _ ->
      let fresh = newchunk c in
      let join pa = function
        | Unused -> pa
        | Used bound -> Passive.bind ~fresh ~bound pa in
      passives := List.map2 join !passives uses ; fresh
  in ref @@ Heap.Map.mapi choose usage, !passives

(* Effects *)

let writes ~(pre:sigma) ~(post:sigma) : domain =
  let domain = ref Domain.empty in
  Heap.Map.iter2
    (fun c u v ->
       let written =
         match u,v with
         | Some x , Some y -> not @@ F.Var.equal x y
         | None , Some _ -> true
         | Some _ , None -> false (* no need to create a new *)
         | None, None -> assert false
       in if written then domain := Domain.add c !domain
    ) !pre !post ;
  !domain

let assigned ~(pre:sigma) ~(post:sigma) written =
  let p = ref Bag.empty in
  Heap.Map.iter2
    (fun chunk x y ->
       if not (Domain.mem chunk written) then
         match x,y with
         | Some x , Some y when x != y ->
           p := Bag.add (F.p_equal (F.e_var x) (F.e_var y)) !p
         | Some x , None -> post := Heap.Map.add chunk x !post
         | None , Some y -> pre := Heap.Map.add chunk y !pre
         | _ -> ())
    !pre !post ; !p

let iter f s = Heap.Map.iter f !s
let iter2 f a b = Heap.Map.iter2 f !a !b

let pretty fmt sigma =
  begin
    Format.fprintf fmt "{@[<hov 2>" ;
    Heap.Map.iter
      (fun c x ->
         let module T = (val c.tag) in
         Format.fprintf fmt "@ %a:%a" T.pretty c.ckind F.Var.pretty x)
      !sigma ;
    Format.fprintf fmt "@]}" ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Chunks                                                             --- *)
(* -------------------------------------------------------------------------- *)

module Make(C : ChunkType) =
struct
  type ckind += Mu of C.t
  module T =
  struct
    let self = C.self
    let tag = Obj.Extension_constructor.id [%extension_constructor Mu]
    let map f = function Mu m -> f m | _ -> assert false
    let map2 f a b =
      match a,b with Mu a, Mu b -> f a b | _ -> assert false
    let hash = map C.hash
    let equal = map2 C.equal
    let compare = map2 C.compare
    let is_init = map C.is_init
    let is_primary = map C.is_primary
    let is_framed = map C.is_framed
    let tau_of_chunk = map C.tau_of_chunk
    let basename_of_chunk = map C.basename_of_chunk
    let pretty fmt = map (C.pretty fmt)
  end
  let tag = (module T : Tag)
  let chunk c = { tag ; ckind = Mu c }
  let mem sigma c = mem sigma @@ chunk c
  let get sigma c = get sigma @@ chunk c
  let value sigma c = value sigma @@ chunk c
  let singleton c = Domain.singleton @@ chunk c
end

(* -------------------------------------------------------------------------- *)
