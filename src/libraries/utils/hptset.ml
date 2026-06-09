(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Sets over ordered types.

    This module implements the set data structure.
    All operations over sets are purely applicative (no side-effects). *)

module type S_Basic_Compare =
sig
  type elt
  type t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements: t -> elt list
  val choose: t -> elt
  val find: elt -> t -> elt
  val of_list: elt list -> t
end

module type S = sig
  type 'a map
  include Datatype.S_with_collections with type t = unit map
  include S_Basic_Compare with type t := t

  val contains_single_elt: t -> elt option
  val intersects: t -> t -> bool

  type action = Neutral | Absorbing | Traversing of (elt -> bool)

  val merge :
    cache:Hptmap_sig.cache_type ->
    symmetric:bool ->
    idempotent:bool ->
    decide_both:(elt -> bool) ->
    decide_left:action ->
    decide_right:action ->
    t -> t -> t

  val from_map: 'a map -> t

  val fold2_join_heterogeneous:
    cache:Hptmap_sig.cache_type ->
    empty_left:('a map -> 'b) ->
    empty_right:(t -> 'b) ->
    both:(elt -> 'a -> 'b) ->
    join:('b -> 'b -> 'b) ->
    empty:'b ->
    t -> 'a map ->
    'b

  val replace: elt map -> t -> bool * t

  val clear_caches: unit -> unit

  val pretty_debug: t Pretty_utils.formatter
end

module type Info = sig
  type elt
  val initial_values : elt list list
  val dependencies : State.t list
end

module Make
    (X: Hptmap.Id_Datatype)
    (Info : Info with type elt := X.t)
  : sig
    include S with type elt = X.t
               and type 'a map = 'a Hptmap.Shape(X).t
    val self : State.t
  end
= struct

  type elt = X.t

  module Hptmap_Info = struct
    let initial_values = List.map (List.map (fun k -> k, ())) Info.initial_values
    let dependencies = Info.dependencies
  end

  module M = Hptmap.Make (X) (Datatype.Unit) (Hptmap_Info)
  include M

  let add k s = add k () s
  let iter f s = iter (fun x () -> f x) s
  let fold f s = fold (fun x () -> f x) s

  let elements s = fold (fun h t -> h::t) s []

  let contains_single_elt s =
    match is_singleton s with
      Some (k, _v) -> Some k
    | None -> None

  let choose s = fst (min_binding s)

  let partition f s =
    fold
      (fun x (w, wo) -> if f x then add x w, wo else w, add x wo) s (empty, empty)

  let mem x s = try find x s; true with Not_found -> false

  let find x s = find_key x s

  let inter =
    let name = Format.sprintf "Hptset(%s).inter" X.datatype_name in
    inter
      ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:true
      ~idempotent:true
      ~decide:(fun _ () () -> Some ())

  let union =
    let name = Format.sprintf "Hptset(%s).union" X.datatype_name in
    join ~cache:(Hptmap_sig.PersistentCache name) ~decide:(fun _ () () -> ())
      ~symmetric:true ~idempotent:true

  let singleton x = add x empty

  let exists f s = exists (fun k () -> f k) s

  let for_all f s = for_all (fun k () -> f k) s

  let subset =
    let name = Format.sprintf "Hptset(%s).subset" X.datatype_name in
    binary_predicate (Hptmap_sig.PersistentCache name) UniversalPredicate
      ~decide_fast:decide_fast_inclusion
      ~decide_fst:(fun _ () -> false)
      ~decide_snd:(fun _ () -> true)
      ~decide_both:(fun _ () () -> true)

  let pretty =
    if X.pretty == Datatype.undefined then
      Datatype.undefined
    else
      Pretty_utils.pp_iter
        ~pre:"@[<hov 1>{" ~sep:",@ " ~suf:"}@]" iter X.pretty

  let intersects =
    let name = Format.asprintf "Hptset(%s).intersects" X.datatype_name in
    symmetric_binary_predicate
      (Hptmap_sig.PersistentCache name)
      ExistentialPredicate
      ~decide_fast:decide_fast_intersection
      ~decide_one:(fun _ () -> false)
      ~decide_both:(fun _ () () -> true)

  let of_list l = List.fold_left (fun acc key -> add key acc) empty l

  type action = Neutral | Absorbing | Traversing of (elt -> bool)

  let translate_action = function
    | Neutral -> M.Neutral
    | Absorbing -> M.Absorbing
    | Traversing f -> M.Traversing (fun k () -> if f k then Some () else None)

  let merge ~cache ~symmetric ~idempotent
      ~decide_both ~decide_left ~decide_right =
    let decide_both = fun k () () -> if decide_both k then Some () else None
    and decide_left = translate_action decide_left
    and decide_right = translate_action decide_right in
    merge ~cache ~symmetric ~idempotent
      ~decide_both ~decide_left ~decide_right

  let diff =
    let name = Format.sprintf "Hptset(%s).diff" X.datatype_name in
    merge
      ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:false
      ~idempotent:false
      ~decide_both:(fun _ -> false)
      ~decide_left:Neutral
      ~decide_right:Absorbing

  let from_map m = from_shape (fun _ _ -> ()) m

  (* Partial application is needed because of caches *)
  let fold2_join_heterogeneous ~cache ~empty_left ~empty_right ~both ~join ~empty =
    let both k () v = both k v in
    fold2_join_heterogeneous ~cache ~empty_left ~empty_right ~both ~join ~empty

  let replace =
    let decide _k () () = () in
    replace_key ~decide

end

(* Test that implementation of function inter in Hptmap is correct *)
let%test _ =
  let module IdInt = struct include Datatype.Int let id = Fun.id end in
  let module Info = struct let initial_values = [[]] let dependencies = [] end in
  let module HSet = Make (IdInt) (Info) in
  let open HSet in
  let l = List.init 10 Fun.id in
  let s1 = List.fold_left (fun set i -> add i set) empty l in
  let s2 = List.fold_left (fun set i -> add (i+5) set) empty l in
  let i1 = fold (fun x acc -> if mem x s1 then add x acc else acc) s2 empty in
  let i2 = inter s1 s2 in
  i1 == i2
