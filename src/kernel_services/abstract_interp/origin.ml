(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type kind =
  | Misalign_read
  | Misalign_write
  | Leaf
  | Merge
  | Arith

let kind_rank = function
  | Misalign_read -> 0
  | Misalign_write -> 1
  | Leaf -> 2
  | Merge -> 3
  | Arith -> 4

let kind_label = function
  | Misalign_read -> "Misaligned read"
  | Misalign_write -> "Misaligned write"
  | Leaf -> "Library function"
  | Merge -> "Merge"
  | Arith -> "Arithmetic"

(* Ideally, we would use the current statement sid instead of the current
   source file location. However, that would require passing the current
   statement through any datastructures and operations that can create
   garbled mixes, which would be an invasive change. *)
type location = Fileloc.t

type tt =
  | Origin of { kind: kind; loc: location; }
  | Well
  | Unknown

module Prototype = struct
  include Datatype.Serializable_undefined
  type t = tt
  let name = "Origin"
  let reprs = [ Unknown ]

  let compare t1 t2 =
    match t1, t2 with
    | Origin o1, Origin o2 ->
      if o1.kind = o2.kind
      then Fileloc.compare o1.loc o2.loc
      else kind_rank o2.kind - kind_rank o1.kind
    | Well, Well | Unknown, Unknown -> 0
    | Origin _, _ | Well, Unknown -> 1
    | _, Origin _ | Unknown, Well -> -1

  let equal = Datatype.from_compare

  let hash = function
    | Well -> 0
    | Unknown -> 1
    | Origin { kind; loc; } ->
      Hashtbl.hash (kind_rank kind, Fileloc.hash loc) + 2

  let pretty fmt = function
    | Well -> Format.fprintf fmt "Well"
    | Unknown -> Format.fprintf fmt "Unknown"
    | Origin { kind; loc; } ->
      let pretty_loc = Fileloc.pretty in
      Format.fprintf fmt "%s@ {%a}" (kind_label kind) pretty_loc loc
end

include Datatype.Make_with_collections (Prototype)

let pretty_as_reason fmt origin =
  if origin <> Unknown
  then Format.fprintf fmt " because of %a" pretty origin

let descr = function
  | Unknown -> "unknown origin"
  | Well -> "well in initial state"
  | Origin { kind } ->
    match kind with
    | Misalign_read -> "misaligned read of addresses"
    | Misalign_write -> "misaligned write of addresses"
    | Leaf -> "assigns clause on addresses"
    | Merge -> "imprecise merge of addresses"
    | Arith -> "arithmetic operation on addresses"


let current kind = Origin { kind; loc = Current_loc.get (); }

let well = Well
let unknown = Unknown
let is_unknown t = t = Unknown


(* For each garbled mix origin, keep track of:
   - a unique id used to keep the oldest origin when merging two garbled mix.
   - the number of writes (according to [register_write] below), i.e. the number
     of times a garbled mix with this origin has been written in a state during
     the analysis.
   - the number of reads (according to [register_read] below), i.e. the number
     of times a garbled mix with this origin is used by the analysis.
     Only reads at a different location than that of the origin are counted:
     if a garbled mix is only used where it has been created, it has no more
     impact on the analysis precision than any other imprecise value.
   - the set of bases related to garbled mix with this origin.

   These info are printed at the end of an Eva analysis. *)

module LocSet = Fileloc.Set

module History = struct

  module Counter = State_builder.Counter (struct let name = "Origin.Id.Counter" end)

  (* Locations of writes, locations of reads, related bases. *)
  module Data = Datatype.Triple (LocSet) (LocSet) (Base.SetLattice)
  module IdData = Datatype.Pair (Datatype.Int) (Data)

  module History_Info = struct
    let name = "Origin.History"
    let dependencies = []
    let size = 10
  end

  module Table = State_builder.Hashtbl (Hashtbl) (IdData) (History_Info)

  let create () =
    let id = Counter.next () in
    id, (LocSet.empty, LocSet.empty, Base.SetLattice.bottom)

  let register origin =
    if not (Table.mem origin)
    then Table.replace origin (create ())

  let apply ~change origin =
    let id, data = try Table.find origin with Not_found -> create () in
    Table.replace origin (id, change data)

  let cardinal_writes origin =
    try let (_, (w, _r, _b)) = Table.find origin in LocSet.cardinal w
    with Not_found -> 0

  let id origin =
    try fst (Table.find origin)
    with Not_found -> Stdlib.max_int

  let to_list () = List.of_seq (Table.to_seq ())

  let clear () = Counter.reset (); Table.clear ()
end

let clear () = History.clear ()

(* Keep the oldest known origin: it is probably the most relevant origin, as
   subsequent ones may have been created because of the first. *)
let join t1 t2 =
  if equal t1 t2 then t1 else
    match t1, t2 with
    | Unknown, x | x, Unknown -> x
    | Well, _ | _, Well -> Well
    | Origin _, Origin _ -> if History.id t1 < History.id t2 then t1 else t2

let is_current = function
  | Unknown | Well -> false
  | Origin { loc } -> Fileloc.equal loc (Current_loc.get ())

(* Registers the creation of the garbled mix of some origins: Misalign_write and
   Merge, which are directly written in the cvalue states, and Arith which can
   be created by many cvalue operations.
   Does not register:
   - Leaf origins, as garbled mix are often created by assigns clause and then
     reduced to precise values by postconditions. Garbled mix with leaf origin
     are thus registered by [register_write] called at the end of the
     interpretation of a function specification.
   - Misalign_read, which should be registered as a read by the cvalue domain.
     This avoids prioritizing garbled mix created by the bitwise domain.
   - Unknown and Well origins, which are processed separately by the [join]. *)
let register _bases t =
  match t with
  | Origin { kind = Misalign_write | Merge | Arith } -> History.register t
  | Origin { kind = Leaf | Misalign_read } | Unknown | Well -> ()

(* Registers a write of a garbled mix of known origin.
   Returns true if it is related to the current location. *)
let register_write bases t =
  if is_unknown t then false else
    let current_loc = Current_loc.get () in
    let is_new = History.cardinal_writes t = 0 in
    let change (w, r, b) =
      LocSet.add current_loc w, r, Base.SetLattice.join b bases
    in
    History.apply ~change t;
    is_new && is_current t

(* Registers a read only if the current location is not that of the origin. *)
let register_read bases t =
  if not (is_unknown t || is_current t) then
    let current_loc = Current_loc.get () in
    let change (w, r, b) =
      w, LocSet.add current_loc r, Base.SetLattice.join b bases
    in
    History.apply ~change t

(* Returns the list of recorded origins, sorted by number of reads.
   Origins with no reads are filtered. *)
let get_history () =
  let list = History.to_list () in
  let count (origin, (_id, (w, r, bases))) =
    if LocSet.is_empty r
    then None
    else Some (origin, (LocSet.cardinal w, LocSet.cardinal r, bases))
  in
  let list = List.filter_map count list in
  let cmp (origin1, (_, r1, _)) (origin2, (_, r2, _)) =
    let r = r2 - r1 in
    if r <> 0 then r else compare origin1 origin2
  in
  List.sort cmp list

let pretty_origin fmt origin =
  match origin with
  | Unknown -> Format.fprintf fmt "Unknown origin"
  | Well -> Format.fprintf fmt "Initial state"
  | Origin { loc } ->
    Format.fprintf fmt "%a: %s"
      Fileloc.pretty loc (descr origin)

let pretty_history fmt =
  let list = get_history () in
  let plural count = if count = 1 then "" else "s" in
  let pp_origin fmt (origin, (w, r, bases)) =
    let bases = Base.SetLattice.filter (fun b -> not (Base.is_null b)) bases in
    Format.fprintf fmt
      "@[<hov 2>%a@ (read in %i statement%s, propagated through %i statement%s)@ \
       garbled mix of &%a@]"
      pretty_origin origin r (plural r) w (plural w) Base.SetLattice.pretty bases
  in
  if list <> [] then
    Format.fprintf fmt
      "@[<v 2>Origins of garbled mix generated during analysis:@,%a@]"
      (Pretty_utils.pp_list ~sep:"@," pp_origin) list
