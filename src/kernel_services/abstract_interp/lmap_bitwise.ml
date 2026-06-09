(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lattice_bounds

exception Bitwise_cannot_copy

module type Location_map_bitwise = sig

  type v

  type map

  type lmap = Top | Map of map | Bottom

  include Datatype.S with type t = lmap
  include Lattice_type.Bottom_Bounded_Join_Semi_Lattice with type t := t
  include Lattice_type.With_Top with type t := t

  module LOffset :
    Offsetmap_bitwise_sig.S
    with type v = v
     and type intervals = Int_Intervals.t

  val is_empty : t -> bool
  val is_bottom : t -> bool
  val empty : t
  val empty_map: map

  val pretty_generic_printer:
    ?pretty_v: v Pretty_utils.formatter ->
    ?skip_v: (v -> bool) ->
    sep:string ->
    unit ->
    t Pretty_utils.formatter

  val pretty_debug: t Pretty_utils.formatter

  val add_binding : exact:bool -> t -> Memory_zone.t -> v -> t
  val add_binding_loc: exact:bool -> t -> Locations.t -> v -> t
  val add_base: Base.t -> LOffset.t -> t -> t
  val remove_base: Base.t -> t -> t

  val find : t -> Memory_zone.t -> v or_bottom
  val filter_base : (Base.t -> bool) -> t -> t

  val map: (v -> v) -> t -> t

  val fold : (Memory_zone.t -> v -> 'a -> 'a) -> map -> 'a -> 'a
  val fold_base : (Base.t -> LOffset.t -> 'a -> 'a) -> map -> 'a -> 'a
  val fold_fuse_same : (Memory_zone.t -> v -> 'a -> 'a) -> map -> 'a -> 'a

  val fold_join_zone:
    both:(Int_Intervals.t -> LOffset.t -> 'a) ->
    conv:(Base.t -> 'a -> 'b) ->
    empty_map:(Memory_zone.t -> 'b) ->
    join:('b -> 'b -> 'b) ->
    empty:'b ->
    Memory_zone.t -> map -> 'b

  val map2:
    cache:Hptmap_sig.cache_type -> symmetric:bool -> idempotent:bool
    -> empty_neutral:bool -> (LOffset.t -> LOffset.t -> LOffset.map2_decide) ->
    (v -> v -> v) -> map -> map -> map

  val shape: map -> LOffset.t Hptmap.Shape(Base.Base).t

  val clear_caches: unit -> unit
end

module type With_default = sig
  include Lattice_type.Join_Semi_Lattice
  include Lattice_type.With_Top with type t := t
  val default: t
  val default_is_bottom : bool
end


module Make_bitwise (V: With_default): Location_map_bitwise with type v = V.t =
struct

  exception Invalid_base

  module LOffset = struct

    include Offsetmap.Make_bitwise(V)
    let copy = Datatype.undefined

    let default_or_bottom b =
      let open Bottom.Operators in
      let+ size = size_from_validity (Base.validity b) in
      create ~size V.default

    let default b =
      match default_or_bottom b with
      | `Bottom -> raise Invalid_base
      | `Value result -> result

  end

  module LBase = struct

    module Hptmap_Info = struct
      let initial_values = []
      let dependencies = [ Ast.self ]
    end

    include Hptmap.Make (Base.Base) (LOffset) (Hptmap_Info)
    let () = Ast.add_monotonic_state self

    (* We override [add] so that the map is canonical: no key should be
       bound to its default value. *)
    let add b offsm m =
      let is_default =
        if Base.is_null b then LOffset.(equal (default Base.null)) offsm
        else LOffset.is_same_value offsm V.default
      in
      if is_default
      then remove b m
      else add b offsm m

    let find b m =
      try `Value (find b m) with Not_found -> LOffset.default_or_bottom b

  end


  type v    = V.t
  type map  = LBase.t
  type lmap = Top | Map of map | Bottom

  let empty_map = LBase.empty
  let empty = Map LBase.empty
  let bottom = Bottom

  let hash = function
    | Top -> 0
    | Bottom -> 17
    | Map x -> LBase.hash x

  let equal a b = match a,b with
    | Top,Top -> true
    | Map m1, Map m2 -> LBase.equal m1 m2
    | Bottom, Bottom -> true
    | (Top | Bottom | Map _),  _ -> false

  let compare_defined compare_lbase m1 m2 =
    match m1, m2 with
    | Bottom, Bottom | Top, Top -> 0
    | Map m1, Map m2 -> compare_lbase m1 m2
    | Bottom, (Top | Map _) | Map _, Top -> -1
    | Top, (Bottom | Map _) | Map _, Bottom -> 1

  let compare =
    if LBase.compare == Datatype.undefined
    then Datatype.undefined
    else compare_defined LBase.compare

  let pretty_generic_printer ?pretty_v ?skip_v ~sep () fmt m =
    match m with
    | Top -> Format.fprintf fmt "@[%sTOP@]" sep
    | Bottom -> Format.fprintf fmt "@[%sUNREACHABLE@]" sep
    | Map m ->
      let pp_one fmt (base, offs) =
        Format.fprintf fmt "@[%a@[<v>%a@]@]"
          Base.pretty base
          (LOffset.pretty_generic
             ?typ:(Base.typeof base) ?pretty_v ?skip_v ~sep ()) offs
      in
      Pretty_utils.pp_iter ~pre:"@[<v>" ~sep:"@ " ~suf:"@]"
        (fun f -> LBase.iter (fun base offset -> f (base, offset)))
        pp_one fmt m

  let pretty = pretty_generic_printer ~sep:"FROM" ()

  let pretty_debug fmt m =
    match m with
    | Top | Bottom -> pretty fmt m
    | Map m -> LBase.pretty_debug fmt m

  include Datatype.Make
      (struct
        type t = lmap
        let reprs = Top :: List.map (fun b -> Map b) LBase.reprs
        let structural_descr =
          Structural_descr.t_sum [| [| LBase.packed_descr |] |]
        let name = LOffset.datatype_name ^ " lmap_bitwise"
        let hash = hash
        let equal = equal
        let compare = compare
        let pretty = pretty
        let rehash = Datatype.identity
        let copy = Datatype.undefined
        let mem_project = Datatype.never_any_project
      end)


  let is_empty x = equal empty x
  let is_bottom x = x = Bottom

  let fold f m acc =
    let on_offset base itvs = f (Memory_zone.inject base itvs) in
    let on_base base = LOffset.fold (on_offset base) in
    LBase.fold on_base m acc

  let fold_base f m acc = LBase.fold f m acc

  let fold_fuse_same f m acc =
    let on_offset base itvs = f (Memory_zone.inject base itvs) in
    let f' base = LOffset.fold_fuse_same (on_offset base) in
    fold_base f' m acc

  let fold_join_zone ~both ~conv ~empty_map ~join ~empty =
    let cache = Hptmap_sig.PersistentCache "Lmap_bitwise.fold_on_zone" in
    let empty_left _ = empty (* zone over which to fold is empty *) in
    let empty_right z = empty_map z in
    let both b itvs map_b = conv b (both itvs map_b) in
    Memory_zone.fold2_join_heterogeneous
      ~cache ~empty_left ~empty_right ~both ~join ~empty

  let map f = function
    | Top -> Top
    | Bottom -> Bottom
    | Map m -> Map (LBase.map (fun m -> LOffset.map f m) m)

  let map2 ~cache ~symmetric ~idempotent ~empty_neutral fv f =
    let aux = LOffset.map2 cache fv f in
    let decide b om1 om2 = match om1, om2 with
      | None, None -> assert false (* decide is never called in this case *)
      | Some m1, None -> aux m1 (LOffset.default b)
      | None, Some m2 -> aux (LOffset.default b) m2
      | Some m1, Some m2 -> aux m1 m2
    in
    if empty_neutral
    then LBase.join ~symmetric ~idempotent ~cache ~decide:(fun _ m1 m2 -> aux m1 m2)
    else LBase.generic_join ~symmetric ~idempotent ~cache ~decide

  let filter_base f m =
    match m with
    | Top -> Top
    | Bottom -> Bottom
    | Map m ->
      let add_if k v acc = if f k then LBase.add k v acc else acc in
      Map (LBase.fold add_if m LBase.empty)


  let add_base_offset add v base offset m =
    let open Bottom.Operators in
    let validity = Base.validity base in
    let result =
      let* offsm = LBase.find base m in
      let+ new_offsm = add ~validity offset v offsm in
      LBase.add base new_offsm m
    in Bottom.value ~bottom:m result

  let add_binding ~exact m (loc:Memory_zone.t) v  =
    let add ~validity = LOffset.add_binding_intervals ~validity ~exact in
    let aux_base_offset = add_base_offset add v in
    match loc, m with
    | Memory_zone.Top (Base.SetLattice.Top, _), _ | _, Top -> Top
    | _, Bottom -> Bottom
    | _, Map m -> Map (Memory_zone.fold_topset_ok aux_base_offset loc m)

  let add_binding_loc ~exact m (loc: Locations.t) v =
    let size = loc.size in
    let add ~validity = LOffset.add_binding_ival ~validity ~exact ~size in
    let aux_base_offset = add_base_offset add v in
    match loc.addr, m with
    | Addresses.Bits.Top (Base.SetLattice.Top, _), _ | _, Top -> Top
    | _, Bottom -> Bottom
    | _, Map m ->
      Map (Addresses.Bits.fold_topset_ok aux_base_offset loc.addr m)

  let add_base b offsm = function
    | Bottom | Top as m -> m
    | Map m -> Map (LBase.add b offsm m)

  let remove_base b = function
    | Bottom | Top as m -> m
    | Map m -> Map (LBase.remove b m)

  let find m loc =
    match loc, m with
    | Memory_zone.Top _, _ | _, Top -> `Value V.top
    | _, Bottom -> `Bottom
    | Memory_zone.Map _, Map m ->
      let treat_offset base itvs acc =
        match Base.validity base with
        | Base.Invalid -> acc
        | validity ->
          let open Bottom.Operators in
          let* offsetmap = LBase.find base m in
          let v = LOffset.find_iset ~validity itvs offsetmap in
          Bottom.join V.join acc v
      in Memory_zone.fold_i treat_offset loc `Bottom


  let top = Top

  let join_on_map =
    let cache = Hptmap_sig.PersistentCache "lmap_bitwise.join" in
    let symmetric = true and idempotent = true in
    (* [join t Empty] is [t] if unbound bases are bound to [bottom] by default*)
    if V.default_is_bottom then
      let decide _ v1 v2 = LOffset.join v1 v2 in
      LBase.join ~cache ~decide ~symmetric ~idempotent
    else
      let get b = function Some v -> v | None -> LOffset.default b in
      let decide b v1 v2 = LOffset.join (get b v1) (get b v2) in
      LBase.generic_join ~cache ~symmetric ~idempotent ~decide

  let join m1 m2 =
    match m1, m2 with
    | Top, _ | _, Top -> Top
    | Bottom, m | m, Bottom -> m
    | Map m1, Map m2 -> Map (join_on_map m1 m2)

  let is_included_map =
    let name = Format.asprintf "Lmap_bitwise(%s).is_included" V.datatype_name in
    let decide_fst b offs1 = LOffset.is_included offs1 (LOffset.default b) in
    let decide_snd b offs2 = LOffset.is_included (LOffset.default b) offs2 in
    let decide_both _ offs1 offs2 = LOffset.is_included offs1 offs2 in
    LBase.binary_predicate (Hptmap_sig.PersistentCache name) LBase.UniversalPredicate
      ~decide_fast:LBase.decide_fast_inclusion
      ~decide_fst ~decide_snd ~decide_both

  let is_included m1 m2 =
    match m1, m2 with
    | _, Top -> true
    | Top ,_ -> false
    | Bottom, _ -> true
    | _, Bottom -> false
    | Map m1, Map m2 -> is_included_map m1 m2


  let shape x = x
  let clear_caches () = LBase.clear_caches () ; LOffset.clear_caches ()

end
