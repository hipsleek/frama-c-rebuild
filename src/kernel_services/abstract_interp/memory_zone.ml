(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Abstract_interp

module Info = struct
  let initial_values = []
  let dependencies = [ Ast.self ]
end

module M = Hptmap.Make (Base.Base) (Int_Intervals) (Info)
let () = Ast.add_monotonic_state M.self
let clear_caches = M.clear_caches

module MapLattice =
  Map_lattice.Make_Map_Lattice (Base) (Int_Intervals) (M)

type map_t = MapLattice.t
let find_or_bottom = MapLattice.find_or_bottom

include Map_lattice.Make_MapSet_Lattice
    (Base.Base) (Base.SetLattice) (Int_Intervals) (MapLattice)

let is_bottom = equal bottom
let is_top = equal top

let filter_base = filter_keys
let fold_bases = fold_keys
let fold_i f t acc = match t with
  | Top _ -> raise Error_Top
  | Map m -> MapLattice.fold f m acc
let fold_topset_ok = fold

let pretty fmt m =
  match m with
  | Top (Base.SetLattice.Top,a) ->
    Format.fprintf fmt "ANYTHING(origin:%a)"
      Origin.pretty a
  | Top (s,a) ->
    Format.fprintf fmt "Unknown(%a, origin:%a)"
      Base.SetLattice.pretty s
      Origin.pretty a
  | Map _ when equal m bottom ->
    Format.fprintf fmt "\\nothing"
  | Map off ->
    let print_binding fmt (k, v) =
      Format.fprintf fmt "@[<h>%a%a@]"
        Base.pretty k
        (Int_Intervals.pretty_typ (Base.typeof k)) v
    in
    Pretty_utils.pp_iter ~pre:"" ~suf:"" ~sep:";@,@ "
      (fun f -> M.iter (fun k v -> f (k, v))) print_binding fmt off

let valid_intersects = intersects

let mem_base b = function
  | Top (top_param, _) ->
    Base.SetLattice.mem b top_param
  | Map m -> M.mem b m

let get_bases = get_keys

let of_bases bases =
  let f base _ =
    match Base.bits_sizeof base with
    | `Top -> Int_Intervals.top
    | `Value size -> Int_Intervals.inject_bounds Z.zero size
  in
  Map (M.from_shape f bases)

let shape x = x

let fold2_join_heterogeneous ~cache ~empty_left ~empty_right ~both ~join ~empty =
  let f_top =
    (* Build a zone corresponding to the garbled mix. Do not add NULL, we
       are reasoning on zones. Inefficient if empty_right does not use
       its argument, though... *)
    let build_z set =
      let aux b z = M.add b Int_Intervals.top z in
      Map (Base.Hptset.fold aux set M.empty)
    in
    let empty_right set = empty_right (build_z set) in
    let both base v = both base Int_Intervals.top v in
    Base.SetLattice.O.fold2_join_heterogeneous
      ~cache ~empty_left ~empty_right ~both ~join ~empty
  in
  let f_map =
    let empty_right m = empty_right (Map m) in
    let both base itvs v = both base itvs v in
    M.fold2_join_heterogeneous
      ~cache ~empty_left ~empty_right ~both ~join ~empty
  in
  fun z ->
    match z with
    | Top (Base.SetLattice.Top, _) -> raise Error_Top
    | Top (Base.SetLattice.Set s, _) -> f_top s
    | Map mm -> f_map mm
