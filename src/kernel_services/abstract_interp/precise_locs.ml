(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type precise_offset =
  | POBottom (* No offset *)
  | POZero (* Offset zero *)
  | POSingleton of Z.t (* Single offset *)
  | POPrecise of Ival.t * (Z.t (* cardinal *))
  (* Offset exactly represented by an ival *)
  | POImprecise of Ival.t (* Offset that could not be represented precisely *)
  | POShift of (* Shifted offset *)
      Ival.t (* number of bits/bytes to shift *) *
      precise_offset *
      Z.t (* cardinal*)

(* Cardinals are over-approximated: the combination [{0, 1} + {0, 1}]
   is considered as having cardinal 4 instead of 3. POBottom is the
   only way to represent Bottom (ie [POImprecise Ival.bottom] is
   forbidden). Other invariants, ie. [POSingleton i] means that [i] is
   non-zero, are not required for correction -- only for performance. *)


let rec pretty_offset fmt = function
  | POBottom -> Format.fprintf fmt "<Bot>"
  | POZero -> Format.fprintf fmt "<0>"
  | POSingleton i -> Format.fprintf fmt "<%a>_0" Z.pretty i
  | POPrecise (po, _) -> Format.fprintf fmt "<%a>p" Ival.pretty po
  | POImprecise po -> Format.fprintf fmt "<%a>i" Ival.pretty po
  | POShift (i, po, _) ->
    Format.fprintf fmt "<%a+%a>" pretty_offset po Ival.pretty i

let rec equal_offset o1 o2 = match o1, o2 with
  | POBottom, POBottom -> true
  | POZero, POZero -> true
  | POSingleton i1, POSingleton i2 -> Z.equal i1 i2
  | POPrecise (i1, _), POPrecise (i2, _) -> Ival.equal i1 i2
  | POImprecise i1, POImprecise i2 -> Ival.equal i1 i2
  | POShift (shift1, o1, _), POShift (shift2, o2, _) ->
    Ival.equal shift1 shift2 && equal_offset o1 o2
  | _, _ -> false

let offset_zero = POZero
let offset_bottom = POBottom
let offset_top = POImprecise Ival.top

let is_bottom_offset off = off = POBottom

let cardinal_zero_or_one_offset = function
  | POBottom | POZero | POSingleton _ -> true
  | POPrecise (_, c) | POShift (_, _, c) -> Z.leq c Z.one
  | POImprecise _ -> false


let small_cardinal c = Z.leq c (Z.of_int (Offsetmap.get_plevel ()))

let _cardinal_offset = function
  | POBottom -> Some Z.zero
  | POZero | POSingleton _ -> Some Z.one
  | POPrecise (_, c) -> Some c
  | POImprecise _ -> None
  | POShift (_, _, c) -> Some c

let rec imprecise_offset = function
  | POBottom -> Ival.bottom
  | POZero -> Ival.zero
  | POSingleton i -> Ival.inject_singleton i
  | POPrecise (i, _) | POImprecise i -> i
  | POShift (shift, po, _) -> Ival.add_int shift (imprecise_offset po)

let rec _scale_offset scale po =
  assert (Z.gt scale Z.zero);
  match po with
  | POBottom -> POBottom
  | POZero -> POZero
  | POSingleton i -> POSingleton (Z.mul i scale)
  | POPrecise (i, c) -> POPrecise (Ival.scale scale i, c)
  | POImprecise i -> POImprecise (Ival.scale scale i)
  | POShift (shift, po, c) ->
    POShift (Ival.scale scale shift, _scale_offset scale po, c)

let shift_offset_by_singleton shift po =
  if Z.is_zero shift then
    po
  else
    match po with
    | POBottom -> POBottom
    | POZero -> POSingleton shift
    | POSingleton i -> POSingleton (Z.add i shift)
    | POPrecise (i, c) -> POPrecise (Ival.add_singleton_int shift i, c)
    | POImprecise i -> POImprecise (Ival.add_singleton_int shift i)
    | POShift (shift', po, c) ->
      POShift (Ival.add_singleton_int shift shift', po, c)

let inject_ival ival =
  if Ival.is_bottom ival then POBottom
  else
    match Ival.cardinal ival with
    | Some c when small_cardinal c ->
      if Z.is_one c then
        let i = Ival.project_int ival in
        if Z.is_zero i then POZero else POSingleton (Ival.project_int ival)
      else
        POPrecise (ival, c)
    | _ -> POImprecise ival

let shift_offset shift po =
  if Ival.is_bottom shift then
    POBottom
  else
    match po with
    | POBottom -> POBottom

    | POZero -> inject_ival shift

    | POImprecise i -> POImprecise (Ival.add_int shift i)

    | POSingleton i ->
      (match Ival.cardinal shift with
       | Some c when small_cardinal c ->
         if Z.is_one c then
           POSingleton (Z.add (Ival.project_int shift) i)
         else
           POPrecise (Ival.add_singleton_int i shift, c)
       | _ -> POImprecise (Ival.add_int shift (imprecise_offset po)))

    | POPrecise (_i, cpo) ->
      (match Ival.cardinal shift with
       | Some cs ->
         let new_card = Z.mul cs cpo in
         if small_cardinal new_card then
           POShift (shift, po, new_card) (* may be a POPrecise depending
                                            on ilevel *)
         else
           POImprecise (Ival.add_int shift (imprecise_offset po))
       | None ->
         POImprecise (Ival.add_int shift (imprecise_offset po)))

    | POShift (_shift', _po', cpo) ->
      (match Ival.cardinal shift with
       | Some cs ->
         let new_card = Z.mul cs cpo in
         if small_cardinal new_card then
           POShift (shift, po, new_card) (* may be a single POShift depending
                                            on the cardinals of shift/shift'*)
         else
           POImprecise (Ival.add_int shift (imprecise_offset po))
       | None ->
         POImprecise (Ival.add_int shift (imprecise_offset po)))

type precise_addr_bits =
  | PLBottom
  | PLLoc of Addresses.Bits.t
  | PLVarOffset of Base.t * precise_offset
  | PLLocOffset of Addresses.Bits.t * precise_offset
type precise_location_bits = precise_addr_bits

let pretty_addr_bits fmt = function
  | PLBottom -> Format.fprintf fmt "[Bot]"
  | PLLoc loc -> Format.fprintf fmt "[%a]" Addresses.Bits.pretty loc
  | PLVarOffset (b, po) ->
    Format.fprintf fmt "[%a+%a]" Base.pretty b pretty_offset po
  | PLLocOffset (loc, po) ->
    Format.fprintf fmt "[%a+%a]" Addresses.Bits.pretty loc pretty_offset po
let pretty_loc_bits = pretty_addr_bits

let equal_addr_bits l1 l2 = match l1, l2 with
  | PLBottom, PLBottom -> true
  | PLLoc l1, PLLoc l2 -> Addresses.Bits.equal l1 l2
  | PLVarOffset (b1, o1), PLVarOffset (b2, o2) ->
    Base.equal b1 b2 && equal_offset o1 o2
  | PLLocOffset (l1, o1), PLLocOffset (l2, o2) ->
    Addresses.Bits.equal l1 l2 && equal_offset o1 o2
  | _, _ -> false

let bottom_addr_bits = PLBottom
let bottom_location_bits = bottom_addr_bits

let cardinal_zero_or_one_addr_bits = function
  | PLBottom -> true
  | PLLoc loc -> Addresses.Bits.cardinal_zero_or_one loc
  | PLVarOffset (_, po) -> cardinal_zero_or_one_offset po
  | PLLocOffset (loc, po) ->
    Addresses.Bits.cardinal_zero_or_one loc && cardinal_zero_or_one_offset po

let inject_addr_bits loc =
  if Addresses.Bits.is_bottom loc then PLBottom else PLLoc loc
let inject_location_bits = inject_addr_bits

let combine_base_precise_offset base po =
  match po with
  | POBottom -> PLBottom
  | _ -> PLVarOffset (base, po)

let combine_addr_precise_offset loc po =
  try
    let base, ival = Addresses.Bits.find_lonely_key loc in
    begin match shift_offset ival po with
      | POBottom -> PLBottom
      | po -> PLVarOffset (base, po)
    end
  with Not_found ->
  match po with
  | POBottom      -> PLBottom
  | POZero        -> PLLoc loc
  | POImprecise i -> PLLoc (Addresses.Bits.shift i loc)
  | POSingleton i -> PLLoc (Addresses.Bits.shift (Ival.inject_singleton i) loc)
  | POPrecise (i, _c) when Addresses.Bits.cardinal_zero_or_one loc ->
    PLLoc (Addresses.Bits.shift i loc)
  | POPrecise (_, c) | POShift (_, _, c) ->
    match Addresses.Bits.cardinal loc with
    | Some card when small_cardinal (Z.mul card c) -> PLLocOffset (loc, po)
    | _ -> PLLoc (Addresses.Bits.shift (imprecise_offset po) loc)
let combine_loc_precise_offset = combine_addr_precise_offset


let imprecise_addr_bits = function
  | PLBottom -> Addresses.Bits.bottom
  | PLLoc l -> l
  | PLVarOffset (b, po) -> Addresses.Bits.inject b (imprecise_offset po)
  | PLLocOffset (loc, po) -> Addresses.Bits.shift (imprecise_offset po) loc
let imprecise_location_bits = imprecise_addr_bits

type precise_location = {
  addr: precise_addr_bits;
  size: Z_or_top.t
}

let equal_loc pl1 pl2 =
  equal_addr_bits pl1.addr pl2.addr && Z_or_top.equal pl1.size pl2.size

let imprecise_location pl =
  Locations.make (imprecise_addr_bits pl.addr) pl.size

let make_precise_loc addr ~size = { addr; size }

let loc_size loc = loc.size

let loc_bottom = {
  addr = PLBottom;
  size = Z_or_top.top;
}
let is_bottom_loc pl = pl.addr = PLBottom

let loc_top = {
  addr = PLLoc Addresses.Bits.top;
  size = Z_or_top.top;
}
let is_top_loc pl = equal_loc loc_top pl

let replace_base substitution po =
  match po.addr with
  | PLBottom -> po
  | PLLoc loc ->
    let modified, loc = Addresses.Bits.replace_base substitution loc in
    if modified then { po with addr = PLLoc loc } else po
  | PLVarOffset (base, offset) ->
    begin
      try
        let base = Base.Hptshape.find_check_missing base substitution in
        { po with addr = PLVarOffset (base, offset) }
      with Not_found -> po
    end
  | PLLocOffset (loc, offset) ->
    let modified, loc = Addresses.Bits.replace_base substitution loc in
    if modified
    then { po with addr = PLLocOffset (loc, offset) }
    else po

let rec fold_offset f po acc =
  match po with
  | POBottom -> f Ival.bottom acc
  | POZero -> f Ival.zero acc
  | POSingleton i -> f (Ival.inject_singleton i) acc
  | POPrecise (iv, _) | POImprecise iv -> f iv acc
  | POShift (shift, po', _) ->
    let aux_po ival acc =
      let aux_ival shift_i acc =
        let ival' = Ival.add_singleton_int shift_i ival in
        f ival' acc
      in
      Ival.fold_int aux_ival shift acc
    in
    fold_offset aux_po po' acc

let fold f pl acc =
  match pl.addr with
  | PLBottom -> acc
  | PLLoc l -> f (Locations.make l pl.size) acc
  | PLVarOffset (b, po) ->
    let aux_po ival acc =
      let loc_b = Addresses.Bits.inject b ival in
      let loc = Locations.make loc_b pl.size in
      f loc acc
    in
    fold_offset aux_po po acc
  | PLLocOffset (loc, po) ->
    let aux_po ival_po acc =
      let aux_loc b ival_loc acc =
        let aux_ival_loc i acc =
          let ival = Ival.add_singleton_int i ival_po in
          let loc_b = Addresses.Bits.inject b ival in
          let loc = Locations.make loc_b pl.size in
          f loc acc
        in
        Ival.fold_int aux_ival_loc ival_loc acc
      in
      Addresses.Bits.fold_i aux_loc loc acc
    in
    fold_offset aux_po po acc

let enumerate_valid_bits access loc =
  let aux loc z = Memory_zone.join z (Locations.enumerate_valid_bits access loc) in
  fold aux loc Memory_zone.bottom


let cardinal_zero_or_one pl =
  not (Z_or_top.is_top pl.size) && cardinal_zero_or_one_addr_bits pl.addr

let valid_cardinal_zero_or_one ~for_writing pl =
  match pl.addr with
  | PLBottom -> true
  | PLLoc lb ->
    let loc = Locations.make lb pl.size in
    Locations.valid_cardinal_zero_or_one ~for_writing loc
  | _ ->
    try
      ignore
        (fold (fun loc found_one ->
             let access = Locations.(if for_writing then Write else Read) in
             let valid = Locations.valid_part access loc in
             if Locations.is_bottom loc then found_one
             else
             if Locations.cardinal_zero_or_one valid then
               if found_one then raise Exit else true
             else raise Exit
           ) pl false);
      true
    with Exit -> false

let pretty_loc fmt loc =
  Format.fprintf fmt "%a (size:%a)"
    pretty_addr_bits loc.addr Z_or_top.pretty loc.size


let rec reduce_offset_by_range range offset = match offset with
  | POBottom -> offset
  | POZero -> if Ival.contains_zero range then offset else POBottom
  | POSingleton i ->
    let i = Ival.inject_singleton i in
    if Ival.is_included i range then offset else POBottom
  | POPrecise (ival, card) ->
    let ival = Ival.narrow range ival in
    if Ival.is_bottom ival then POBottom else POPrecise (ival, card)
  | POImprecise ival ->
    let ival = Ival.narrow range ival in
    if Ival.is_bottom ival then POBottom else POImprecise ival
  | POShift (shift, offset, card) ->
    let range = Ival.sub_int range shift in
    let offset = reduce_offset_by_range range offset in
    if offset = POBottom then offset else POShift (shift, offset, card)

let reduce_offset_by_validity ~bitfield access size base offset =
  let access = Locations.base_access ~size access in
  let range = Base.valid_offset ~bitfield access base in
  if Ival.is_bottom range then POBottom else reduce_offset_by_range range offset

let reduce_by_valid_part access ~bitfield precise_loc size =
  match precise_loc with
  | PLBottom -> precise_loc
  | PLLoc addr ->
    let loc = Locations.make addr size in
    PLLoc Locations.((valid_part access ~bitfield loc).Locations.addr)
  | PLVarOffset (base, offset) ->
    begin
      match reduce_offset_by_validity ~bitfield access size base offset with
      | POBottom -> PLBottom
      | offset -> PLVarOffset (base, offset)
    end
  | PLLocOffset (_loc, _offset) ->
    (* Reduction is difficult in this case, because we must take into account
       simultaneously [loc] and [offset]. We do nothing for the time being. *)
    precise_loc

let valid_part access ~bitfield {addr; size} =
  { addr = reduce_by_valid_part ~bitfield access addr size;
    size = size }
