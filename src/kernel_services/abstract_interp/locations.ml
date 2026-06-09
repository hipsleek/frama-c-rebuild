(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Location_Bytes = Addresses.Bytes
module Location_Bits = Addresses.Bits

module Zone = Memory_zone


type t =
  { addr : Addresses.Bits.t;
    size : Z_or_top.t }

type access = Read | Write | Object_pointer | Any_pointer

let project_size = function
  | `Value size -> size
  | `Top -> Z.zero

(* Conversion into Base.access. A location valid for an access of unknown size
   must be at least valid for an empty access, so accesses of unknown sizes are
   converted into empty accesses. *)
let base_access ~size = function
  | Object_pointer -> Base.Object_pointer
  | Any_pointer -> Base.Any_pointer
  | Read -> Base.Read (project_size size)
  | Write -> Base.Write (project_size size)

exception Found_two

let valid_cardinal_zero_or_one ~for_writing {addr;size} =
  Addresses.Bits.equal Addresses.Bits.bottom addr ||
  let found_one =
    let already = ref false in
    function () ->
      if !already then raise Found_two;
      already := true
  in
  try
    match addr, size with
    | Addresses.Bits.Top _, _ -> false
    | _, `Top -> false
    | Addresses.Bits.Map m, `Value size ->
      Addresses.Bits.M.iter
        (fun base offsets ->
           if Base.is_weak base then raise Found_two;
           let access =
             if for_writing then Base.Write size else Base.Read size
           in
           let valid_offsets =
             Ival.narrow offsets (Base.valid_offset access base)
           in
           if Ival.cardinal_zero_or_one valid_offsets
           then begin
             if not (Ival.is_bottom valid_offsets)
             then found_one ()
           end
           else raise Found_two
        ) m;
      true
  with
  | Abstract_interp.Error_Top | Found_two -> false


let addr_bytes { addr } = Addresses.Bits.to_bytes addr
let size { size = size } = size

let make addr_bits size = { addr = addr_bits; size = size }

let is_valid access {addr; size} =
  not (Z_or_top.is_top size) &&
  let access = base_access ~size access in
  let is_valid_offset = Base.is_valid_offset access in
  match addr with
  | Top _ -> false
  | Map _ -> Addresses.Bits.for_all is_valid_offset addr


let filter_base f loc =
  { loc with addr = Addresses.Bits.filter_base f loc.addr }

let size_of_varinfo v =
  try Cil.bitsSizeOf v.vtype |> Z_or_top.of_int
  with Cil.SizeOfError (msg, _) ->
    Abstract_interp.feedback_approximation
      "imprecise size for variable %a (%s)" Printer.pp_varinfo v msg;
    Z_or_top.top

let of_varinfo v =
  let base = Base.of_varinfo v in
  make (Addresses.Bits.inject base Ival.zero) (size_of_varinfo v)

let of_base v =
  make (Addresses.Bits.inject v Ival.zero) (Base.bits_sizeof v)

let of_type_offset b typ offset =
  try
    let offs, size = Cil.bitsOffset typ offset in
    let size = Z_or_top.of_int size in
    make (Addresses.Bits.inject b (Ival.of_int offs)) size
  with Cil.SizeOfError _ as _e ->
    make (Addresses.Bits.inject b Ival.top) Z_or_top.top

let top = make Addresses.Bits.top Z_or_top.top
let bottom = make Addresses.Bits.bottom Z_or_top.top
let is_bottom l = Addresses.Bits.(equal l.addr bottom)

let cardinal_zero_or_one { addr ; size = size } =
  Addresses.Bits.cardinal_zero_or_one addr && not (Z_or_top.is_top size)

let equal_loc { addr = addr1 ; size = size1 } { addr = addr2 ; size = size2 } =
  Z_or_top.equal size1 size2 &&
  Addresses.Bits.equal addr1 addr2

let hash_loc { addr ; size } =
  Z_or_top.hash size + 317 * Addresses.Bits.hash addr

let compare_loc { addr = addr1 ; size = size1 } { addr = addr2 ; size = size2 } =
  let c1 = Z_or_top.compare size1 size2 in
  if c1 <> 0 then c1
  else Addresses.Bits.compare addr1 addr2

let pretty_loc fmt { addr ; size = size } =
  Format.fprintf fmt "%a (size:%a)"
    Addresses.Bits.pretty addr
    Z_or_top.pretty size

let pretty_english ~prefix fmt { addr = m ; size = size } =
  match m with
  | Addresses.Bits.Top (Base.SetLattice.Top,a) ->
    Format.fprintf fmt "somewhere unknown (origin:%a)"
      Origin.pretty a
  | Addresses.Bits.Top (s,a) ->
    Format.fprintf fmt "somewhere in %a (origin:%a)"
      Base.SetLattice.pretty s
      Origin.pretty a
  | Addresses.Bits.Map _ when Addresses.Bits.(equal m bottom) ->
    Format.fprintf fmt "nowhere"
  | Addresses.Bits.Map off ->
    let print_binding fmt (k, v) =
      ( match Ival.is_zero v, Base.validity k, size with
        | true, Base.Known (_,s1), `Value s2 when Z.equal (Z.succ s1) s2 ->
          Format.fprintf fmt "@[<h>%a@]" Base.pretty k
        | _ ->
          Format.fprintf fmt "@[<h>%a with offsets %a@]"
            Base.pretty k
            Ival.pretty v)
    in
    Pretty_utils.pp_iter
      ~pre:(if prefix then format_of_string "in " else "") ~suf:"" ~sep:";@,@ "
      (fun f -> Addresses.Bits.M.iter (fun k v -> f (k, v)))
      print_binding fmt off

(* Case [Top (Top, _)] must be handled by caller. *)
let enumerate_valid_bits_under_over under_over access {addr; size} =
  let access = base_access ~size access in
  let compute_offset base offs acc =
    let valid_offset = Ival.narrow offs (Base.valid_offset access base) in
    if Ival.is_bottom valid_offset then
      acc
    else
      let valid_itvs = under_over base valid_offset size in
      if Int_Intervals.(equal bottom valid_itvs) then acc
      else Memory_zone.add base valid_itvs acc
  in
  Addresses.Bits.fold_topset_ok compute_offset addr Memory_zone.bottom

let interval_from_ival_over _ offset size =
  Int_Intervals.from_ival_size offset size

let interval_from_ival_under base offset size =
  match Base.validity base with
  | Base.Variable { Base.weak = true } -> Int_Intervals.bottom
  | _ -> Int_Intervals.from_ival_size_under offset size

let enumerate_valid_bits access loc =
  match loc.addr with
  | Addresses.Bits.Top (Base.SetLattice.Top, _) -> Memory_zone.top
  | _ ->
    enumerate_valid_bits_under_over interval_from_ival_over access loc
;;

let enumerate_valid_bits_under access loc =
  match loc.size with
  | `Top -> Memory_zone.bottom
  | `Value _ ->
    match loc.addr with
    | Addresses.Bits.Top _ -> Memory_zone.bottom
    | Addresses.Bits.Map _ ->
      enumerate_valid_bits_under_over interval_from_ival_under access loc
;;

(** [valid_part l] is an over-approximation of the valid part
    of the location [l]. *)
let valid_part access ?(bitfield=true) {addr ; size } =
  let access = base_access ~size access in
  let compute_addr base offs acc =
    let valid_offset =
      Ival.narrow offs (Base.valid_offset access ~bitfield base)
    in
    if Ival.is_bottom valid_offset then
      acc
    else
      Addresses.Bits.add base valid_offset acc
  in
  let addr_bits =
    match addr with
    | Addresses.Bits.Top (Base.SetLattice.Top, _) ->
      addr
    | Addresses.Bits.Top (Base.SetLattice.Set _, _)
    | Addresses.Bits.Map _ ->
      Addresses.Bits.(fold_topset_ok compute_addr addr bottom)
  in
  make addr_bits size

let enumerate_bits_under_over under_over {addr; size} =
  let compute_offset base offs acc =
    let valid_offset = under_over base offs size in
    if Int_Intervals.(equal valid_offset bottom) then acc
    else Memory_zone.add base valid_offset acc
  in
  Addresses.Bits.fold_topset_ok compute_offset addr Memory_zone.bottom

let enumerate_bits loc =
  match loc.addr with
  | Addresses.Bits.Top (Base.SetLattice.Top, _) -> Memory_zone.top
  | _ -> enumerate_bits_under_over interval_from_ival_over loc

let enumerate_bits_under loc =
  match loc.addr, loc.size with
  | Addresses.Bits.Top _, _ | _, `Top -> Memory_zone.bottom
  | _ -> enumerate_bits_under_over interval_from_ival_under loc


let zone_of_varinfo var = enumerate_bits (of_varinfo var)

(** [invalid_part l] is an over-approximation of the invalid part
    of the location [l] *)
let invalid_part l = l (* TODO (but rarely useful) *)

let overlaps ~partial l1 l2 =
  try
    let size = Z.max (Z_or_top.project l1.size) (Z_or_top.project l2.size) in
    Addresses.Bits.overlaps ~partial ~size l1.addr l2.addr
  with Abstract_interp.Error_Top -> true

module Datatype_Input = struct
  include Datatype.Serializable_undefined
  type nonrec t = t
  let structural_descr =
    Structural_descr.t_record
      [| Addresses.Bits.packed_descr; Z_or_top.packed_descr |]
  let reprs =
    List.fold_left
      (fun acc l ->
         List.fold_left
           (fun acc n -> { addr = l; size = n } :: acc)
           acc
           Z_or_top.reprs)
      []
      Addresses.Bits.reprs
  let name = "Locations"
  let mem_project = Datatype.never_any_project
  let equal = equal_loc
  let compare = compare_loc
  let hash = hash_loc
  let pretty = pretty_loc
end

include (Datatype.Make (Datatype_Input) : Datatype.S with type t := t)

(* Deprecated alias *)

type location = t

let loc_top = top
let loc_bottom = bottom
let is_bottom_loc = is_bottom
let make_loc = make
let loc_size = size
let loc_equal = equal
let loc_of_varinfo = of_varinfo
let loc_of_base = of_base
let loc_of_typoffset = of_type_offset

let loc_bytes_to_loc_bits x = Addresses.Bits.of_bytes x
let loc_bits_to_loc_bytes x = Addresses.Bits.to_bytes x
let loc_bits_to_loc_bytes_under x = Addresses.Bits.to_bytes_under x
let loc_to_loc_without_size { addr } = Addresses.Bits.to_bytes addr
