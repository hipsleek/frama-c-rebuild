(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type deps = {
  data: Memory_zone.t;
  indirect: Memory_zone.t;
}

(* Pretty printing of detailed internal representation *)
let pretty_precise fmt {data; indirect} =
  let bottom_data = Memory_zone.is_bottom data in
  let bottom_indirect = Memory_zone.is_bottom indirect in
  match bottom_indirect, bottom_data with
  | true, true ->
    Format.fprintf fmt "\\nothing"
  | true, false ->
    Format.fprintf fmt "direct: %a"
      Memory_zone.pretty data
  | false, true ->
    Format.fprintf fmt "indirect: %a"
      Memory_zone.pretty indirect
  | false, false ->
    Format.fprintf fmt "indirect: %a; direct: %a"
      Memory_zone.pretty indirect
      Memory_zone.pretty data

(* Conversion to zone, used by default pretty printing *)
let to_zone d = Memory_zone.join d.data d.indirect


(* Datatype *)

module Prototype = struct
  include Datatype.Serializable_undefined

  type t = deps = {
    data: Memory_zone.t;
    indirect: Memory_zone.t;
  }
  [@@deriving eq,ord]

  let name = "Deps"
  let pretty fmt d = Memory_zone.pretty fmt (to_zone d)
  let hash fd = Hashtbl.hash Memory_zone.(hash fd.data, hash fd.indirect)
  let reprs = List.map (fun z -> {data = z; indirect = z}) Memory_zone.reprs
end

include Datatype.Make (Prototype)
include Prototype


(* Constructors *)

let bottom = {
  data = Memory_zone.bottom;
  indirect = Memory_zone.bottom;
}

let top = {
  data = Memory_zone.top;
  indirect = Memory_zone.top;
}

let data z = {
  data = z;
  indirect = Memory_zone.bottom;
}

let indirect z = {
  data = Memory_zone.bottom;
  indirect = z;
}


(* Mutators *)

let add_data d data =
  { d with data = Memory_zone.join d.data data }

let add_indirect d indirect =
  { d with indirect = Memory_zone.join d.indirect indirect }


(* Map *)

let map f d = {
  data = f d.data;
  indirect = f d.indirect;
}


(* Lattice *)

let is_included fd1 fd2 =
  Memory_zone.is_included fd1.data fd2.data &&
  Memory_zone.is_included fd1.indirect fd2.indirect

let join d1 d2 =
  if d1 == bottom then d2
  else if d2 == bottom then d1
  else {
    data = Memory_zone.join d1.data d2.data;
    indirect = Memory_zone.join d1.indirect d2.indirect;
  }

let narrow d1 d2 = {
  data = Memory_zone.narrow d1.data d2.data;
  indirect = Memory_zone.narrow d1.indirect d2.indirect;
}
