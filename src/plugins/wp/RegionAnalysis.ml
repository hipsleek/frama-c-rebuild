(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Proxy to Region Analysis for Region Model                          --- *)
(* -------------------------------------------------------------------------- *)

type region = Region.node

let get_map () =
  match WpContext.get_scope () with
  | Kf kf -> Region.map kf
  | Global -> Wp_parameters.not_yet_implemented "[region] logic context"

let id = Region.id
let of_id k =
  try Some (Region.of_id (get_map ()) k) with Invalid_argument _ -> None
let pretty = Region.pretty
let compare = Region.compare

module R =
struct
  type t = region
  let compare = compare
  let pretty = pretty
end

(* Keeping track of the decision to apply which memory model to each region *)
module Kind = WpContext.Generator(R)
    (struct
      open MemRegion
      let name = "Wp.RegionAnalysis.Kind"
      type key = region
      type data = kind
      let kind r p = if Region.singleton r then Single p else Many p
      let compile r =
        match Region.typed r with
        | Some ty ->
          begin
            match Ctypes.object_of ty with
            | C_int i -> kind r (Int i)
            | C_float f -> kind r (Float f)
            | C_pointer _ -> kind r Ptr
            | _ -> Garbled
          end
        | None -> Garbled
    end)

module Name = WpContext.Generator(R)
    (struct
      let name = "Wp.RegionAnalysis.Name"
      type key = region
      type data = string option
      let compile r =
        match Region.labels r with
        | label::_ -> Some label
        | [] ->
          match Region.cvars r with
          | v::_ -> Some v.vorig_name
          | _ -> None
    end)

let kind = Kind.get
let name = Name.get
let points_to region = Region.points_to region
let separated r1 r2 = Region.separated r1 r2
let included r1 r2 = Region.included r1 r2

let cvar var =
  try Some (Region.cvar (get_map ()) var)
  with Not_found -> None

let field r fd =
  try Some (Region.field r fd)
  with Not_found -> None

let shift r obj =
  try Some (Region.index r (Ctypes.object_to obj))
  with Not_found -> None

let literal ~eid _ = ignore eid ; None

let footprint r = Region.footprint r
