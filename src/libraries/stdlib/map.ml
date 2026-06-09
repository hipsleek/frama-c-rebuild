(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Stdlib.Map

module type S = sig
  include S
  val pretty :
    ?format:(Pretty.tformatter -> unit) Pretty.format ->
    ?item:(key Pretty.aformatter -> key -> 'a Pretty.aformatter -> 'a -> unit)
        Pretty.format ->
    ?sep:unit Pretty.format ->
    ?last:unit Pretty.format ->
    ?empty:unit Pretty.format ->
    (Format.formatter -> key -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit
  val closed_union : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
end

module Make (Ord : OrderedType) =
struct
  include Make (Ord)

  let pretty
      ?(format=format_of_string "{{ %t }}")
      ?(item=
        let mapsto = Format.asprintf "%t" Unicode.pp_maps_to in
        "%a @<1>" ^^ Scanf.format_from_string mapsto "" ^^ "@ %a")
      ?(sep=format_of_string ";@ ")
      ?(last=sep)
      ?(empty=format_of_string "{{}}")
      pp_key pp_val fmt m =
    let pp_item fmt (k,v) =
      Format.fprintf fmt item pp_key k pp_val v
    in
    Pretty.pretty_seq ~format ~item:"%a" ~sep ~last ~empty
      pp_item fmt (to_seq m)

  let closed_union f m1 m2 =
    union (fun k v1 v2 -> Some (f k v1 v2)) m1 m2
end
