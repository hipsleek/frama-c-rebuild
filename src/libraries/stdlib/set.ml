(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Stdlib.Set

module type S = sig
  include S
  val hash : (elt -> int) -> t -> int
  val pretty :
    ?format:(Pretty.tformatter -> unit) Pretty.format ->
    ?item:(elt Pretty.aformatter -> elt -> unit) Pretty.format ->
    ?sep:unit Pretty.format ->
    ?last:unit Pretty.format ->
    ?empty:unit Pretty.format ->
    (Format.formatter -> elt -> unit) ->
    Format.formatter -> t -> unit
  val pretty_text :
    ?format:(Pretty.tformatter -> unit) Pretty.format ->
    ?item:(elt Pretty.aformatter -> elt -> unit) Pretty.format ->
    ?sep:unit Pretty.format ->
    ?last:unit Pretty.format ->
    ?empty:unit Pretty.format ->
    (Format.formatter -> elt -> unit) ->
    Format.formatter -> t -> unit
end

module Make (Ord : OrderedType) =
struct
  include Make (Ord)

  let hash = Hash.hash_iter iter

  let pretty
      ?(format=format_of_string "{ %t }")
      ?(item=format_of_string "%a")
      ?(sep=format_of_string ";@ ")
      ?(last=sep)
      ?(empty=format_of_string "{}")
      pp_elt fmt l =
    Pretty.pretty_seq ~format ~item ~sep ~last ~empty pp_elt fmt (to_seq l)

  let pretty_text
      ?(format=format_of_string "%t")
      ?(item=format_of_string "%a")
      ?(sep=format_of_string ",@ ")
      ?(last=format_of_string "@ and@ ")
      ?(empty=format_of_string "<empty>")
      pp_elt fmt l =
    Pretty.pretty_seq ~format ~item ~sep ~last ~empty pp_elt fmt (to_seq l)
end
