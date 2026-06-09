(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Stdlib.Hashtbl

module type S = sig
  include S

  val bindings_sorted:
    ?cmp:(key -> key -> int) -> 'a t -> (key * 'a) list

  val iter_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter_sorted_by_entry:
    cmp:((key * 'a) -> (key * 'a) -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted_by_entry:
    cmp:((key * 'a) -> (key * 'a) -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter_sorted_by_value:
    cmp:('a -> 'a -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted_by_value:
    cmp:('a -> 'a -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val find_default: default:'a -> 'a t -> key  -> 'a

  val memo: 'a t -> key -> (key -> 'a) -> 'a

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
end

module Make(H: HashedType) : S with type key = H.t  = struct

  include Make(H)

  let bindings_sorted ?(cmp=Stdlib.compare) h =
    to_seq h |> List.of_seq |> List.fast_sort (fun (k1,_) (k2,_) -> cmp k1 k2)

  let fold_sorted ?(cmp=Stdlib.compare) f h acc =
    let l = bindings_sorted ~cmp h in
    List.fold_left (fun acc (k,v) -> f k v acc) acc l

  let iter_sorted ?cmp f h =
    fold_sorted ?cmp (fun k v () -> f k v) h ()

  let fold_sorted_by_entry ~cmp f h acc =
    let l = to_seq h |> List.of_seq |> List.fast_sort cmp in
    List.fold_left (fun acc (k,v) -> f k v acc) acc l

  let iter_sorted_by_entry ~cmp f h =
    fold_sorted_by_entry ~cmp (fun k v () -> f k v) h  ()

  let fold_sorted_by_value ~cmp f h acc =
    fold_sorted_by_entry ~cmp:(fun (_ka,va) (_kb,vb) -> cmp va vb) f h acc

  let iter_sorted_by_value ~cmp f h =
    iter_sorted_by_entry ~cmp:(fun (_ka,va) (_kb,vb) -> cmp va vb) f h

  let find_default ~default h k =
    match find_opt h k with
    | None -> default
    | Some v -> v

  let memo tbl k f =
    try find tbl k
    with Not_found ->
      let v = f k in
      add tbl k v;
      v

  let pretty
      ?(format=format_of_string "[[ %t ]]")
      ?(item=
        let mapsto = Format.asprintf "%t" Unicode.pp_maps_to in
        "%a @<1>" ^^ Scanf.format_from_string mapsto "" ^^ "@ %a")
      ?(sep=format_of_string ";@ ")
      ?(last=sep)
      ?(empty=format_of_string "[[]]")
      pp_key pp_val fmt m =
    let pp_item fmt (k,v) =
      Format.fprintf fmt item pp_key k pp_val v
    in
    Pretty.pretty_seq ~format ~item:"%a" ~sep ~last ~empty
      pp_item fmt (to_seq m)
end
