(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Invalid
  | Set of Z.t list
  | Interval of Z.t * Z.t * Z.t
  | Overlap of Z.t * Z.t * Origin.t option

let pretty fmt = function
  | Invalid -> Format.fprintf fmt "Invalid"
  | Set l -> Format.fprintf fmt "Set [%a]"
               (Pretty_utils.pp_list ~sep:",@ " Z.pretty) l
  | Interval (mn, mx, modu) -> Format.fprintf fmt "Interval (%a,%a,%a)"
                                 Z.pretty mn Z.pretty mx Z.pretty modu
  | Overlap (mn, mx, o) -> Format.fprintf fmt "Overlap (%a,%a,%a)"
                             Z.pretty mn Z.pretty mx
                             (Pretty_utils.pp_opt Origin.pretty) o

(* Reduces [ival] for an access according to [validity]. *)
let reduce_offset_by_validity origin ival size validity =
  (* Reduces [ival] so that all accesses fit within [min] and [max]. *)
  let reduce_for_bounds min max =
    if Z.is_zero size
    then Set []
    else
      let max_valid = Z.sub max (Z.pred size) in
      let valid_range = Ival.inject_range (Some min) (Some max_valid) in
      let reduced_ival = Ival.narrow ival valid_range in
      match Ival.project_small_set reduced_ival with
      | Some l -> if l = [] then Invalid else Set l
      | None ->
        let min, max, _r, modu = Ival.min_max_r_mod reduced_ival in
        (* The bounds are finite thanks to the narrow with the valid range. *)
        let min = Option.get min and max = Option.get max in
        if Z.lt modu size
        then Overlap (min, Z.add max (Z.pred size), origin)
        else Interval (min, max, modu)
  in
  match validity with
  | Base.Invalid -> Invalid
  | Base.Empty -> Set []
  | Base.Known (min, max)
  | Base.Unknown (min, _, max) -> reduce_for_bounds min max
  | Base.Variable v -> reduce_for_bounds Z.zero v.Base.max_alloc

let trim_by_validity ?origin ival size validity =
  reduce_offset_by_validity origin ival size validity
