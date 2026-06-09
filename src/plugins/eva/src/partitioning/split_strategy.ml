(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* To be completed with more involved strategies *)
type split_strategy =
  | NoSplit
  | SplitAuto
  | SplitEqList of Z.t list
  | FullSplit
[@@ deriving eq, ord]

include Datatype.Make (struct
    include Datatype.Serializable_undefined

    type t = split_strategy [@@ deriving eq, ord]
    let name = "Eva.Split_strategy"
    let reprs = [NoSplit]

    let hash = function
      | NoSplit -> 0
      | SplitAuto -> 1
      | FullSplit -> 2
      | SplitEqList l -> 3 + Hashtbl.hash (List.map Z.hash l)

    let pretty fmt = function
      | NoSplit -> Format.pp_print_string fmt "no split"
      | SplitAuto -> Format.pp_print_string fmt "auto split"
      | FullSplit -> Format.pp_print_string fmt "full split"
      | SplitEqList l ->
        Format.fprintf fmt "Split on \\result == %a"
          (Pretty_utils.pp_list ~sep:",@ " Z.pretty) l

    let copy = Datatype.identity
  end)

let of_string s =
  match s with
  | "" -> NoSplit
  | "full" -> FullSplit
  | "auto" -> SplitAuto
  | _ ->
    let r = Str.regexp ":" in
    let conv s =
      try Z.of_string s
      with Invalid_argument _ ->
        raise (Self.Cannot_build ("unknown split strategy " ^ s))
    in
    SplitEqList (List.map conv (Str.split r s))

let to_string = function
  | NoSplit -> ""
  | SplitAuto -> "auto"
  | FullSplit -> "full"
  | SplitEqList l ->
    Format.asprintf "%t"
      (fun fmt ->
         Pretty_utils.pp_list ~sep:":" Z.pretty fmt l)
