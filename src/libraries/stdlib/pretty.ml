(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type 'a aformatter = Format.formatter -> 'a -> unit
type tformatter = Format.formatter -> unit
type nonrec 'a format = ('a,Format.formatter,unit) format

(* This function is inspired by Pretty_utils functions and must be redefined
   here to avoid cyclic dependencies as Pretty_utils depends on Array and
   List. *)

let pretty_seq ~format ~item ~sep ?(last=sep) ?empty pp_item fmt seq =
  match Seq.uncons seq with
  | Some (first, remaining) ->
    let pretty_nonempty fmt =
      Format.fprintf fmt item pp_item first;
      match Seq.uncons remaining with
      | None -> () (* Only one element, already printed *)
      | Some (second, remaining) ->
        let pp previous current =
          Format.fprintf fmt sep;
          Format.fprintf fmt item pp_item previous;
          current
        in
        let last_elt = Seq.fold_left pp second remaining in
        Format.fprintf fmt last;
        Format.fprintf fmt item pp_item last_elt
    in
    Format.fprintf fmt format pretty_nonempty
  | None ->
    (* Empty sequence *)
    match empty with
    | None -> Format.fprintf fmt format ignore
    | Some empty -> Format.fprintf fmt empty
