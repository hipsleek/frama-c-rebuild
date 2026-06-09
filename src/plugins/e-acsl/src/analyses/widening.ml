(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Analyses_types
open Interval_utils

let is_lower l1 l2 =
  match l1, l2 with
  | None, _ -> true
  | Some l1, Some l2 -> Z.compare l1 l2 <= 0
  | Some _, None -> false

let is_higher u1 u2 =
  match u1, u2 with
  | None, _ -> true
  | Some u1, Some u2 -> Z.compare u1 u2 >= 0
  | Some _, None -> false

let widen_ival_naive _ _ =
  top_ival

let widen_ival_default i1 i2 =
  try
    let kind = ikind_of_ival (Ival.join i1 i2) in
    let i = ival_of_ikind kind in
    let l1,u1 = Ival.min_and_max i1 in
    let l2,u2 = Ival.min_and_max i2 in
    let lmask = if is_lower l1 l2 then l1 else None in
    let umask = if is_higher u1 u2  then u1 else None in
    Ival (Ival.meet i (Ival.inject_range lmask umask))
  with Interval_utils.Not_representable_ival -> top_ival

let widen_ival_precise i1 i2 =
  let i = Ival.join i1 i2 in
  try ignore (ikind_of_ival i); Ival i
  with Interval_utils.Not_representable_ival -> top_ival

let chose_widen n i1 i2 =
  match n with
  | 0 -> widen_ival_naive i1 i2
  | 1 -> widen_ival_default i1 i2
  | 2 -> widen_ival_precise i1 i2
  | _ ->
    Options.fatal "argument unsupported: %s with value %i"
      Options.Widening_arguments.name
      n

let widen_ival ~arg name i1 i2 =
  if Options.Gmp_only.get ()
  then top_ival
  else
    let n =
      if arg
      then
        try Options.Widening_arguments.find name
        with Not_found -> Options.Widening_arguments_base.get ()
      else
        try Options.Widening_output.find name
        with Not_found -> Options.Widening_output_base.get ()
    in chose_widen n i1 i2

let widen ?(arg = false) li i1 i2 =
  match i1, i2 with
  | Ival i1, i2 when Ival.is_bottom i1 -> i2
  | Ival i1, Ival i2 -> widen_ival ~arg li.l_var_info.lv_name i1 i2
  | Float _, Float _ | Rational, Rational | Real, Real | Nan, Nan ->
    join i1 i2
  | Ival _, _| Float _, _ | Rational, _ | Real, _ | Nan, _ -> assert false

let widen_profile li profile new_profile =
  Cil_datatype.Logic_var.Map.mapi
    (fun lv ival ->
       let new_ival =
         match Cil_datatype.Logic_var.Map.find_opt lv new_profile with
         | None -> Ival Ival.bottom
         | Some i -> i
       in
       widen ~arg:true li ival new_ival)
    profile
