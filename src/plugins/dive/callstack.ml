(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Callsite = Datatype.Pair (Kernel_function) (Cil_datatype.Kinstr)
type call_site = Callsite.t

include Datatype.With_collections (Datatype.List (Callsite))

let init kf = [(kf,Kglobal)]

let pop cs =
  match cs with
  | [] | (_,Kglobal) :: _ :: _ | [(_,Kstmt _)] -> assert false (* Invariant *)
  | [(_,Kglobal)] -> None
  | (kf,Kstmt stmt) :: t -> Some (kf,stmt,t)

let top_kf cs =
  match cs with
  | [] | (_,Kglobal) :: _ :: _ | [(_,Kstmt _)] -> assert false (* Invariant *)
  | (kf,_) :: _ -> kf

let rec pop_downto top_kf = function
  | [] -> failwith "the callstack doesn't contain this function"
  | ((kf,_kinstr) :: tail) as cs ->
    if Kernel_function.equal kf top_kf
    then cs
    else pop_downto top_kf tail

let push (kf,stmt) cs =
  match cs with
  (* When the callstack is truncated, we ignore the first callsite *)
  | [] -> [(kf,Kglobal)]
  | cs -> (kf,Kstmt stmt) :: cs

let rec is_prefix cs1 cs2 =
  match cs1, cs2 with
  | [], _ -> true
  | _, [] -> false
  | [(kf,Kglobal)], (kf',_)::_ -> Kernel_function.equal kf kf'
  | _, [(_,Kglobal)] -> false
  | s1 :: t1, s2 :: t2 ->
    if Callsite.equal s1 s2
    then is_prefix t1 t2
    else false

let truncate_to_sub full_cs sub_cs =
  let rec aux acc = function
    | [] -> None
    | (s :: t) as cs ->
      if is_prefix sub_cs cs
      then Some (List.rev_append acc sub_cs)
      else aux (s :: acc) t
  in
  aux [] full_cs
