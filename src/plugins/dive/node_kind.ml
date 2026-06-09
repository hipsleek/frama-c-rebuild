(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Dive_types

module DatatypeInput =
struct
  include Datatype.Undefined

  open Cil_datatype

  type t = node_kind

  let (<?>) c (cmp,x,y) =
    if c = 0
    then cmp x y
    else c

  let name = "Node_kind"

  let reprs = [ Scalar (
      List.hd Varinfo.reprs,
      List.hd Typ.reprs,
      List.hd Offset.reprs) ]

  let compare k1 k2 =
    match k1, k2 with
    | Scalar (vi1, _, offset1), Scalar (vi2, _, offset2) ->
      Varinfo.compare vi1 vi2 <?> (OffsetStructEq.compare, offset1, offset2)
    | Scalar _, _ -> 1
    | _, Scalar _ -> -1
    | Composite vi1, Composite vi2 -> Varinfo.compare vi1 vi2
    | Composite _, _ -> 1
    | _, Composite _ -> -1
    | Scattered (lv1,stmt1), Scattered (lv2,stmt2) ->
      LvalStructEq.compare lv1 lv2 <?> (Stmt.compare, stmt1, stmt2)
    | Scattered _, _ -> 1
    | _, Scattered _ -> -1
    | Unknown (lv1,stmt1), Unknown (lv2,stmt2) ->
      LvalStructEq.compare lv1 lv2 <?> (Stmt.compare, stmt1, stmt2)
    | Unknown _, _ -> 1
    | _, Unknown _ -> -1
    | Alarm (stmt1, alarm1), Alarm (stmt2, alarm2) ->
      Stmt.compare stmt1 stmt2 <?> (Alarms.compare, alarm1, alarm2)
    | Alarm _, _ -> 1
    | _, Alarm _ -> -1
    | AbsoluteMemory, AbsoluteMemory -> 0
    | AbsoluteMemory, _ -> 1
    | _, AbsoluteMemory -> -1
    | Const e1, Const e2 ->
      Cil_datatype.Exp.compare e1 e2
    | Const _, _ -> 1
    | _, Const _ -> -1
    | Error s1, Error s2 ->
      Datatype.String.compare s1 s2

  let equal k1 k2 =
    match k1, k2 with
    | Scalar (vi1, _, offset1), Scalar (vi2, _, offset2) ->
      Varinfo.equal vi1 vi2 && OffsetStructEq.equal offset1 offset2
    | Composite vi1, Composite vi2 -> Varinfo.equal vi1 vi2
    | Scattered (lv1, stmt1), Scattered (lv2, stmt2) ->
      LvalStructEq.equal lv1 lv2 && Stmt.equal stmt1 stmt2
    | Unknown (lv1, stmt1), Unknown (lv2, stmt2) ->
      LvalStructEq.equal lv1 lv2 && Stmt.equal stmt1 stmt2
    | Alarm (stmt1, alarm1), Alarm (stmt2, alarm2) ->
      Stmt.equal stmt1 stmt2 && Alarms.equal alarm1 alarm2
    | AbsoluteMemory, AbsoluteMemory -> true
    | Const e1, Const e2 -> Cil_datatype.Exp.equal e1 e2
    | Error s1, Error s2 -> Datatype.String.equal s1 s2
    | _ -> false

  let hash k =
    match k with
    | Scalar (vi, _, offset) ->
      Hashtbl.hash (1, Varinfo.hash vi, OffsetStructEq.hash offset)
    | Composite vi -> Hashtbl.hash (2, Varinfo.hash vi)
    | Scattered (lv, stmt) ->
      Hashtbl.hash (3, LvalStructEq.hash lv, Stmt.hash stmt)
    | Unknown (lv, stmt) ->
      Hashtbl.hash (4, LvalStructEq.hash lv, Stmt.hash stmt)
    | Alarm (stmt, alarm) ->
      Hashtbl.hash (5, Stmt.hash stmt, Alarms.hash alarm)
    | AbsoluteMemory -> 6
    | Const e -> Hashtbl.hash (8, Cil_datatype.Exp.hash e)
    | Error s -> Hashtbl.hash (9, s)
end

include Datatype.Make (DatatypeInput)


let get_base = function
  | Scalar (vi,_,_) | Composite (vi) -> Some vi
  | Scattered _ | Unknown _ | Alarm _ | AbsoluteMemory
  | Const _ | Error _ -> None

let to_lval = function
  | Scalar (vi,_typ,offset) -> Some (Cil_types.Var vi, offset)
  | Composite (vi) -> Some (Cil_types.Var vi, Cil_types.NoOffset)
  | Scattered (lval,_) -> Some lval
  | Unknown (lval,_) -> Some lval
  | Alarm (_,_) | AbsoluteMemory | Const _ | Error _ -> None

let pretty fmt = function
  | (Scalar _ | Composite _ | Scattered _ | Unknown _) as kind ->
    Printer.pp_lval fmt (Option.get (to_lval kind))
  | Alarm (_stmt,alarm) ->
    Printer.pp_predicate fmt (Alarms.create_predicate alarm)
  | AbsoluteMemory ->
    Format.fprintf fmt "%s" (Kernel.AbsoluteValidRange.get ())
  | Const e ->
    Format.fprintf fmt "%a" Cil_printer.pp_exp e
  | Error s ->
    Format.fprintf fmt "%s" s
