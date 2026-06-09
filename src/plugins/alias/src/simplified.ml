(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

let nul_exp = Cil.kinteger64 ~loc:Fileloc.unknown ~repr:"0.." ~kind:IInt Z.zero
let is_nul_exp = Cil_datatype.ExpStructEq.equal nul_exp

module HL = Lval.Hashtbl
module HE = Exp.Hashtbl

let cached_lval = HL.create 23
let cached_exp = HE.create 37

let clear_cache () =
  HL.clear cached_lval;
  HE.clear cached_exp

exception Explicit_pointer_address of location

let check_cast_compatibility e to_type =
  let rec cast_preserves_indirection_level from_type to_type =
    let recurse = cast_preserves_indirection_level in
    match Ast_types.unroll_node from_type, Ast_types.unroll_node to_type with
    | TPtr from_type, TPtr to_type -> recurse from_type to_type
    | TPtr _, _ -> false
    | _, TPtr _ -> false
    | _ -> true
  in
  let from_type = Cil.typeOf e in
  if Cil.need_cast to_type from_type && not (Cil.isZero e)
     && not (cast_preserves_indirection_level from_type to_type) then
    (* emit a warning for unsafe casts, but not for the NULL pointer *)
    Options.warning
      ~once:true
      ~source:(fst @@ e.eloc)
      ~wkey:Options.Warn.unsafe_cast
      "unsafe cast from %a to %a; analysis may be unsound"
      Printer.pp_typ from_type Printer.pp_typ to_type

let rec simplify_offset o =
  match o with
  | NoOffset -> NoOffset
  | Field(f,o) -> Field(f, simplify_offset o)
  | Index(_e,o) -> Index(nul_exp, simplify_offset o)

let rec simplify_lval (h,o) =
  try HL.find cached_lval (h,o)
  with Not_found ->
    let res = (simplify_host h, simplify_offset o) in
    HL.add cached_lval (h,o) res;
    res

and simplify_host h =
  match h with
  | Var _ -> h
  | Mem e ->
    let simp_e = simplify_exp e in
    if is_nul_exp simp_e
    then raise (Explicit_pointer_address e.eloc)
    else Mem simp_e

and simplify_exp e =
  try
    HE.find cached_exp e
  with Not_found ->
    let res =
      match e.enode with
      | CastE (typ, e) ->
        check_cast_compatibility e typ;
        simplify_exp e
      | Lval lv -> {e with enode = Lval (simplify_lval lv)}
      | StartOf lv -> {e with enode = Lval (simplify_lval lv)}
      | AddrOf lv -> {e with enode = AddrOf (simplify_lval lv)}
      | BinOp(PlusPI, e1, _, _) | BinOp(MinusPI, e1, _, _) ->
        begin
          match (simplify_exp e1).enode with
          | Lval _ | AddrOf _ as node -> {e with enode = node}
          | _ -> raise (Explicit_pointer_address e1.eloc)
        end
      | _ -> e
    in
    HE.add cached_exp e res;
    Options.debug ~level:9 "simplify_exp %a = %a"
      Printer.pp_exp e Printer.pp_exp res;
    res

module LvalOrRef = struct
  type t = Lval of lval | Ref of lval

  let pretty l =
    let print f fmt x =
      match x with
      | Lval lv -> f fmt lv
      | Ref lv -> Format.fprintf fmt "&%a" f lv
    in
    print Printer.pp_lval l

  let from_exp e =
    let e = simplify_exp e in
    match e.enode with
      Lval lv -> Some (Lval lv)
    | AddrOf lv -> Some (Ref lv)
    | _ -> None
end

module Lval = struct
  let simplify = simplify_lval
end
