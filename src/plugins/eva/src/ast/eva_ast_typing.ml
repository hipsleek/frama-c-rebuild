(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast_types

let type_of_const : constant -> typ = function
  | CTopInt ik -> Cil_const.mk_tint ik
  | CInt64 (_, ik, _) -> Cil_const.mk_tint ik
  | CChr _ -> Cil_const.intType
  | CReal (_, fk, _) -> Cil_const.mk_tfloat fk
  | CEnum (_ei, e) -> e.typ

let rec type_of_offset (basetyp : typ) : offset -> typ = function
  | NoOffset -> basetyp
  | Index (_, o) ->
    type_of_offset (Ast_types.direct_element_type basetyp) o
  | Field (fi, o) ->
    let base_attrs = (Ast_types.unroll basetyp).tattr in
    let base_attrs = Ast_attributes.filter_qualifiers base_attrs in
    let base_attrs =
      if Ast_attributes.(contains frama_c_mutable fi.fattr) then
        Ast_attributes.drop "const" base_attrs
      else
        base_attrs
    in
    type_of_offset (Ast_types.add_attributes base_attrs fi.ftype) o

let type_of_lhost : lhost -> typ = function
  | Var vi -> vi.vtype
  | Mem addr -> Ast_types.direct_pointed_type addr.typ

let type_of_lval_node (host, offset : lval_node) : typ =
  let basetyp = type_of_lhost host in
  type_of_offset basetyp offset

let type_of_exp_node : exp_node -> typ = function
  | Const c -> type_of_const c
  | Lval lv -> Ast_types.remove_qualifiers lv.typ
  | UnOp (_, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | CastE (t, _) -> t
  | AddrOf lv -> Cil_const.mk_tptr lv.typ
  | StartOf lv ->
    match Ast_types.unroll lv.typ with
    | { tnode = TArray (t, _); tattr } -> Cil_const.mk_tptr ~tattr t
    | _ ->  assert false
