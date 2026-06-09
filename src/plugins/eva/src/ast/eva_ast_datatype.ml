(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast_types

module Typ = Cil_datatype.Typ
module Varinfo = Cil_datatype.Varinfo


(** Hashing functions  *)

let rec hash_lval lv =
  let (h, o) = lv.node in
  Hashtbl.hash (hash_lhost h, hash_offset o)

and hash_lhost = function
  | Var v -> Hashtbl.hash (1, Varinfo.hash v)
  | Mem e -> Hashtbl.hash (2, hash_exp e)

and hash_offset = function
  | NoOffset -> Hashtbl.hash 1
  | Index (e, o) -> Hashtbl.hash (2, hash_exp e, hash_offset o)
  | Field (f, o) -> Hashtbl.hash (3, f.forder, hash_offset o)

and hash_exp e =
  match e.node with
  | Const c -> Hashtbl.hash (1, hash_constant c)
  | Lval lv -> Hashtbl.hash (2, hash_lval lv)
  | UnOp (op, e, typ) -> Hashtbl.hash (3, op, hash_exp e, Typ.hash typ)
  | BinOp (op, e1, e2, typ) ->
    Hashtbl.hash (4, op, hash_exp e1, hash_exp e2, Typ.hash typ)
  | CastE (typ, e) -> Hashtbl.hash (5, Typ.hash typ, hash_exp e)
  | AddrOf lv -> Hashtbl.hash (6, hash_lval lv)
  | StartOf lv -> Hashtbl.hash (7, hash_lval lv)

and hash_constant c =
  match c with
  | CTopInt ikind -> Hashtbl.hash (1, ikind)
  | CChr c -> Hashtbl.hash (3, c)
  | CReal (fn, fk, _) -> Hashtbl.hash (4, fn, fk)
  | CInt64 (n, k, _) -> Hashtbl.hash (5, n, k)
  | CEnum (ei, _) -> Hashtbl.hash (6, ei.einame)


(* Exported modules *)

module Lval =
  Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    type t = lval
    let name = "Eva_ast_datatype.Lval"
    let compare = compare_lval
    let equal = equal_lval
    let hash = hash_lval
    let reprs = List.map Eva_ast_builder.translate_lval Cil_datatype.Lval.reprs
    let pretty = Eva_ast_printer.pp_lval
  end)

module Lhost =
  Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    type t = lhost
    let name = "Eva_ast_datatype.Lhost"
    let compare = compare_lhost
    let equal = equal_lhost
    let hash = hash_lhost
    let reprs = List.map (fun v -> Var v) Cil_datatype.Varinfo.reprs
    let pretty fmt h =
      let lv = Eva_ast_builder.mk_lval (h, NoOffset) in
      Eva_ast_printer.pp_lval fmt lv
  end)

module Offset = Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    type t = offset
    let name = "Eva_ast_datatype.Offset"
    let compare = compare_offset
    let equal = equal_offset
    let hash = hash_offset
    let reprs = [NoOffset]
    let pretty = Eva_ast_printer.pp_offset
  end)

module Exp = Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    type t = exp
    let name = "Eva_ast_datatype.Exp"
    let compare = compare_exp
    let equal = equal_exp
    let hash = hash_exp
    let reprs = List.map Eva_ast_builder.translate_exp Cil_datatype.Exp.reprs
    let pretty = Eva_ast_printer.pp_exp
  end)

module Constant = Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    type t = constant
    let name = "Eva_ast_datatype.Constant"
    let compare = compare_constant
    let equal = equal_constant
    let hash = hash_constant
    let reprs = [ CInt64 (Z.zero, IInt, None) ]
    let pretty = Eva_ast_printer.pp_constant
  end)
