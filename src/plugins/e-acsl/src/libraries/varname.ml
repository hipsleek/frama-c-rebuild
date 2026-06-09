(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type scope =
  | Global
  | Function
  | Block

module H = Datatype.String.Hashtbl
let tbl = H.create 7
let globals = H.create 7

let sanitize =
  let trimmed_chars = Str.regexp "\\" in
  fun n -> Str.global_replace trimmed_chars "" n

let get ~scope name =
  let s = sanitize name in
  let _, u =
    Extlib.make_unique_name
      (fun s -> H.mem tbl s || H.mem globals s)
      ~sep:"_"
      s
  in
  let add = match scope with
    | Global -> H.add globals
    | Function | Block -> H.add tbl
  in
  add u ();
  u

let clear_locals () = H.clear tbl

let of_binop = function
  | PlusA -> "plus"
  | PlusPI -> "plus"
  | MinusA -> "minus"
  | MinusPI -> "minus"
  | MinusPP -> "minus"
  | Mult -> "mult"
  | Div -> "div"
  | Mod -> "mod"
  | Shiftlt -> "shiftl"
  | Shiftrt -> "shiftr"
  | Lt -> "lt"
  | Gt -> "gt"
  | Le -> "le"
  | Ge -> "ge"
  | Eq -> "eq"
  | Ne -> "ne"
  | BAnd -> "and"
  | BXor -> "xor"
  | BOr -> "or"
  | LAnd -> "and"
  | LOr -> "or"

let of_unop = function
  | Neg -> "neg"
  | BNot -> "not"
  | LNot -> "not"

(* we try to use the constant value as a suffix; but if it is not a viable for
   variable name we do not use any suffix *)
let suffix suf =
  let is_alphanum c =
    c = '_' ||
    'a' <= c && c <= 'z' ||
    'A' <= c && c <= 'Z' ||
    '0' <= c && c <= '9'
  in
  if String.for_all is_alphanum suf
  then suf
  else
    let () = Kernel.warning "invalid suffix: %s" suf in
    ""

let point = Str.regexp_string "\\."
let trailing_point = Str.regexp_string "\\.$"

let rec of_exp exp = match exp.enode with
  | Lval (lhost, offset) -> of_lhost lhost ^ of_offset offset
  | Const (CInt64 (i, _, _)) -> "const_" ^ suffix (Z.to_string i)
  | Const (CReal (float, _, txt)) ->
    let suf = Option.value ~default:(Float.to_string float) txt in
    let suf = Str.global_replace trailing_point suf "" in
    let suf = Str.global_replace point suf "p" in
    "real" ^ suffix suf
  | Const (CEnum {einame}) -> "enum" ^ suffix ("_" ^ einame)
  | Const (CChr c) -> "char" ^ suffix (Z.to_string @@ Cil.charConstToInt c)
  | BinOp (op, x, y, _) -> of_binop op ^ "_" ^ of_exp x ^ "_" ^ of_exp y
  | UnOp (op, x, _) -> of_unop op ^ "_" ^ of_exp x
  | CastE (_, exp) -> of_exp exp
  | e ->
    Options.debug "Varname.of_exp: supply default or extend this function \
                   to handle enodes like: %a" Cil_types.pp_exp_node e;
    "exp"

and of_lhost = function
  | Var {vorig_name} -> vorig_name
  | Mem exp -> of_exp exp

and of_offset = function
  | NoOffset -> ""
  | Field (fieldinfo, offset) -> "_" ^ fieldinfo.forig_name ^ of_offset offset
  | Index (exp, offset) -> "_" ^ of_exp exp ^ of_offset offset
