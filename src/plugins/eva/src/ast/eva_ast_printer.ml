(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast_types

module Precedence =
struct
  let lhost_level = function
    | Var _ -> 0
    | Mem _ -> 20

  let lval_level lv = match lv.node with
    | Mem _, _
    | Var _, (Field _|Index _) -> 20
    | Var _, NoOffset -> 0

  let binop_level = function
    | LAnd -> 83
    | LOr -> 85
    | BOr | BXor | BAnd -> 75
    | Eq | Ne | Gt | Lt | Ge | Le -> 70
    | MinusA | MinusPP | MinusPI | PlusA | PlusPI | Shiftlt | Shiftrt -> 60
    | Div | Mod | Mult-> 40

  let exp_level e = match e.node with
    | BinOp (op, _, _, _) -> binop_level op
    | CastE _ | AddrOf _ | StartOf _ | UnOp _ -> 30
    | Lval lval -> lval_level lval
    | Const _ -> 0
end

let rec pp_lval fmt lval =
  let pp_exp' = pp_exp' ~precedence:(Precedence.lval_level lval) in
  match lval.node with
  | Var vi, o ->
    Format.fprintf fmt "%s%a" vi.vname pp_offset o
  | Mem e, Field(fi, o) ->
    Format.fprintf fmt "%a->%s%a" pp_exp' e fi.fname pp_offset o
  | Mem e, NoOffset ->
    Format.fprintf fmt "*%a" pp_exp' e
  | Mem e, o ->
    Format.fprintf fmt "(*%a)%a" pp_exp' e pp_offset o

and pp_lval' ~precedence fmt lval =
  if Precedence.lval_level lval >= precedence then
    Format.fprintf fmt "(%a)" pp_lval lval
  else
    pp_lval fmt lval

and pp_lhost fmt lhost =
  match lhost with
  | Var vi ->
    Format.fprintf fmt "%s" vi.vname
  | Mem e ->
    let pp_exp' = pp_exp' ~precedence:(Precedence.lhost_level lhost) in
    Format.fprintf fmt "*%a" pp_exp' e

and pp_offset fmt = function
  | NoOffset -> ()
  | Field (fi, o) ->
    Format.fprintf fmt ".%s%a" fi.Cil_types.fname pp_offset o
  | Index (e, o) ->
    Format.fprintf fmt "[%a]%a" pp_exp e pp_offset o

and pp_exp fmt exp =
  let precedence = Precedence.exp_level exp in
  let pp_exp' = pp_exp' ~precedence in
  match exp.node with
  | Const c ->
    pp_constant fmt c
  | Lval lv ->
    pp_lval fmt lv
  | UnOp (op, e1, _) ->
    Format.fprintf fmt "%a %a" pp_unop op pp_exp' e1
  | BinOp (op, e1, e2, _) ->
    Format.fprintf fmt "@[%a %a %a@]" pp_exp' e1 pp_binop op pp_exp' e2
  | CastE (t, e) ->
    Format.fprintf fmt "(%a)%a" Printer.pp_typ t pp_exp' e
  | AddrOf lv ->
    Format.fprintf fmt "& %a" (pp_lval' ~precedence) lv
  | StartOf lv ->
    Format.fprintf fmt "%a" (pp_lval' ~precedence) lv

and pp_exp' ~precedence fmt exp =
  if Precedence.exp_level exp >= precedence then
    Format.fprintf fmt "(%a)" pp_exp exp
  else
    pp_exp fmt exp

and pp_unop fmt u =
  let s = match u with
    | Neg -> "-"
    | BNot -> "~"
    | LNot -> "!"
  in
  Format.fprintf fmt "%s" s

and pp_binop fmt b =
  let s = match b with
    | PlusA | PlusPI -> "+"
    | MinusA | MinusPP | MinusPI -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Shiftlt -> "<<"
    | Shiftrt -> ">>"
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="
    | Eq -> "=="
    | Ne -> "!="
    | BAnd -> "&"
    | BXor -> "^"
    | BOr -> "|"
    | LAnd -> "&&"
    | LOr -> "||"
  in
  Format.fprintf fmt "%s" s

and pp_constant fmt = function
  | CTopInt _ ->
    Unicode.pp_top fmt
  | CInt64 (_, _, Some s) ->
    Format.fprintf fmt "%s" s
  | CInt64 (i, _, None) ->
    Z.pretty fmt i
  | CChr c ->
    Format.fprintf fmt "'%s'" (Escape.escape_char c)
  | CReal (_, _, Some s) ->
    Format.fprintf fmt "%s" s
  | CReal (f, _, None) ->
    Floating_point.pretty fmt f
  | CEnum ({ einame }, _) ->
    Format.fprintf fmt "%s" einame
