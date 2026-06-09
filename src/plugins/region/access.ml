(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

type clause =
  | Body of logic_info
  | Prop of Property.t
  | Call of stmt * kernel_function * Property.t

let compare_clause a b =
  match a, b with
  | Body f , Body g -> Logic_info.compare f g
  | Body _ , _ -> (-1)
  | _ , Body _ -> (+1)
  | Prop f , Prop g -> Property.compare f g
  | Prop _ , _ -> (-1)
  | _ , Prop _ -> (+1)
  | Call(s1,kf1,p1) , Call(s2,kf2,p2) ->
    let c = Stmt.compare s1 s2 in
    if c <> 0 then c else
      let c = Kernel_function.compare kf1 kf2 in
      if c <> 0 then c else
        Property.compare p1 p2

type acs =
  | Exp of Stmt.t * exp
  | Ret of Stmt.t * exp
  | Lval of Stmt.t * lval
  | Init of Stmt.t * lval * exp
  | Term of clause * term_lval

let compare a b =
  match a, b with
  | Init(sa,la,va), Init(sb,lb,vb) ->
    let cmp = Stmt.compare sa sb in
    if cmp <> 0 then cmp else
      let cmp = Lval.compare la lb in
      if cmp <> 0 then cmp else
        Exp.compare va vb
  | Init _ , _ -> (-1)
  | _ , Init _ -> (+1)

  | Lval(sa,la), Lval(sb,lb) ->
    let cmp = Stmt.compare sa sb in
    if cmp <> 0 then cmp else Lval.compare la lb
  | Lval _ , _ -> (-1)
  | _ , Lval _ -> (+1)

  | Exp(sa,ea), Exp(sb,eb) ->
    let cmp = Stmt.compare sa sb in
    if cmp <> 0 then cmp else Exp.compare ea eb
  | Exp _ , _ -> (-1)
  | _ , Exp _ -> (+1)

  | Ret(sa,ea), Ret(sb,eb) ->
    let cmp = Stmt.compare sa sb in
    if cmp <> 0 then cmp else Exp.compare ea eb
  | Ret _ , _ -> (-1)
  | _ , Ret _ -> (+1)

  | Term(ca,ta), Term(cb,tb) ->
    let cmp = compare_clause ca cb in
    if cmp <> 0 then cmp else Term_lval.compare ta tb

module Set = Set.Make(struct type t = acs let compare = compare end)

let pp_label fmt (s : stmt) =
  match s.labels with
  | Label(l,_,_)::_ -> Format.pp_print_string fmt l
  | _ ->
    let line = Stmt.loc s |> Fileloc.line in
    Format.fprintf fmt "L%d" line

let pp_clause fmt = function
  | Body l -> Format.pp_print_string fmt "logic:" ; Logic_info.pretty fmt l
  | Prop p -> Format.pp_print_string fmt @@ Property.Names.get_prop_name_id p
  | Call(st,kf,prop) ->
    Format.fprintf fmt "%a@%a@%s"
      Kernel_function.pretty kf pp_label st
      (Property.Names.get_prop_name_id prop)

let pretty fmt = function
  | Init(s,l,v) ->
    Format.fprintf fmt "(%a=%a)@%a" Lval.pretty l Exp.pretty v pp_label s
  | Lval(s,l) ->
    Format.fprintf fmt "%a@%a" Lval.pretty l pp_label s
  | Exp(s,e) ->
    Format.fprintf fmt "(%a)@%a" Exp.pretty e pp_label s
  | Ret(s,e) ->
    Format.fprintf fmt "(return %a)@%a" Exp.pretty e pp_label s
  | Term(c,l) ->
    Format.fprintf fmt "(%a)@%a" Term_lval.pretty l pp_clause c

let pp_access fmt = function
  | Exp(_,e) -> Printer.pp_exp fmt e
  | Ret(_,e) -> Format.fprintf fmt "return %a" Printer.pp_exp e
  | Lval(_,l) -> Printer.pp_lval fmt l
  | Init(_,l,v) ->
    Format.fprintf fmt "init %a=%a" Printer.pp_lval l Printer.pp_exp v
  | Term(Prop _,t) -> Printer.pp_term_lval fmt t
  | Term(Body fn,t) ->
    Format.fprintf fmt "%a { %a }" Logic_info.pretty fn Printer.pp_term_lval t
  | Term(Call(_,kf,_),t) ->
    Format.fprintf fmt "%a { %a }" Kernel_function.pretty kf Printer.pp_term_lval t

let pp_line fmt stmt =
  let line = Stmt.loc stmt |> Fileloc.line in
  List.iter (Format.fprintf fmt "%a " Printer.pp_label) stmt.labels ;
  Format.fprintf fmt "s%d, line %d" stmt.sid line

let pp_source fmt = function
  | Init(stmt,_,_) | Ret(stmt,_) | Exp(stmt,_) | Lval(stmt,_) ->
    pp_line fmt stmt
  | Term(Prop ip,_) -> Description.pp_local fmt ip
  | Term(Body fn,_) ->
    if fn.l_type = None then
      Format.fprintf fmt "predicate %a" Logic_info.pretty fn
    else
      Format.fprintf fmt "logic %a" Logic_info.pretty fn
  | Term(Call(stmt,_,_),_) ->
    Format.fprintf fmt "call at %a" pp_line stmt

let ctype_of = function
  | Ctype t -> t
  | _ -> Cil_const.voidType

let location = function
  | Body _ -> Fileloc.unknown (* TODO *)
  | Prop ip | Call(_,_,ip) -> Property.location ip

let typeof = function
  | Init(_,lv,_) | Lval(_,lv) -> Cil.typeOfLval lv
  | Exp(_,e) | Ret(_,e) -> Cil.typeOf e
  | Term(_,lv) ->
    Logic_const.plain_or_set ctype_of @@ Cil.typeOfTermLval lv

open Printer_tag

let marker = function
  | Exp(stmt,e) | Ret(stmt,e) -> PExp(None,Kstmt stmt,e)
  | Init (stmt,(Var vi,_),_) -> PVDecl(None,Kstmt stmt,vi)
  | Init (stmt,(Mem e,_),_) -> PExp(None,Kstmt stmt,e)
  | Lval(stmt,_) | Term (Call (stmt, _, _), _) ->
    PStmtStart(Kernel_function.find_englobing_kf stmt, stmt)
  | Term (Body fn, _) ->
    PGlobal(GAnnot(Dfun_or_pred(fn,Fileloc.unknown),Fileloc.unknown))
  | Term (Prop ip, _) -> PIP ip

let rank = function
  | Term (Body _, _) | Term(Prop _, _) -> 0
  | Exp(s,_) | Ret(s,_) | Init(s,_,_) | Lval(s,_) | Term(Call(s,_,_),_)
    -> s.sid
