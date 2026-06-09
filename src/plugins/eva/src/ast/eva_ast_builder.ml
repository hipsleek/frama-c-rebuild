(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast_types

[@@@alert "-eva_ast_builder"]

(* --- Constructors --- *)

let mk_exp ?(origin=Built) node =
  let typ = Eva_ast_typing.type_of_exp_node node in
  Eva_ast_types.mk_tag ~node ~typ ~origin

let mk_lval ?(origin=Built) node =
  let typ = Eva_ast_typing.type_of_lval_node node in
  Eva_ast_types.mk_tag ~node ~typ ~origin


(* --- Translation from Cil --- *)

let translate_unop : Cil_types.unop -> Eva_ast_types.unop = function
  | Neg -> Neg
  | BNot -> BNot
  | LNot -> LNot

let translate_binop : Cil_types.binop -> Eva_ast_types.binop = function
  | PlusA -> PlusA
  | PlusPI -> PlusPI
  | MinusA -> MinusA
  | MinusPI -> MinusPI
  | MinusPP -> MinusPP
  | Mult -> Mult
  | Div -> Div
  | Mod -> Mod
  | Shiftlt -> Shiftlt
  | Shiftrt -> Shiftrt
  | Lt -> Lt
  | Gt -> Gt
  | Le -> Le
  | Ge -> Ge
  | Eq -> Eq
  | Ne -> Ne
  | BAnd -> BAnd
  | BXor -> BXor
  | BOr -> BOr
  | LAnd -> LAnd
  | LOr -> LOr


let rec translate_exp (e : Cil_types.exp) =
  let node = match e.enode with
    | Const cst -> Const (translate_constant cst)
    | Lval lval -> Lval (translate_lval lval)
    | UnOp (unop, expr, typ) ->
      UnOp (translate_unop unop, translate_exp expr, typ)
    | BinOp (binop, e1, e2, typ) ->
      BinOp (translate_binop binop, translate_exp e1, translate_exp e2, typ)
    | CastE (typ, expr) -> CastE (typ, translate_exp expr)
    | AddrOf lval -> AddrOf (translate_lval lval)
    | StartOf lval -> StartOf (translate_lval lval)
    | SizeOf _ | SizeOfE _ | AlignOf _ | AlignOfE _ ->
      match (Cil.constFold true e).enode with
      | Const c -> Const (translate_constant c)
      | _ -> Const (CTopInt (Machine.sizeof_kind ()))
  in
  mk_exp ~origin:(Exp e) node

and translate_host : Cil_types.lhost -> Eva_ast_types.lhost = function
  | Var vi -> Var vi
  | Mem e -> Mem (translate_exp e)

and translate_offset : Cil_types.offset -> Eva_ast_types.offset = function
  | NoOffset -> NoOffset
  | Index (expr, offset) ->
    Index (translate_exp expr, translate_offset offset)
  | Field (fieldinfo, offset) ->
    Field (fieldinfo, translate_offset offset)

and translate_lval (host, offset as lval) =
  let node = translate_host host, translate_offset offset in
  mk_lval ~origin:(Lval lval) node

and translate_constant : Cil_types.constant -> Eva_ast_types.constant = function
  | CInt64 (cst, ikind, str) -> CInt64 (cst, ikind, str)
  | CChr chr -> CChr chr
  | CReal (float, fkind, str) -> CReal (float, fkind, str)
  | CEnum ei -> CEnum (ei, translate_exp ei.eival)

let rec translate_init : Cil_types.init -> Eva_ast_types.init = function
  | SingleInit e -> SingleInit (translate_exp e, e.eloc)
  | CompoundInit (t, l) ->
    let translate_field_init (o, i) =
      translate_offset o, translate_init i
    in
    CompoundInit (t, List.map translate_field_init l)

let translate_init_or_str : Cil_types.init_or_str -> Eva_ast_types.init_or_str =
  function
  | CInit i -> CInit (translate_init i)
  | StrInit s -> StrInit s

(* --- Relations --- *)

let invert_relation : binop -> binop = function
  | Gt -> Le
  | Lt -> Ge
  | Le -> Gt
  | Ge -> Lt
  | Eq -> Ne
  | Ne -> Eq
  | _ -> invalid_arg "invert_relation: must be given a comparison operator"

let conv_relation : binop -> Abstract_interp.Comp.t = function
  | Eq -> Eq
  | Ne -> Ne
  | Le -> Le
  | Lt -> Lt
  | Ge -> Ge
  | Gt -> Gt
  | _ -> invalid_arg "conv_relation: must be given a comparison operator"


(* --- Offsets --- *)

let rec concat_offset (o1 : offset) (o2 : offset) : offset =
  match o1 with
  | NoOffset -> o2
  | Field (fid, o1') -> Field (fid, concat_offset o1' o2)
  | Index (e, o1') -> Index (e, concat_offset o1' o2)

let add_offset (lval : lval) (offset : offset) : lval =
  let (lval_host, lval_offset) = lval.node in
  mk_lval (lval_host, concat_offset lval_offset offset)


(* --- Smart constructors --- *)

module Build =
struct
  let integer ~ikind i =
    let i', _truncated = Cil.truncateInteger64 ikind i in
    mk_exp (Const (CInt64 (i', ikind, None)))

  let int ~ikind i = integer ~ikind (Z.of_int i)

  let zero = int ~ikind:IInt 0
  let one = int ~ikind:IInt 1
  let bool = function false -> zero | true -> one

  let float ~fkind f =
    let f = Floating_point.round_if_single_precision fkind f in
    mk_exp (Const (CReal (f, fkind, None)))

  let cast typ exp =
    if Cil.need_cast exp.typ typ
    then mk_exp (CastE (Ast_types.remove_qualifiers typ, exp))
    else exp

  let binop op e1 e2 =
    (* TODO: const folding *)
    match op with
    | PlusA | MinusA | Mult | Div ->
      let t = Cil.arithmeticConversion e1.typ e2.typ in
      mk_exp (BinOp (op,e1,e2,t))

    | Eq | Ne | Lt | Le | Ge |Gt ->
      let t =
        if Ast_types.(is_arithmetic e1.typ && is_arithmetic e2.typ) then
          Cil.arithmeticConversion e1.typ e2.typ
        else if Ast_types.(is_ptr e1.typ && is_ptr e2.typ) then
          if Cil.need_cast ~force:true e1.typ e2.typ then
            Machine.uintptr_type ()
          else
            e1.typ
        else
          invalid_arg "unsupported construction"
      in
      mk_exp (BinOp (op, cast t e1, cast t e2, Cil_const.intType))

    | _ -> invalid_arg "unsupported construction"

  let add = binop PlusA
  let div = binop Div
  let eq = binop Eq
  let ne = binop Ne

  let index (base : lval) (index : exp) : lval =
    assert (Ast_types.is_array base.typ);
    add_offset base (Index (index, NoOffset))

  let field (base : lval) (field : Cil_types.fieldinfo) : lval =
    let field_belongs_to_typ fi typ =
      match typ.Cil_types.tnode with
      | TComp ci -> ci == fi.Cil_types.fcomp
      | _ -> false
    in
    assert (field_belongs_to_typ field base.typ);
    add_offset base (Field (field, NoOffset))

  let mem (exp : exp) : lval =
    match exp.node with
    | AddrOf lv -> lv
    | StartOf lv -> index lv zero (* Must be an array *)
    | _ -> mk_lval (Mem exp, NoOffset)

  let var vi = mk_lval (Var vi, NoOffset)
  let var_exp vi = mk_exp (Lval (var vi))
  let var_addr vi = mk_exp (AddrOf (var vi))

  let lval lv =
    Eva_ast_types.mk_tag ~node:(Lval lv) ~typ:lv.typ ~origin:lv.origin
end


(* --- Condition normalization --- *)

let zero_typed (typ : Cil_types.typ) =
  match typ.tnode with
  | TFloat fk -> mk_exp (Const (CReal (0., fk, None)))
  | TEnum {ekind = ik }
  | TInt ik -> mk_exp (Const (CInt64 (Z.zero, ik, None)))
  | TPtr _ ->
    let ik = Machine.uintptr_kind () in
    let zero = mk_exp (Const (CInt64 (Z.zero, ik, None))) in
    Build.cast typ zero
  | _ ->
    Self.fatal ~current:true "non-scalar type %a" Printer.pp_typ typ

(* Transform an expression supposed to be [positive] into an equivalent
   one in which the root expression is a comparison operator. *)
let rec normalize_condition exp positive =
  match exp.node with
  | UnOp (LNot, e, _) -> normalize_condition e (not positive)
  | BinOp ((Le|Ne|Eq|Gt|Lt|Ge as binop), e1, e2, typ) ->
    if positive
    then exp
    else mk_exp (BinOp (invert_relation binop, e1, e2, typ))
  | _ ->
    let op = if positive then Ne else Eq in
    let typ = Ast_types.unroll exp.typ in
    mk_exp (BinOp (op, zero_typed typ, exp, Cil_const.intType))


(* --- Hide mk optional parameters --- *)

let mk_exp = mk_exp ~origin:Built
let mk_lval = mk_lval ~origin:Built
