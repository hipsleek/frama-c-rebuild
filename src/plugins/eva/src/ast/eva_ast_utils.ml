(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast_types
open Eva_ast_builder


(* --- Conversion to Cil --- *)

(* Memoization to avoid creating too many expressions. *)
module ConversionToCil =
  State_builder.Hashtbl
    (Eva_ast_datatype.Exp.Hashtbl)
    (Cil_datatype.Exp)
    (struct
      let name = "Eva.Eva_ast_utils.ConversionToCil"
      let size = 16
      let dependencies = [ Ast.self ]
    end)

let rec to_cil_exp exp =
  match exp.origin with
  | Exp e -> e
  | _ -> ConversionToCil.memo build_cil_exp exp

and build_cil_exp exp =
  let exp_node : Cil_types.exp_node =
    match exp.node with
    | Const c -> Const (to_cil_const c)
    | Lval lv -> Lval (to_cil_lval lv)
    | UnOp (op, e, t) -> UnOp (to_cil_unop op, to_cil_exp e, t)
    | BinOp (op, e1, e2, t) ->
      BinOp (to_cil_binop op, to_cil_exp e1, to_cil_exp e2, t)
    | CastE (t, e) -> CastE (t, to_cil_exp e)
    | AddrOf lv -> AddrOf (to_cil_lval lv)
    | StartOf lv -> StartOf (to_cil_lval lv)
  in
  Cil.new_exp ~loc:Fileloc.unknown exp_node

and to_cil_unop : Eva_ast_types.unop -> Cil_types.unop = function
  | Neg -> Neg
  | BNot -> BNot
  | LNot -> LNot

and to_cil_binop : Eva_ast_types.binop -> Cil_types.binop = function
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

and to_cil_lval lval =
  match lval.origin with
  | Lval lv -> lv
  | _ ->
    let (lhost, offset) = lval.node in
    to_cil_lhost lhost, to_cil_offset offset

and to_cil_lhost : Eva_ast_types.lhost -> Cil_types.lhost = function
  | Var vi -> Var vi
  | Mem e -> Mem (to_cil_exp e)

and to_cil_offset : Eva_ast_types.offset -> Cil_types.offset = function
  | NoOffset -> NoOffset
  | Field (fi, off) -> Field (fi, to_cil_offset off)
  | Index (e, off) -> Index (to_cil_exp e, to_cil_offset off)

and to_cil_const : Eva_ast_types.constant -> Cil_types.constant = function
  | CInt64 (i, ik, s) -> CInt64 (i, ik, s)
  | CChr c -> CChr c
  | CReal (f, fk, s) -> CReal (f, fk, s)
  | CEnum (ei, _) -> CEnum ei
  | CTopInt _ as constant ->
    Self.fatal "The Eva constant %a cannot be converted to cil"
      Eva_ast_printer.pp_constant constant


(* --- Queries --- *)

let is_mutable (lval : lval) : bool =
  let (lhost, offset) = lval.node in
  let rec aux base_mutable typ off =
    let base_mutable = base_mutable && not (Ast_types.is_const typ) in
    let typ = Ast_types.unroll typ in
    match typ.tnode, off with
    | _, NoOffset -> base_mutable
    | _, Field (fi, off) ->
      let base_mutable =
        base_mutable || Ast_attributes.(contains frama_c_mutable fi.fattr)
      in
      aux base_mutable fi.ftype off
    | TArray(typ, _), Index(_, off) -> aux base_mutable typ off
    | _, Index _ ->
      Self.fatal "Index on non-array type %a" Printer.pp_typ typ
  in
  aux false (Eva_ast_typing.type_of_lhost lhost) offset

let rec is_initialized_exp (on_same_obj : bool) (exp : exp) =
  match exp.node with
  | Lval lv | AddrOf lv | StartOf lv ->
    let (lh, _) = lv.node in
    is_initialized_lhost on_same_obj lh
  | BinOp ((PlusPI|MinusPI), e, _, _) | CastE (_, e) ->
    is_initialized_exp on_same_obj e
  | _ -> false

and is_initialized_lhost (on_same_obj : bool) (lhost : lhost) =
  match lhost with
  | Var vi -> Ast_attributes.(contains frama_c_init_obj vi.vattr)
  | Mem e -> on_same_obj && is_initialized_exp false e

let is_initialized lval =
  let (lhost, _) = lval.node in
  is_initialized_lhost true lhost


(* --- Heights --- *)

let rec height_exp exp =
  match exp.node with
  | Const _ -> 0
  | Lval lv | AddrOf lv | StartOf lv  -> height_lval lv + 1
  | UnOp (_,e,_) | CastE (_, e) -> height_exp e + 1
  | BinOp (_,e1,e2,_) -> max (height_exp e1) (height_exp e2) + 1

and height_lval lv =
  let host, offset = lv.node in
  let h1 = match host with
    | Var _ -> 0
    | Mem e -> height_exp e + 1
  in
  max h1 (height_offset offset) + 1

and height_offset = function
  | NoOffset  -> 0
  | Field (_,r) -> height_offset r + 1
  | Index (e,r) -> max (height_exp e) (height_offset r) + 1


(* --- Specialized visitors --- *)

let exp_contains_volatile, lval_contains_volatile =
  let open Eva_ast_visitor.Observe in
  let neutral = false and combine b1 b2 = b1 || b2 in
  let observe_lval ~visitor lval =
    Ast_types.is_volatile lval.typ || default.observe_lval ~visitor lval
  in
  let observer = { default with observe_lval } in
  visit_exp ~neutral ~combine observer, visit_lval ~neutral ~combine observer

let vars_in_exp, vars_in_lval =
  let module VarSet = Cil_datatype.Varinfo.Set in
  let open Eva_ast_visitor.Observe in
  let neutral = VarSet.empty and combine = VarSet.union in
  let observe_lval ~visitor lval =
    let vars = default.observe_lval ~visitor lval in
    match fst lval.node with
    | Var vi -> VarSet.add vi vars
    | Mem _ -> vars
  in
  let observer = { default with observe_lval } in
  visit_exp ~neutral ~combine observer, visit_lval ~neutral ~combine observer


let rec to_integer e =
  match e.node with
  | Const (CInt64 (n,_,_)) -> Some n
  | Const (CChr c) -> Some (Cil.charConstToInt c)
  | Const (CEnum ({eival = v}, _)) -> Cil.isInteger v
  | CastE (typ, e) when Ast_types.is_ptr typ ->
    begin
      match to_integer e with
      | Some i as r when Cil.fitsInInt (Machine.uintptr_kind ()) i -> r
      | _ -> None
    end
  | _ -> None

let is_zero exp =
  match to_integer exp with
  | None -> false
  | Some i -> Z.is_zero i

let is_zero_ptr exp =
  is_zero exp && Ast_types.is_ptr exp.typ

(* Constant folding *)

let apply_binop_to_integers binop i1 i2 ikind =
  (* Can a shift operation be safely computed ? *)
  let shift_in_bounds i2 =
    let size = Z.of_int (Cil.bitsSizeOfInt ikind) in
    Z.geq i2 0z && Z.lt i2 size
  in
  let bool op x y = if op x y then Z.one else Z.zero in
  let integer_op = function
    | PlusA -> Z.add
    | MinusA -> Z.sub
    | Mult -> Z.mul
    | Div -> Z.div
    | Mod -> Z.rem
    | BAnd -> Z.logand
    | BOr -> Z.logor
    | BXor -> Z.logxor
    | Shiftlt when shift_in_bounds i2 -> Z.shift_left_z
    | Shiftrt when shift_in_bounds i2 ->
      if Cil.isSigned ikind
      then Z.shift_right_z
      else Z.shift_right_logical
    | Eq -> bool Z.equal
    | Ne -> bool (fun x y -> not (Z.equal x y))
    | Le -> bool Z.leq
    | Ge -> bool Z.geq
    | Lt -> bool Z.lt
    | Gt -> bool Z.gt
    | _ -> raise Not_found
  in
  try Some (integer_op binop i1 i2)
  with Division_by_zero | Not_found -> None

let to_value exp =
  match exp.node with
  | Const (CInt64 (i, _, _)) -> `Int i
  | Const (CReal (f, _, _)) -> `Float f
  | _ -> `None

let type_kind typ =
  match Ast_types.unroll_node typ with
  | TInt ikind | TEnum {ekind = ikind} -> `Int ikind
  | TFloat fkind -> `Float fkind
  | _ -> `None

(* These functions are largely based on Cil.constFold. See there for details. *)
let rec const_fold (exp: exp) : exp =
  match exp.node with
  | Const (CChr c) -> Build.integer ~ikind:IInt (Cil.charConstToInt c)
  | Const (CEnum (_ei, e)) -> const_fold e
  | Const (CTopInt _ | CReal _ | CInt64 _) -> exp
  | Lval lv -> mk_exp (Lval (const_fold_lval lv))
  | AddrOf lv -> mk_exp (AddrOf (const_fold_lval lv))
  | StartOf lv -> mk_exp (StartOf (const_fold_lval lv))
  | CastE (t, e) -> const_fold_cast t e
  | UnOp (op, e, t) -> const_fold_unop op e t
  | BinOp (op, e1, e2, t) -> const_fold_binop op e1 e2 t

and const_fold_cast (t : typ) (e : exp) : exp  =
  let e = const_fold e in
  let default () = mk_exp (CastE (t, e)) in
  if t.tattr <> [] then
    default ()
  else
    match to_value e, type_kind t with
    (* integer -> integer *)
    | `Int i, `Int ikind -> Build.integer ~ikind i
    (* float -> float *)
    | `Float f, `Float fkind -> Build.float ~fkind f
    (* float -> integer *)
    | `Float f, `Int ikind ->
      begin match Floating_point.truncate_to_integer f with
        | Integer i when Cil.fitsInInt ikind i -> Build.integer ~ikind i
        | Overflow | Underflow | Integer _ -> default ()
      end
    (* int -> float *)
    | `Int i, `Float fkind -> Build.float ~fkind (Z.to_float i)
    | _, _ -> default ()

and const_fold_unop (op : unop) (e : exp) (t : typ) : exp =
  let e = const_fold e in
  let default () = mk_exp (UnOp (op, e, t)) in
  match op, to_value e, type_kind t with
  (* Integer operations *)
  | Neg, `Int i, `Int ikind -> Build.integer ~ikind (Z.neg i)
  | BNot, `Int i, `Int ikind -> Build.integer ~ikind (Z.lognot i)
  | LNot, `Int i, `Int _ ->
    if Z.(equal i zero) then Build.one else Build.zero
  (* Float operations *)
  | Neg, `Float f, `Float fkind ->
    let f = match fkind with
      | FFloat  | FFloat32 -> Floating_point.round_to_single_precision f
      | FDouble | FFloat64 | FLongDouble -> f
    in
    mk_exp (Const (CReal (-.f, fkind, None)))
  (* No possible folding *)
  | _ -> default ()

and const_fold_binop (op : binop) (e1 : exp) (e2 : exp) (t : typ) : exp =
  (* TODO: float comparisons *)
  let e1 = const_fold e1 in
  let e2 = const_fold e2 in
  let default () = mk_exp (BinOp (op, e1, e2, t)) in
  match type_kind t, to_value e1, to_value e2 with
  (* Integer operations on constants *)
  | `Int ikind, `Int i1, `Int i2 ->
    begin
      match apply_binop_to_integers op i1 i2 ikind with
      | Some res -> Build.integer ~ikind res
      | None -> default ()
    end
  (* Special cases for some integer operations *)
  | `Int _, i1, i2 ->
    (* These three functions always return false when the value is not a
       constant integer — so [is_non_zero] is not the opposite of [is_zero]. *)
    let is_zero = function `Int i -> Z.is_zero i | _ -> false in
    let is_one = function `Int i -> Z.is_one i | _ -> false in
    let is_non_zero = function `Int i -> not (Z.is_zero i) | _ -> false in
    begin
      match op with
      | PlusA when is_zero i1 -> e2
      | PlusA | MinusA | PlusPI | MinusPI when is_zero i2 -> e1
      | Mult when is_zero i1 || is_one i2 -> e1
      | Mult when is_zero i2 || is_one i1 -> e2
      | Div when is_one i2 -> e1
      | BAnd when is_zero i1 -> e1
      | BAnd when is_zero i2 -> e2
      | BOr when is_zero i1 -> e2
      | BOr when is_zero i2 -> e1
      | Shiftlt | Shiftrt when is_zero i1 || is_zero i2 -> e1
      | LAnd when is_zero i1 || is_zero i2 -> Build.zero
      | LOr when is_non_zero i1 || is_non_zero i2 -> Build.one
      | LAnd when is_non_zero i1 -> e2
      | LAnd when is_non_zero i2 -> e1
      | LOr when is_zero i1 -> e2
      | LOr when is_zero i2 -> e1
      | _ -> default ()
    end
  (* Floating-point operation *)
  | `Float fkind, `Float f1, `Float f2 ->
    begin
      match op with
      | PlusA  -> Build.float ~fkind (f1 +. f2)
      | MinusA -> Build.float ~fkind (f1 -. f2)
      | Mult   -> Build.float ~fkind (f1 *. f2)
      | Div    -> Build.float ~fkind (f1 /. f2)
      | _ -> default ()
    end
  | _ -> default ()

and const_fold_lval (lval : lval) : lval =
  let lhost, offset = lval.node in
  mk_lval (const_fold_lhost lhost, const_fold_offset offset)

and const_fold_lhost : lhost -> lhost = function
  | Mem e -> Mem (const_fold e)
  | Var _ as host -> host

and const_fold_offset : offset -> offset = function
  | NoOffset -> NoOffset
  | Field (fi, o) -> Field (fi, const_fold_offset o)
  | Index (e, o) -> Index (const_fold e, const_fold_offset o)

let fold_to_integer exp =
  to_integer (const_fold exp)


(* --- Offsets --- *)

let rec last_offset offset : offset =
  match offset with
  | NoOffset | Field(_,NoOffset) | Index(_,NoOffset) -> offset
  | Field(_,off) | Index(_,off) -> last_offset off

let is_bitfield lval =
  let (_, offset) = lval.node in
  match last_offset offset with
  | Field({fbitfield=Some _}, _) -> true
  | _ -> false
