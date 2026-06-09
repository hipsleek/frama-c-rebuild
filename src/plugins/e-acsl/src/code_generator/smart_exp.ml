(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

let new_exp ~loc exp_node =
  let exp = Cil.new_exp ~loc exp_node in
  let visitor = object
    inherit Visitor.frama_c_inplace
    method !vtype _ = Cil.DoChildrenPost Misc.unghost_type
  end in
  Visitor.visitFramacExpr visitor exp

let lval ~loc lv =
  new_exp ~loc (Lval lv)

let deref ~loc lv = lval ~loc (Mem lv, NoOffset)

let subscript ~loc array idx =
  match Misc.extract_uncoerced_lval array with
  | Some { enode = Lval lv } ->
    let subscript_lval = Cil.addOffsetLval (Index(idx, NoOffset)) lv in
    lval ~loc subscript_lval
  | Some _ | None ->
    Options.fatal
      ~current:true
      "Trying to create a subscript on an array that is not an Lval: %a"
      Cil_types.pp_exp
      array

let ptr_sizeof ~loc typ =
  match Ast_types.unroll_node typ with
  | TPtr t' -> new_exp ~loc (SizeOf t')
  | _ -> assert false

let lnot ~loc e =
  let ty = Cil.typeOf e in
  if not (Ast_types.is_scalar ty) then
    Options.fatal
      ~current:true
      "Trying to create a logical not on an expression that is not scalar: %a"
      Printer.pp_exp e;
  match Cil.isInteger e with
  | None -> begin
      (* The expression is not an integer constant. Simplify the case where a
         logical not is already present, but otherwise return a new expression
         with the [LNot] operator. *)
      match e.enode with
      | UnOp (LNot, e, _) -> e
      | _ -> new_exp ~loc (UnOp (LNot, e, Cil_const.intType))
    end
  | Some i when Z.is_zero i ->
    (* The expression is an integer equal to zero, directly return one. *)
    Cil.one ~loc
  | _ ->
    (* The expression is an integer that is not equal to zero, directly return
       zero. *)
    Cil.zero ~loc

let null ~loc =
  Cil.mkCast ~newt:Cil_const.voidPtrType (Cil.zero ~loc)

let mem ~loc vi =
  lval
    ~loc
    (Cil.mkMem ~addr:(Cil.evar ~loc vi) ~off:(Index (Cil.zero ~loc, NoOffset)))
