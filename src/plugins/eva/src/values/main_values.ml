(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module CVal = struct
  include Cvalue.V

  type context = unit
  let context = Abstract_context.Leaf (module Unit_context)

  let zero = Cvalue.V.singleton_zero

  let top = Cvalue.V.top
  let top_int = Cvalue.V.top_int
  let inject_int _typ = Cvalue.V.inject_int

  let equal = Cvalue.V.equal
  let is_included = Cvalue.V.is_included
  let join = Cvalue.V.join
  let narrow a b =
    let n = Cvalue.V.narrow a b in
    if Cvalue.V.is_bottom n
    then `Bottom
    else `Value n

  let assume_non_zero v =
    (Cvalue_forward.assume_non_zero v :> t Abstract_value.truth)

  let assume_bounded kind bound v =
    (Cvalue_forward.assume_bounded kind bound v :> t Abstract_value.truth)

  let assume_not_nan ~assume_finite fkind v =
    (Cvalue_forward.assume_not_nan ~assume_finite fkind v :> t Abstract_value.truth)

  let assume_pointer v =
    (Cvalue_forward.assume_pointer v :> t Abstract_value.truth)

  let assume_aligned align v =
    (Cvalue_forward.assume_aligned align v :> t Abstract_value.truth)

  let assume_comparable cmp v1 v2 =
    (Cvalue_forward.assume_comparable cmp v1 v2 :> (t * t) Abstract_value.truth)

  let constant _context _exp = function
    | Eva_ast.CTopInt _ -> Cvalue.V.top_int
    | CInt64 (i,_k,_s) -> Cvalue.V.inject_int i
    | CChr c           -> Cvalue.V.inject_int (Cil.charConstToInt c)
    | CReal (f, fkind, fstring) ->
      Cvalue_forward.eval_float_constant f fkind fstring
    | CEnum _ -> assert false

  let forward_unop _context typ unop value =
    let value = Cvalue_forward.forward_unop typ unop value in
    (* TODO: `Bottom must be in CValue and Cvalue_forward. *)
    if Cvalue.V.is_bottom value then `Bottom else `Value value

  let forward_binop _context typ binop v1 v2 =
    let value =
      match typ.Cil_types.tnode with
      | TFloat fkind ->
        Cvalue_forward.forward_binop_float (Fval.kind fkind) v1 binop v2
      | TInt _ | TPtr _ | _ ->
        Cvalue_forward.forward_binop_int ~typ v1 binop v2
    in
    if Cvalue.V.is_bottom value
    then `Bottom
    else `Value value

  let rewrap_integer _context = Cvalue_forward.rewrap_integer

  let forward_cast _context ~src_type ~dst_type v =
    let v = Cvalue_forward.forward_cast ~src_type ~dst_type v in
    if Cvalue.V.is_bottom v then `Bottom else `Value v

  let backward_binop _ctx ~input_type ~resulting_type op ~left ~right ~result =
    let reduction =
      Cvalue_backward.backward_binop
        ~typ_res:resulting_type ~res_value:result ~typ_e1:input_type left op right
    in
    match reduction with
    | None -> `Value (None, None)
    | Some (v1, v2) ->
      if Cvalue.V.is_bottom v1 || Cvalue.V.is_bottom v2
      then `Bottom
      else `Value (Some v1, Some v2)

  let backward_unop _context ~typ_arg op ~arg ~res =
    let reduction = Cvalue_backward.backward_unop ~typ_arg op ~arg ~res in
    match reduction with
    | None -> `Value None
    | Some v as r ->
      if Cvalue.V.is_bottom v
      then `Bottom
      else `Value r

  let backward_cast _context ~src_typ ~dst_typ ~src_val ~dst_val =
    let reduction =
      Cvalue_backward.backward_cast ~src_typ ~dst_typ ~src_val ~dst_val
    in
    match reduction with
    | None -> `Value None
    | Some v ->
      if Cvalue.V.is_bottom v
      then `Bottom
      else if Cvalue.V.is_included src_val v
      then `Value None
      else `Value (Some v)

  let resolve_functions v =
    let aux base offs (acc, alarm) =
      match base with
      | Base.Null | Base.CLogic_Var _ | Base.Allocated _ ->
        acc, true
      | Base.Var (v,_) ->
        if Ast_types.is_fun v.vtype then
          let alarm = alarm || Ival.contains_non_zero offs in
          let kf = Globals.Functions.get v in
          let list = if Ival.contains_zero offs then kf :: acc else acc in
          list, alarm
        else acc, true
    in
    try
      let init = [], false in
      let kfs, alarm = Addresses.Bytes.fold_topset_ok aux v init in
      `Value kfs, alarm
    with Abstract_interp.Error_Top -> `Top, true

  let replace_base substitution t = snd (Cvalue.V.replace_base substitution t)

  let key = Structure.Key_Value.create_key "cvalue"
end

let cval = Abstract_value.Leaf (module CVal)


module Interval = struct
  include Datatype.Option (Ival)

  type context = unit
  let context = Abstract_context.Leaf (module Unit_context)

  let pretty_typ _ = pretty

  let top = None

  let is_included a b = match a, b with
    | _, None        -> true
    | None, _        -> false
    | Some a, Some b -> Ival.is_included a b

  let join a b = match a, b with
    | None, _ | _, None -> None
    | Some a, Some b    -> Some (Ival.join a b)

  let narrow a b = match a, b with
    | None, x | x, None -> `Value x
    | Some a, Some b ->
      let res = Ival.narrow a b in
      if Ival.is_bottom res then `Bottom else `Value (Some res)

  let zero = None
  let top_int = None
  let inject_int _typ i = Some (Ival.inject_singleton i)

  let assume_non_zero v = `Unknown v
  let assume_bounded _ _ v = `Unknown v
  let assume_not_nan ~assume_finite:_ _ v = `Unknown v
  let assume_pointer v = `Unknown v
  let assume_aligned _ v = `Unknown v
  let assume_comparable _ v1 v2 = `Unknown (v1, v2)

  let constant _ _ _ = top
  let forward_unop _ _ _ _ = `Value top
  let forward_binop _ _ _ _ _ = `Value top
  let forward_cast _ ~src_type:_ ~dst_type:_ _ = `Value top

  let resolve_functions _ = `Top, true
  let replace_base _substitution t = t

  let rewrap_integer _ range value =
    match value with
    | None -> value
    | Some value ->
      let size = Z.of_int range.Eval_typ.i_bits in
      let signed = range.Eval_typ.i_signed in
      Some (Ival.cast_int_to_int ~signed ~size value)

  let backward_unop _ ~typ_arg:_ _unop ~arg:_ ~res:_ = `Value None
  let backward_binop _ ~input_type:_ ~resulting_type:_ _binop ~left:_ ~right:_ ~result:_ =
    `Value (None, None)
  let backward_cast _ ~src_typ:_ ~dst_typ:_ ~src_val:_ ~dst_val:_ =
    `Value None

  let key = Structure.Key_Value.create_key "interval"
end

let ival = Abstract_value.Leaf (module Interval)

module Sign = Sign_value
let sign = Abstract_value.Leaf (module Sign)
