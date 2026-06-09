(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

exception No_conversion

let error_lval () = raise No_conversion

let rec logic_type_to_typ = function
  | Ctype typ -> typ
  | Linteger -> Cil_const.longLongType (*TODO: to have an unlimited integer type
                                         in the logic interpretation*)
  | Lreal -> Cil_const.doubleType (* TODO: handle reals, not floats... *)
  | Lboolean  -> Cil_const.longLongType
  | Ltype({lt_name = "set"},[t]) -> logic_type_to_typ t
  | Ltype _ | Lvar _ | Larrow _ -> error_lval ()


(* Expect conversion to be possible on all sub-terms, otherwise raise an error. *)

let logic_var_to_var { lv_origin = lv } =
  match lv with
  | None -> error_lval ()
  | Some lv -> lv

let create_const_list loc kind low high =
  let rec aux acc i =
    if Z.lt i low then acc
    else
      aux (Cil.new_exp ~loc (Const (CInt64 (i,kind,None)))::acc) (Z.pred i)
  in aux [] high

let range low high =
  let loc = fst low.eloc, snd high.eloc in
  match (Cil.constFold true low).enode, (Cil.constFold true high).enode with
    Const(CInt64(low,kind,_)), Const(CInt64(high,_,_)) ->
    create_const_list loc kind low high
  | _ -> error_lval()

let singleton f loc =
  match f loc with
    [ l ] -> l
  | _ -> error_lval()

let rec loc_lval_to_lval ?result (lh, lo) =
  List.product_map
    (fun x y -> (x,y))
    (loc_lhost_to_lhost ?result lh)
    (loc_offset_to_offset ?result lo)

and loc_lhost_to_lhost ?result = function
  | TVar lvar -> [Var (logic_var_to_var lvar)]
  | TMem lterm -> List.map (fun x -> Mem x) (loc_to_exp ?result lterm)
  | TResult _ ->
    ( match result with
        None -> error_lval()
      | Some v -> [Var v])

and loc_offset_to_offset ?result = function
  | TNoOffset -> [NoOffset]
  | TModel _ -> error_lval ()
  | TField (fi, lo) ->
    List.map (fun x -> Field (fi,x)) (loc_offset_to_offset ?result lo)
  | TIndex (lexp, lo) ->
    List.product_map
      (fun x y -> Index(x,y))
      (loc_to_exp ?result lexp)
      (loc_offset_to_offset ?result lo)

and loc_to_exp ?result {term_node = lnode ; term_type = ltype; term_loc = loc} =
  match lnode with
  | TLval lv ->
    List.map (fun x -> Cil.new_exp ~loc (Lval x)) (loc_lval_to_lval ?result lv)
  | TAddrOf lv ->
    List.map (fun x -> Cil.new_exp ~loc (AddrOf x)) (loc_lval_to_lval ?result lv)
  | TStartOf lv ->
    List.map (fun x -> Cil.new_exp ~loc (StartOf x)) (loc_lval_to_lval ?result lv)
  | TSizeOfE lexp ->
    List.map (fun x -> Cil.new_exp ~loc (SizeOfE x)) (loc_to_exp ?result lexp)
  | TAlignOfE lexp ->
    List.map (fun x -> Cil.new_exp ~loc (AlignOfE(x, `Standard))) (loc_to_exp ?result lexp)
  | TUnOp (unop, lexp) ->
    List.map
      (fun x -> Cil.new_exp ~loc (UnOp (unop, x, logic_type_to_typ ltype)))
      (loc_to_exp ?result lexp)
  | TBinOp (binop, lexp1, lexp2) ->
    List.product_map
      (fun x y -> Cil.new_exp ~loc (BinOp (binop, x,y, logic_type_to_typ ltype)))
      (loc_to_exp ?result lexp1)
      (loc_to_exp ?result lexp2)
  | TConst constant ->
    (* TODO: Very likely to fail on large integer and incorrect on reals not
       representable as floats *)
    [Cil.new_exp ~loc (Const (Logic_utils.lconstant_to_constant constant))]
  | TCast (false, Ctype typ, lexp) ->
    List.map
      (fun x -> Cil.new_exp ~loc (CastE (typ, x))) (loc_to_exp ?result lexp)
  | TAlignOf typ -> [Cil.new_exp ~loc (AlignOf (typ, `Standard))]
  | TSizeOf typ -> [Cil.new_exp ~loc (SizeOf typ)]
  | Trange (Some low, Some high) ->
    let low = singleton (loc_to_exp ?result) low in
    let high = singleton (loc_to_exp ?result) high in
    range low high
  | Tunion l -> List.concat (List.map (loc_to_exp ?result) l)
  | Tempty_set -> []
  | Tinter _ | Tcomprehension _ -> error_lval()
  | Tat ({term_node = TAddrOf (TVar _, TNoOffset)} as taddroflval, _) ->
    loc_to_exp ?result taddroflval
  | TCast (true, Linteger, t) when Logic_utils.is_integral_type t.term_type ->
    loc_to_exp ?result t
  | TCast (true, Lreal, t) when Logic_utils.is_integral_type t.term_type ->
    List.map
      (fun x -> Cil.new_exp ~loc (CastE (logic_type_to_typ Lreal, x)))
      (loc_to_exp ?result t)
  | TCast (true, Lreal, t) when Logic_utils.is_arithmetic_type t.term_type ->
    loc_to_exp ?result t
  | TCast (true, set, t)
    when
      Logic_const.is_set_type set &&
      Logic_utils.is_same_type
        (Logic_utils.type_of_set_elem set) t.term_type ->
    loc_to_exp ?result t
  | Tnull -> [ Cil.mkCast ~newt:(Cil_const.voidPtrType) (Cil.zero ~loc) ]

  (* additional constructs *)
  | Tapp _ | Tlambda _ | Trange _   | Tlet _
  | TDataCons _
  | Tif _
  | Tat _
  | Tbase_addr _
  | Toffset _
  | Tblock_length _
  | TUpdate _ | Ttypeof _ | Ttype _
  | TCast _
    -> error_lval ()

let rec loc_to_lval ?result t =
  match t.term_node with
  | TLval lv -> loc_lval_to_lval ?result lv
  | TAddrOf lv -> loc_lval_to_lval ?result lv
  | TStartOf lv -> loc_lval_to_lval ?result lv
  | Tunion l1 -> List.concat (List.map (loc_to_lval ?result) l1)
  | Tempty_set -> []
  (* coercions to arithmetic types cannot be lval. We only have to consider
     a coercion to set here.
  *)
  | TCast (true, set, t) when
      Logic_utils.is_set_type set &&
      Logic_utils.is_same_type
        (Logic_utils.type_of_set_elem set) t.term_type ->
    loc_to_lval ?result t
  | Tinter _ -> error_lval() (* TODO *)
  | Tcomprehension _ -> error_lval()
  | TSizeOfE _ | TAlignOfE _ | TUnOp _ | TBinOp _
  | TConst _ | TCast _ | TAlignOf _ | TSizeOf _ | Tapp _ | Tif _
  | Tat _ | Toffset _ | Tbase_addr _ | Tblock_length _ | Tnull | Trange _
  | TDataCons _ | TUpdate _ | Tlambda _
  | Ttypeof _ | Ttype _ | Tlet _ ->
    error_lval ()

let loc_to_offset ?result loc =
  let rec aux h =
    function
      TLval(h',o) | TStartOf (h',o) ->
      (match h with None -> Some h', loc_offset_to_offset ?result o
                  | Some h when Logic_utils.is_same_lhost h h' ->
                    Some h, loc_offset_to_offset ?result o
                  | Some _ -> error_lval()
      )
    | Tat ({ term_node = TLval(TResult _,_)} as lv, BuiltinLabel Post) ->
      aux h lv.term_node
    | Tunion locs ->
      List.fold_left
        (fun (b,l) x -> let (b,l') = aux b x.term_node in b, l @ l')
        (h,[]) locs
    | Tempty_set -> h,[]
    | Trange _ | TAddrOf _ | Tat _
    | TSizeOfE _ | TAlignOfE _ | TUnOp _ | TBinOp _
    | TConst _ | TCast _ | TAlignOf _ | TSizeOf _ | Tapp _ | Tif _
    | Toffset _ | Tbase_addr _ | Tblock_length _ | Tnull
    | TDataCons _ | TUpdate _ | Tlambda _
    | Ttypeof _ | Ttype _ | Tcomprehension _ | Tinter _ | Tlet _
      -> error_lval ()
  in snd (aux None loc.term_node)

let term_lval_to_lval ?result = singleton (loc_lval_to_lval ?result)

let term_to_lval ?result = singleton (loc_to_lval ?result)

let term_to_exp ?result = singleton (loc_to_exp ?result)

let term_offset_to_offset ?result = singleton (loc_offset_to_offset ?result)
