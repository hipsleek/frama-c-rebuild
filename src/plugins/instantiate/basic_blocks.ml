(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Logic_const

let const_of t = Ast_types.add_attributes [("const", [])] t

let size_t () =
  Globals.Types.find_type Logic_typing.Typedef "size_t"

let prepare_definition name fun_type =
  let vi = Cil.makeGlobalVar ~referenced:true name fun_type in
  vi.vdefined <- true ;
  let fd = Cil.emptyFunctionFromVI vi in
  Cil.setFormalsDecl vi fun_type ;
  fd.sformals <- Cil.getFormalsDecl vi ;
  fd

let call_function lval vi args =
  let loc  = Fileloc.unknown in
  let _, typs, _, _ = Cil.splitFunctionTypeVI vi in
  let typs = Cil.argsToList typs in
  let gen_arg exp (_, typ, _) =
    if Cil_datatype.Typ.equal vi.vtype typ then exp
    else Cil.mkCast ~newt:typ exp
  in
  let args = List.map2 gen_arg args typs in
  Call(lval, (Var vi), args, loc)

let rec string_of_typ_aux t =
  match t.tnode with
  | TVoid -> "void"
  | TInt IBool -> "bool"
  | TInt IChar -> "char"
  | TInt ISChar -> "schar"
  | TInt IUChar -> "uchar"
  | TInt IInt -> "int"
  | TInt IUInt -> "uint"
  | TInt IShort -> "short"
  | TInt IUShort -> "ushort"
  | TInt ILong -> "long"
  | TInt IULong -> "ulong"
  | TInt ILongLong -> "llong"
  | TInt IULongLong -> "ullong"
  | TFloat FFloat -> "float"
  | TFloat FDouble -> "double"
  | TFloat FLongDouble -> "ldouble"
  | TPtr t -> "ptr_" ^ string_of_typ t
  | TEnum ei -> "e_" ^ ei.ename
  | TComp ci when ci.cstruct -> "st_" ^ ci.cname
  | TComp ci -> "un_" ^ ci.cname
  | TArray (t, Some e) ->
    "arr" ^ (string_of_exp e) ^ "_" ^ string_of_typ t
  | _ ->
    Options.fatal "unsupported type %a" Cil_printer.pp_typ t
and string_of_typ t = string_of_typ_aux (Ast_types.unroll t)
and string_of_exp e = Format.asprintf "%a" Cil_printer.pp_exp e

let size_var ?(name_ext="") t value = {
  l_var_info = Cil_const.make_logic_var_local ("__fc_" ^ name_ext ^ "len") t;
  l_type = Some t;
  l_tparams = [];
  l_labels = [];
  l_profile = [];
  l_body = LBterm value;
}

(** Features related to terms *)

let cvar_to_tvar vi = tvar (Cil.cvar_to_lvar vi)

let tminus ?loc t1 t2 =
  let minus, typ = match t1.term_type, t2.term_type with
    | Ctype(t1), Ctype(t2)
      when Ast_types.is_ptr t1 && Ast_types.is_ptr t2 ->
      MinusPP, Linteger
    | Ctype(t), _ when Ast_types.is_ptr t ->
      MinusPI, Ctype(t)
    | t, _ ->
      MinusA, t
  in
  term ?loc (TBinOp(minus, t1, t2)) typ

let tplus ?loc t1 t2 =
  let plus = match t1.term_type with
    | Ctype(t) when Ast_types.is_ptr t -> PlusPI
    | _ -> PlusA
  in
  term ?loc (TBinOp(plus, t1, t2)) t1.term_type

let tdivide ?loc t1 t2 =
  term ?loc (TBinOp(Div, t1, t2)) t1.term_type

let ttype_of_pointed t =
  match Ast_types.unroll_logic t with
  | Ctype { tnode = TPtr t | TArray (t, _) } -> Ctype t
  | _ -> Options.fatal "ttype_of_pointed on a non pointer type"

let tbuffer_range ?loc ptr len =
  let last = tminus ?loc len (tinteger ?loc 1) in
  let range = trange ?loc (Some (tinteger ?loc 0), Some last) in
  tplus ?loc ptr range

let rec tunref_range ?loc ptr len =
  let typ = ttype_of_pointed ptr.term_type in
  let range = tbuffer_range ?loc ptr len in
  let tlval = (TMem range), TNoOffset in
  let tlval, typ = tunref_range_unfold ?loc tlval typ in
  term (TLval tlval) typ
and tunref_range_unfold ?loc lval typ =
  match typ with
  | Ctype { tnode = TArray (typ, Some e) } ->
    let len = Logic_utils.expr_to_term ~coerce:true e in
    let last = tminus ?loc len (tinteger ?loc 1) in
    let range = trange ?loc (Some (tinteger ?loc 0), Some last) in
    let lval = addTermOffsetLval (TIndex(range, TNoOffset)) lval in
    tunref_range_unfold lval (Ctype typ)
  | _ -> lval, typ

let taccess ?loc ptr offset =
  let get_lval = function
    | TLval(lval) -> lval
    | _ -> Options.fatal "unexpected non-lvalue on call to taccess"
  in
  match Ast_types.unroll_logic ptr.term_type with
  | Ctype { tnode = TPtr _ } ->
    let address = tplus ?loc ptr offset in
    let lval = TLval(TMem(address), TNoOffset) in
    term ?loc lval (ttype_of_pointed ptr.term_type)
  | Ctype { tnode = TArray _ } ->
    let lval = get_lval ptr.term_node in
    let lval = addTermOffsetLval (TIndex(offset, TNoOffset)) lval in
    term ?loc (TLval lval) (ttype_of_pointed ptr.term_type)
  | _ -> Options.fatal "taccess on a non pointer type"

let sizeofpointed = function
  | Ctype { tnode = (TPtr t | TArray (t, _)) } -> Cil.bytesSizeOf t
  | _ -> Options.fatal "size_of_pointed on a non pointer type"

let sizeof = function
  | Ctype t -> Cil.bytesSizeOf t
  | _ -> Options.fatal "sizeof on a non C type"

let tunref_range_bytes_len ?loc ptr bytes_len =
  let sizeof = sizeofpointed ptr.term_type in
  if sizeof = 1 then
    tunref_range ?loc ptr bytes_len
  else
    let sizeof = tinteger ?loc sizeof in
    let len = tdivide ?loc bytes_len sizeof in
    tunref_range ?loc ptr len


(** Features related to predicates *)

let plet_len_div_size ?loc ?name_ext t bytes_len pred =
  let sizeof = sizeofpointed t in
  if sizeof = 1 then
    pred bytes_len
  else
    let len = tdivide ?loc bytes_len (tinteger ?loc sizeof) in
    let len_var = size_var ?name_ext Linteger len in
    plet ?loc len_var (pred (tvar len_var.l_var_info))

let pgeneric_valid_buffer ?loc validity lbl ptr len =
  let buffer = tbuffer_range ?loc ptr len in
  (* higher-order funs do not mix well with optional labels, hence the binding
     below *)
  let names = None in
  validity ?loc ?names (lbl, buffer)

let pgeneric_valid_len_bytes ?loc validity lbl ptr bytes_len =
  plet_len_div_size ?loc ptr.term_type bytes_len
    (pgeneric_valid_buffer ?loc validity lbl ptr)

let pobject_pointer ?loc label ptr =
  Logic_const.pobject_pointer ?loc (label, ptr)

let pvalid_len_bytes ?loc = pgeneric_valid_len_bytes ?loc pvalid
let pvalid_read_len_bytes ?loc = pgeneric_valid_len_bytes ?loc pvalid_read

let pcorrect_len_bytes ?loc t bytes_len =
  let sizeof = tinteger ?loc (sizeofpointed t) in
  let modulo = term ?loc (TBinOp(Mod, bytes_len, sizeof)) Linteger in
  prel ?loc (Req, modulo, tinteger ?loc 0)

let pbounds_incl_excl ?loc low value up =
  let geq_0 = prel ?loc (Rle, low, value) in
  let lt_len = prel ?loc (Rlt, value, up) in
  pand ?loc (geq_0, lt_len)

let rec punfold_all_elems_eq ?loc t1 t2 len =
  assert(Cil_datatype.Logic_type.equal t1.term_type t2.term_type) ;
  pall_elems_eq ?loc 0 t1 t2 len
and pall_elems_eq ?loc depth t1 t2 len =
  let ind = Cil_const.make_logic_var_quant ("j" ^ (string_of_int depth)) Linteger in
  let tind = tvar ind in
  let bounds = pbounds_incl_excl ?loc (tinteger 0) tind len in
  let t1_acc = taccess ?loc t1 tind in
  let t2_acc = taccess ?loc t2 tind in
  let eq = peq_unfold ?loc (depth+1) t1_acc t2_acc in
  pforall ?loc ([ind], (pimplies ?loc (bounds, eq)))
and peq_unfold ?loc depth t1 t2 =
  match Ast_types.unroll_logic t1.term_type with
  | Ctype { tnode = TArray (_, Some len) } ->
    let len = Logic_utils.expr_to_term ~coerce:true len in
    pall_elems_eq ?loc depth t1 t2 len
  | _ -> prel ?loc (Req, t1, t2)

let rec punfold_all_elems_pred ?loc t1 len pred =
  pall_elems_pred ?loc 0 t1 len pred
and pall_elems_pred ?loc depth t1 len pred =
  let ind = Cil_const.make_logic_var_quant ("j" ^ (string_of_int depth)) Linteger in
  let tind = tvar ind in
  let bounds = pbounds_incl_excl ?loc (tinteger 0) tind len in
  let t1_acc = taccess ?loc t1 tind in
  let eq = punfold_pred ?loc depth t1_acc pred in
  pforall ?loc ([ind], (pimplies ?loc (bounds, eq)))
and punfold_pred ?loc ?(dyn_len = None) depth t1 pred =
  match Ast_types.unroll_logic t1.term_type with
  | Ctype { tnode = TArray (_, opt_len) } ->
    let len =
      match opt_len, dyn_len with
      | Some len, None -> Logic_utils.expr_to_term ~coerce:true len
      | _   , Some len -> len
      | None, None ->
        Options.fatal "Unfolding array: cannot find a length"
    in
    pall_elems_pred ?loc (depth+1) t1 len pred
  | Ctype { tnode = TComp ci } ->
    pall_fields_pred ?loc depth t1 ci pred
  | _ -> pred ?loc t1
and pall_fields_pred ?loc ?(flex_mem_len=None) depth t1 ci pred =
  let eq dyn_len fi =
    let lval = match t1.term_node with TLval(lv) -> lv | _ -> assert false in
    let nlval = addTermOffsetLval (TField(fi, TNoOffset)) lval in
    let term = term ?loc (TLval nlval) (Ctype fi.ftype) in
    punfold_pred ?loc ~dyn_len depth term pred
  in
  let rec eqs_fields = function
    | [] -> []
    | [ x ] -> [ eq flex_mem_len x ]
    | x :: l -> let x' = eq None x in x' :: (eqs_fields l)
  in
  pands (eqs_fields (Option.get ci.cfields))

let punfold_flexible_struct_pred ?loc the_struct bytes_len pred =
  let struct_len = tinteger ?loc (sizeof the_struct.term_type) in
  let ci = match the_struct.term_type with
    | Ctype ({ tnode = TComp ci } as t) when Cil.has_flexible_array_member t -> ci
    | _ -> Options.fatal "Unfolding flexible on a non flexible structure"
  in
  let flex_type = Ctype (List.last (Option.get ci.cfields)).ftype in
  let flex_len = tminus bytes_len struct_len in
  let pall_fields_pred len =
    pall_fields_pred ?loc ~flex_mem_len:(Some len) 0 the_struct ci pred
  in
  plet_len_div_size ?loc ~name_ext:"flex" flex_type flex_len pall_fields_pred


let pseparated_memories ?loc p1 len1 p2 len2 =
  let b1 = tbuffer_range ?loc p1 len1 in
  let b2 = tbuffer_range ?loc p2 len2 in
  pseparated ?loc [ b1 ; b2 ]

let make_behavior
    ?(name=Cil.default_behavior_name)
    ?(assumes=[]) ?(requires=[]) ?(ensures=[])?(assigns=WritesAny)
    ?(alloc=FreeAllocAny) ?(extension=[])
    () =
  {
    b_name = name ;
    b_requires = requires ;
    b_assumes = assumes ;
    b_post_cond = ensures ;
    b_assigns = assigns ;
    b_allocation = alloc;
    b_extended = extension
  }

let default_comp_disj bhvs =
  let b_names = List.filter
      (fun b -> not (String.equal Cil.default_behavior_name b))
      (List.fold_left (fun l b -> b.b_name :: l) [] bhvs)
  in match b_names with
  | [] -> [], []
  | _  -> [b_names], [b_names]

let make_funspec bhvs ?(termination=None)
    ?(complete_disjoint=(default_comp_disj bhvs)) () =
  let complete, disjoint = complete_disjoint in
  {
    spec_behavior = bhvs ;
    spec_variant = None ;
    spec_terminates = termination ;
    spec_complete_behaviors = complete ;
    spec_disjoint_behaviors = disjoint
  }
