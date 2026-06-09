(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Side Condition Helpers                                             --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype

let taddrof ?loc tlv =
  Logic_utils.mk_logic_AddrOf ?loc tlv @@ Cil.typeOfTermLval tlv

let addrof ?loc lv = taddrof ?loc @@ Logic_utils.lval_to_term_lval lv

let pnull ?loc ?names ~eq addr =
  let null = Logic_const.term ?loc Tnull addr.term_type in
  let rel = if eq then Req else Rneq in
  Logic_const.prel ?loc ?names (rel,addr,null)

let pvalid ?loc ?names ?(label=Logic_const.here_label) addr =
  Logic_const.pvalid ?loc ?names (label,addr)

let pvalid_read ?loc ?names ?(label=Logic_const.here_label) addr =
  Logic_const.pvalid_read ?loc ?names (label,addr)

let pvalid_pointer ?loc ?names ?(label=Logic_const.here_label) addr =
  Logic_const.por ?loc ?names
    ( pnull ?loc ?names ~eq:true addr ,
      if Ast_types.is_logic_fun_ptr addr.term_type
      then Logic_const.pvalid_function ?loc ?names addr
      else Logic_const.pobject_pointer ?loc ?names (label, addr) )

let pinitialized ?loc ?names ?(label=Logic_const.here_label) addr =
  Logic_const.pinitialized ?loc ?names (label,addr)

let paligned ?loc ?names addr te =
  let size = Logic_const.term ?loc (TAlignOf te) Linteger in
  Logic_const.paligned ?loc ?names (addr,size)

(* -------------------------------------------------------------------------- *)
(* --- Valid Region Built-in                                              --- *)
(* -------------------------------------------------------------------------- *)

let l_valid_region = "\\valid_region"
let is_valid_region lf = lf.l_var_info.lv_name = l_valid_region

let () = Logic_builtin.register {
    bl_name = l_valid_region;
    bl_labels = [FormalLabel "A"] ;
    bl_params = [] ;
    bl_type = None ;
    bl_profile = [
      "ptr", Ctype Cil_const.voidConstPtrType ;
      "size", Linteger ;
    ];
  }

let pvalid_region ?loc ?names ?(label=Logic_const.here_label) addr =
  let f = List.hd @@ Logic_env.find_all_logic_functions l_valid_region in
  let te = Logic_typing.ctype_of_pointed addr.term_type in
  let size = Logic_const.term ?loc (TSizeOf te) Linteger in
  Logic_const.papp ?loc ?names (f,[label],[addr;size])

(* -------------------------------------------------------------------------- *)
(* --- L-Val Kinds                                                        --- *)
(* -------------------------------------------------------------------------- *)

type lkind = {
  host : varinfo option ;
  indexed : bool ;
  aligned : bool ;
}

let pp_kind fmt kd =
  begin
    match kd.host with
    | None -> Format.pp_print_string fmt "(*"
    | Some v -> Format.fprintf fmt "(%s" v.vname
  end ;
  if not kd.indexed then Format.pp_print_string fmt ",misindexed" ;
  if not kd.aligned then Format.pp_print_string fmt ",misaligned" ;
  Format.pp_print_string fmt ")"

let safe = { host = None ; indexed = true ; aligned = true }
let unsafe = { host = None ; indexed = false ; aligned = false }

let rec kind e =
  match e.enode with
  | Lval _ -> safe
  | AddrOf lv | StartOf lv -> lkind lv
  | BinOp((PlusPI|MinusPI),p,_,_) -> { (kind p) with indexed = false }
  | CastE(ty,p) when Ast_types.is_ptr ty -> { (kind p) with aligned = false }
  | _ -> unsafe

and lkind (h,o) =
  let kd = hkind h in
  if kd.indexed && safe_array_offset (Cil.typeOfLhost h) o then kd
  else { kd with indexed = false }

and hkind = function
  | Var v -> { safe with host = Some v }
  | Mem e -> kind e

and safe_array_offset t = function
  | NoOffset -> true
  | Field(fd,o) -> safe_array_offset fd.ftype o
  | Index(_,o) ->
    Kernel.SafeArrays.get () &&
    let n = Ast_info.direct_array_size t in
    not (Z.is_zero n) &&
    safe_array_offset (Ast_types.direct_element_type t) o

let rec term_kind t =
  match t.term_node with
  | TLval _ -> safe
  | TAddrOf lv | TStartOf lv -> term_lkind lv
  | TBinOp((PlusPI|MinusPI),p,_) ->
    { (term_kind p) with indexed = false }
  | TCast(_,Ctype ty,p) when Ast_types.is_ptr ty ->
    { (term_kind p) with aligned = true }
  | _ -> unsafe

and term_lkind (h,o) =
  let kd = term_hkind h in
  if kd.indexed && safe_array_toffset (Cil.typeOfTermLval (h,TNoOffset)) o
  then kd
  else { kd with indexed = false }

and term_hkind = function
  | TVar { lv_origin = (Some _ as host) } -> { safe with host }
  | TMem e -> term_kind e
  | _ -> safe

and safe_array_toffset t = function
  | TNoOffset -> true
  | TField(fd,o) -> safe_array_toffset (Ctype fd.ftype) o
  | TModel(fm,o) -> safe_array_toffset fm.mi_field_type o
  | TIndex(_,o) ->
    Kernel.SafeArrays.get () &&
    let n = Ast_info.direct_array_size @@ Logic_utils.logicCType t in
    not (Z.is_zero n) &&
    safe_array_toffset (Logic_utils.type_of_array_elem t) o

(* -------------------------------------------------------------------------- *)
(* --- Side Condition Generators                                          --- *)
(* -------------------------------------------------------------------------- *)

type residual = [ `Default | `True | `False ]

let pp_residual fmt = function
  | `Default -> Format.pp_print_string fmt "default"
  | `True -> Format.pp_print_string fmt "true"
  | `False -> Format.pp_print_string fmt "false"

let rpath kd =
  if kd.indexed && kd.aligned then `True else `Default

(* -------------------------------------------------------------------------- *)
(* ---  Validity                                                          --- *)
(* -------------------------------------------------------------------------- *)

let in_scope v stmt =
  List.exists
    (fun b ->
       List.exists (Varinfo.equal v) b.blocals
    ) @@ Kernel_function.find_all_enclosing_blocks stmt

let rallocated kinstr v =
  if v.vglob || v.vformal then `True else
    match kinstr with
    | Kglobal -> `Default
    | Kstmt stmt -> if in_scope v stmt then `True else `False

let rvalid ?(writing=false) kinstr node kd =
  if not kd.aligned then `Default
  else
    match kd.host with
    | Some v ->
      if writing && Attr.is_const v then `False else
      if kd.indexed then rallocated kinstr v else `Default
    | None ->
      let flags = Memory.flags node in
      if writing && Attr.mem `Readonly flags then `False else `Default

(* -------------------------------------------------------------------------- *)
(* ---  Initialized                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rinitialized node kd =
  match kd.host with
  | Some _ ->
    let flags = Memory.flags node in
    let garbage = Attr.mem `Garbage flags in
    if not garbage then `True else `Default
  | None -> `Default

(* -------------------------------------------------------------------------- *)
(* ---  Aligned                                                           --- *)
(* -------------------------------------------------------------------------- *)

let raligned node ~bits ?(default=true) kd =
  if (default && kd.aligned) || (Memory.size node mod bits = 0)
  then `True
  else `Default

(* -------------------------------------------------------------------------- *)
