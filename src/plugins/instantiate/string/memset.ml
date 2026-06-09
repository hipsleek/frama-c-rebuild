(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Logic_const
open Basic_blocks

let function_name = "memset"
type key = (typ * int option)

let unexpected = Options.fatal "String.Memset: unexpected: %s"

module With_collection = struct
  module OptInt = Datatype.Option_with_collections (Datatype.Int)
  include Datatype.Pair_with_collections (Cil_datatype.Typ) (OptInt)
end

let rec any_char_composed_type t =
  match t.tnode with
  | _ when Ast_types.is_any_char t -> true
  | TArray (t, _) -> any_char_composed_type t
  | _ -> false

let rec base_char_type t =
  assert (any_char_composed_type t) ;
  match t.tnode with
  | _ when Ast_types.is_any_char t -> t
  | TArray (t, _) -> base_char_type t
  | _ -> assert false


let presult_ptr ?loc t ptr =
  prel ?loc (Req, (tresult ?loc t), ptr)

let pset_len_bytes_to_value ?loc ptr value bytes_len =
  let eq_value ?loc t = prel ?loc (Req, t, value) in
  plet_len_div_size ?loc ptr.term_type bytes_len
    (fun len -> punfold_all_elems_pred ?loc ptr len eq_value)

let pset_len_bytes_to_zero ?loc ptr bytes_len =
  let eq_value ?loc t =
    let value = match Ast_types.unroll_logic t.term_type with
      | Ctype { tnode = TPtr _ } -> term Tnull t.term_type
      | Ctype { tnode = TFloat _ } -> treal ?loc 0.
      | Ctype { tnode = (TInt _ | TEnum _) } -> tinteger ?loc 0
      | _ -> unexpected "non atomic type during equality generation"
    in
    prel ?loc (Req, t, value)
  in
  plet_len_div_size ?loc ptr.term_type bytes_len
    (fun len -> punfold_all_elems_pred ?loc ptr len eq_value)

let pset_len_bytes_all_bits_to_one ?loc ptr bytes_len =
  let nans = Logic_env.find_all_logic_functions "\\is_NaN" in
  let of_type t = function
    | { l_profile = [ x ] } -> Cil_datatype.Logic_type.equal x.lv_type t
    | _ -> false
  in
  let find_nan_for_type t = List.find (of_type t) nans in
  let all_bits_to_one ?loc t =
    match Ast_types.unroll_logic t.term_type with
    | Ctype { tnode = TFloat _ } ->
      papp ?loc ((find_nan_for_type t.term_type), [], [t])
    | Ctype { tnode = TPtr _ } ->
      pnot ?loc (pvalid_read ?loc (here_label, t))
    | Ctype ({ tnode = (TInt kind | TEnum { ekind = kind }) } as typ) ->
      let is_signed = Cil.isSigned kind in
      let bits = Cil.bitsSizeOfInt kind in
      let value =
        if is_signed then
          let zero = tinteger ?loc 0 in
          let zero = Logic_utils.mk_cast ?loc typ zero in
          term (TUnOp(BNot, zero)) t.term_type
        else
          let value = Cil.max_unsigned_number bits in
          term ?loc (TConst (Integer (value,None))) Linteger
      in
      prel ?loc (Req, t, value)
    | _ ->
      unexpected "non atomic type during equality generation"
  in
  plet_len_div_size ?loc ptr.term_type bytes_len
    (fun len -> punfold_all_elems_pred ?loc ptr len all_bits_to_one)


let generate_requires loc ptr value len =
  let open Cil in
  let bounds = match value with
    | None ->
      [ { (pcorrect_len_bytes ~loc ptr.term_type len)
          with pred_name = ["aligned_end"] } ]
    | Some value ->
      let low, up = match Ast_types.unroll_logic value.term_type with
        | Ctype { tnode = TInt ((IChar|ISChar|IUChar) as kind) } ->
          let bits = bitsSizeOfInt kind in
          let plus_one = Z.(add one) in
          let low, up = if (isSigned kind) then
              (min_signed_number bits), (plus_one (max_signed_number bits))
            else
              Z.zero, (plus_one (max_unsigned_number bits))
          in
          let integer ?loc i = term ?loc (TConst (Integer (i, None))) Linteger in
          (integer ~loc low), (integer ~loc up)
        | _ ->
          unexpected "non atomic type during value bounds generation"
      in
      [ { (pbounds_incl_excl ~loc low value up)
          with pred_name = [ "in_bounds_value" ] } ]
  in
  List.map new_predicate (bounds @ [
      Logic_const.pand ~loc ~names:["valid_dest"] (
        pobject_pointer ~loc here_label ptr,
        pvalid_len_bytes ~loc here_label ptr len)
    ])

let generate_assigns loc t ptr value len =
  let ptr_range = new_identified_term (tunref_range_bytes_len ~loc ptr len) in
  let value = Option.to_list (Option.map new_identified_term value) in
  let set = ptr_range, From value in
  let result = new_identified_term (tresult t) in
  let res = result, From [ new_identified_term ptr ] in
  Writes [ set ; res ]

let generate_ensures e loc t ptr value len =
  let pred_name = [ "set_content" ] in
  let content = match e, value with
    | None, Some value ->
      [ { (pset_len_bytes_to_value ~loc ptr value len) with pred_name } ]
    | Some 0, None ->
      [ { (pset_len_bytes_to_zero ~loc ptr len) with pred_name } ]
    | Some 255, None ->
      [ { (pset_len_bytes_all_bits_to_one ~loc ptr len) with pred_name }]
    | _ ->
      unexpected "ill-formed key in ensure generation"
  in
  List.map (fun p -> Normal, new_predicate p) (content @ [
      { (presult_ptr ~loc t ptr) with pred_name = [ "result"] }
    ])

let generate_spec (_t, e) loc { svar = vi } =
  let (cptr, cvalue, clen) = match Cil.getFormalsDecl vi with
    | [ ptr ; value ; len ] -> ptr, (Some value), len
    | [ ptr ; len ] -> ptr, None, len
    | _ -> unexpected "ill-formed fundec in specification generation"
  in
  let t = cptr.vtype in
  let ptr = cvar_to_tvar cptr in
  let len = cvar_to_tvar clen in
  let value = Option.map cvar_to_tvar cvalue in
  let requires = generate_requires loc ptr value len in
  let assigns  = generate_assigns loc t ptr value len in
  let ensures  = generate_ensures e loc t ptr value len in
  make_funspec [make_behavior ~requires ~assigns ~ensures ()] ()

let memset_value e =
  let ff = 255z in
  match (Cil.constFold false e).enode with
  | Const(CInt64(ni, _, _)) when Z.is_zero ni -> Some 0
  | Const(CInt64(ni, _, _)) when Z.equal ni ff -> Some 255
  | _ -> None

let rec contains_union_type t =
  match Ast_types.unroll_node t with
  | TComp { cstruct = false } ->
    true
  | TComp { cfields = Some fields } ->
    List.exists contains_union_type (List.map (fun f -> f.ftype) fields)
  | TArray (t, _) ->
    contains_union_type t
  | _ -> false

let well_typed_call _ret _fct = function
  | [ ptr ; value ; _ ] ->
    begin match Mem_utils.exp_type_of_pointed ptr, memset_value value with
      | (No_pointed | Of_null _) , _ -> false
      | Value_of t , _ when any_char_composed_type t -> true
      | Value_of t , _ when contains_union_type t -> false
      | Value_of t , _ when Ast_types.is_void t -> false
      | Value_of t , _ when not (Cil.isCompleteType t) -> false
      | _, None -> false
      | _, Some _ -> true
    end
  | _ -> false

let key_from_call _ret _fct = function
  | [ ptr ; value ; _ ] ->
    begin match Mem_utils.exp_type_of_pointed ptr, memset_value value with
      | Value_of t, _ when any_char_composed_type t -> t, None
      | Value_of t, value when not (contains_union_type t) -> t, value
      | _ , _ -> unexpected "trying to generate a key on an ill-typed call"
    end
  | _ -> unexpected "trying to generate a key on an ill-typed call"

let char_prototype t =
  assert (any_char_composed_type t) ;
  let params = [
    ("ptr", Cil_const.mk_tptr t, []) ;
    ("value", base_char_type t, []) ;
    ("len", size_t(), [])
  ] in
  Cil_const.(mk_tfun (mk_tptr t) (Some params) false)

let non_char_prototype t =
  let params = [
    ("ptr", Cil_const.mk_tptr t, []) ;
    ("len", size_t(), [])
  ] in
  Cil_const.(mk_tfun (mk_tptr t) (Some params) false)

let generate_prototype = function
  | t, _ when any_char_composed_type t ->
    let name = function_name ^ "_" ^ (string_of_typ t) in
    let fun_type = char_prototype t in
    name, fun_type
  | t, Some x when not (contains_union_type t) && (x = 0 || x = 255) ->
    let ext = if x = 0 then "_0" else if x = 255 then "_FF" else assert false in
    let name = function_name ^ "_" ^ (string_of_typ t) ^ ext in
    let fun_type = non_char_prototype t in
    name, fun_type
  | _, _ ->
    unexpected "trying to generate a prototype on an ill-typed call"

let retype_args (_t, e) args =
  match e, args with
  | None, [ ptr ; v ; n ] ->
    let ptr = Cil.stripCasts ptr in
    let base_type = match Mem_utils.exp_type_of_pointed ptr with
      | Value_of t -> base_char_type t
      | _ -> unexpected "trying to retype arguments on an ill-typed call"
    in
    let v = Cil.mkCast ~newt:base_type (Cil.stripCasts v) in
    [ ptr ; v ; n ]
  | Some fv, [ ptr ; v ; n ] ->
    let ptr = Cil.stripCasts ptr in
    assert (match memset_value v with Some x when x = fv -> true | _ -> false) ;
    [ ptr ; n ]
  | _ ->
    unexpected "trying to retype arguments on an ill-typed call"

let args_for_original (_t , e) args =
  match e with
  | None -> args
  | Some n ->
    let loc = Fileloc.unknown in
    match args with
    | [ ptr ; len ] -> [ ptr ; (Cil.integer ~loc n) ; len]
    | _ ->
      unexpected "wrong number of arguments replacing call"

let () = Transform.register (module struct
    module Hashtbl = With_collection.Hashtbl
    type override_key = key

    let function_name = function_name
    let well_typed_call = well_typed_call
    let key_from_call = key_from_call
    let retype_args = retype_args
    let generate_prototype = generate_prototype
    let generate_spec = generate_spec
    let args_for_original = args_for_original
  end)
