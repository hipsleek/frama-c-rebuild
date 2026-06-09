(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* infinite sequence whose first elements are the characters of the string
   and the rest are [0]. We'll use it later to initialize an array of
   arbitrary length, padding with [0]'s as appropriate (or truncating if
   the declared length is smaller than the length of the string literal)
*)
let to_seq_string loc s =
  let mk_char_exp c = Cil.new_exp ~loc (Const (CChr c)) in
  let es = Seq.map mk_char_exp (String.to_seq s) in
  Seq.append es (Seq.repeat (mk_char_exp '\000'))

(* same as to_seq_string above, but for wide strings *)
let to_seq_wstring loc s =
  let kind = Machine.wchar_kind () in
  let z_of_wchar =
    if Machine.char_is_unsigned() then Z.of_int64_unsigned else Z.of_int64
  in
  let mk_wchar_exp w = Cil.kinteger64 ~loc ~kind (z_of_wchar w) in
  let es = Seq.map mk_wchar_exp (List.to_seq s) in
  Seq.append es (Seq.repeat (Cil.kinteger ~loc kind 0))

(* finite sequence of indices of elements to initialize. *)
let to_seq_idx up =
  let gen i =
    if Z.lt i up then
      Some (i, Z.succ i)
    else None
  in
  Seq.unfold gen Z.zero

let init_idx loc idx elt =
  let kind = Machine.sizeof_kind () in
  Index (Cil.kinteger64 ~loc ~kind idx, NoOffset), SingleInit elt

let mk_array_init loc dest src =
  let s = Globals.Vars.get_string_literal src in
  let len, elts =
    match s with
    | Str s -> String.length s, to_seq_string loc s
    | Wstr s -> List.length s, to_seq_wstring loc s
  in
  let _,alen = Ast_types.array_elem_type_and_size dest.vtype in
  let alen = Option.bind Cil.constFoldToInt alen in
  let alen = Option.value ~default:(Z.of_int (len + 1)) alen in
  let idx = to_seq_idx alen in
  let l = Seq.map2 (init_idx loc) idx elts in
  CompoundInit (dest.vtype,List.of_seq l)

class demote vi =
  object
    inherit Visitor.frama_c_inplace

    method! vinst i =
      match i with
      | Local_init(
          dest,AssignInit(SingleInit { enode = Lval(Var src,NoOffset) }),loc)
        when Cil_datatype.Varinfo.equal vi src ->
        let new_init = mk_array_init loc dest src in
        Ast.mark_as_changed();
        Cil.ChangeTo [Local_init(dest,AssignInit new_init,loc)]
      | _ -> Cil.SkipChildren
  end

let demote vi =
  if Ast_info.is_string_literal vi then begin
    let vis = new demote vi in
    Visitor.visitFramacFileSameGlobals vis (Ast.get());
    vi.vattr <- Ast_attributes.(drop fc_literal vi.vattr)
  end
