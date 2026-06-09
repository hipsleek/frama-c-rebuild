open Cil_types

let loc = Fileloc.unknown

let field_name =
  let count = ref 0 in
  fun () ->
    let c = Char.chr(Char.code 'a' + (!count mod 26)) in
    incr count;
    String.make 1 c

let struct_name =
  let count = ref 0 in
  fun () ->
    let c = Char.chr(Char.code 'A' + (!count mod 26)) in
    let base = String.make 1 c in
    let res =
      if !count < 26 then base else base ^ "_" ^ (string_of_int (!count / 26))
    in
    incr count; res

type attr_kind = NoAttr | Const | Mutable

let attr_of_kind =
  function NoAttr | Const -> [] | Mutable -> [ (Ast_attributes.frama_c_mutable, []) ]

let tattr_of_kind =
  function NoAttr | Mutable -> [] | Const -> [ ("const", []) ]

let is_const = function
  | NoAttr | Mutable -> false
  | Const -> true

let merge_kind field_kind subobj_kind =
  match field_kind, subobj_kind with
  | _, NoAttr -> field_kind
  | _, Mutable -> Mutable
  | _, Const -> Const

let gen_attr =
  Crowbar.(choose [ const NoAttr; const Const; const Mutable ])

let mk_type ftype attr =
  let tname = struct_name () in
  let fname = field_name () in
  let mk_type _ =
    Some [ fname, ftype, None, None, attr, loc ]
  in
  Cil_const.mkCompInfo true tname ~norig:tname mk_type []

let mk_int_type field_kind =
  let field_attr = attr_of_kind field_kind in
  let tattr = tattr_of_kind field_kind in
  [ mk_type (Cil_const.mk_tint ~tattr IInt) field_attr ], field_kind

let mk_composite_type field_kind (subtypes, subkind) =
  let field_attr = attr_of_kind field_kind in
  let tattr = tattr_of_kind field_kind in
  let subtype = List.hd subtypes in
  let kind = merge_kind field_kind subkind in
  let field_type = Cil_const.mk_tcomp ~tattr subtype in
  (mk_type field_type field_attr) :: subtypes, kind

let rec mk_offset { cfields } =
  match cfields with
  | None | Some [] -> NoOffset
  | Some fields ->
    let field = List.hd fields in
    let offset =
      match field.ftype.tnode with TComp comp -> mk_offset comp | _ -> NoOffset
    in
    Field (field, offset)

let gen_type =
  let open Crowbar in
  fix
    (fun gen_type ->
       choose
         [ map [ gen_attr ] mk_int_type;
           map [ gen_attr; gen_type ] mk_composite_type ])

let generate_failure_file name is_const types =
  let file = Crowbar_utils.generate_cil_file name in
  let typ = List.hd types in
  let x =
    Cil.makeGlobalVar "x" (Cil_const.mk_tcomp typ)
  in
  let y =
    Cil.makeGlobalVar "y" (Cil_const.intType)
  in
  let lvx = Var x, mk_offset typ in
  let lvy = Var y, NoOffset in
  let lv, rv = if is_const then lvy, lvx else lvx, lvy in
  let instr = Set (lv, Cil.new_exp ~loc (Lval rv),loc) in
  let s = Cil.mkStmtOneInstr instr in
  let b = Cil.mkBlock [ s ] in
  let ft = Cil_const.(mk_tfun voidType (Some []) false) in
  let f = Cil.makeGlobalVar "f" ft in
  let fdef =
    { svar = f;
      sformals = [];
      slocals = [];
      smaxid = 0;
      sbody = b;
      smaxstmtid = None;
      sallstmts = [ s ];
      sspec = Cil.empty_funspec () }
  in
  let file =
    { file with
      globals =
        List.rev_map (fun typ -> GCompTag (typ,loc)) types @
        [ GVarDecl (x,loc); GVarDecl(y,loc); GFun (fdef, loc) ]
    }
  in
  Crowbar_utils.generate_file file;
  Filepath.to_string_abs file.fileName

let test (types, kind) =
  let out_type = List.hd types in
  let offset = mk_offset out_type in
  let inner_type =
    Cil.typeOffset (Cil_const.mk_tcomp out_type) offset
  in
  let is_const = is_const kind in
  let kind = if is_const then "const" else "mutable" in
  let has_const = Ast_types.has_attribute "const" inner_type in
  if is_const && not has_const then begin
    let filename = generate_failure_file kind is_const types in
    Crowbar.fail
      ("typeOffset should have marked a field as const. \
        File saved in '" ^ filename ^ "'.")
  end
  else if not is_const && has_const then begin
    let filename = generate_failure_file kind is_const types in
    Crowbar.fail
      ("typeOffset declared const a field that should have been mutable. \
        File saved in '" ^ filename ^ "'.")
  end
  else true

let f () =
  Crowbar.add_test ~name:"mutable typeOffset" [ gen_type ] @@
  (fun x -> Crowbar.check (test x))

let () =
  Crowbar_utils.run "mutable" f
