open Cil_types


let is_empty_ast () =
  let ast = Ast.get () in
  ast.globals = []

let main () =
  Ast.compute ();
  if not (is_empty_ast ()) then
    let def =
      Kernel_function.get_definition
        (Globals.Functions.find_def_by_name "f")
    in
    let s = List.hd (def.sbody.bstmts) in
    match s.skind with
    | Instr (Set (_,{ enode = Lval (Var x,offset) },_)) ->
      let is_const =
        Ast_types.has_attribute "const" (Cil.typeOffset x.vtype offset)
      in
      Format.printf "@[<v2>Variable %s :@;Type  : %a@;Offset: %a@;Is const: %b@]@."
        x.vname Printer.pp_typ x.vtype Printer.pp_offset offset is_const
    | _ -> assert false

let () = Boot.Main.extend main
