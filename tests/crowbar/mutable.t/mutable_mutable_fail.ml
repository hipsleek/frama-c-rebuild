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
    | Instr (Set ((Var x,offset),_,_)) ->
      Format.printf "@[<v2>Variable %s :@;Type  : %a@;Offset: %a@]@."
        x.vname Printer.pp_typ x.vtype Printer.pp_offset offset;
      assert
        (not (Ast_types.has_attribute "const" (Cil.typeOffset x.vtype offset)))
    | _ -> assert false

let () = Boot.Main.extend main
