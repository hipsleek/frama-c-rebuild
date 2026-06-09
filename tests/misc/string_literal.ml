open Cil_types
open Fun.Operators

let loc = Fileloc.unknown

let main () =
  let prj = Project.create "empty" in
  Project.set_current prj;
  let str_lit =
    Globals.Vars.add_string_literal ~loc (Str "example string literal")
  in
  let wstr_lit =
    Globals.Vars.add_string_literal ~loc
      (Wstr (List.map (Int64.of_int $ int_of_char) ['t'; 'e'; 's'; 't']))
  in
  Format.printf
    "String literal: %a@." Printer.pp_str_literal
    (Globals.Vars.get_string_literal str_lit);
  Format.printf
    "Wide string literal: %a@." Printer.pp_str_literal
    (Globals.Vars.get_string_literal wstr_lit);
  Demote_string_literal.demote str_lit;
  Demote_string_literal.demote wstr_lit;
  File.pretty_ast ()

let () = main ()
