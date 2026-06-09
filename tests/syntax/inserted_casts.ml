include Plugin.Register
    (struct
      let name = "Test"
      let shortname = "test"
      let help = "unitary test of inserted cast hook"
    end)

let print_warning e ot nt =
  result "Inserting cast for expression %a of type %a to type %a@."
    Cil_printer.pp_exp e Cil_printer.pp_typ ot Cil_printer.pp_typ nt;
  nt
;;

Cil.typeForInsertedCast := print_warning
