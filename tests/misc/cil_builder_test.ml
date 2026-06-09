module Build = Cil_builder.Pure

let run () =
  let kf, _ = Globals.entry_point () in
  let stmt = Kernel_function.find_first_stmt kf in
  let loc = Kernel_function.get_location kf in
  let into = Kernel_function.get_definition kf in
  let f = Globals.Functions.find_by_name "f" in
  stmt.skind <- Build.(
      cil_stmtkind ~into ~loc @@ sequence @@
      let+ i = local int "x" in [
        i := (of_int 1);
        if_ (i < of_int 3)
          ~then_:[incr i]
          ~else_:[i := zero];
        call f [i]
      ]
    );
  Kernel.result "%a" Printer.pp_file (Ast.get ())


let () =
  Boot.Main.extend run
