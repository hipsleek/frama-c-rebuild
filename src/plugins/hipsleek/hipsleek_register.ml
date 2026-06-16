let run () =
  if Hipsleek_parameters.Enable.get () then begin
    Hipsleek_parameters.feedback "Running HipSleek plugin...";
    Ast.compute ();
    let file = Ast.get () in
    let ss_content = Hipsleek_translate.translate file in
    ignore (Hipsleek_run.run ~ss_content)
  end

let () = Boot.Main.extend run
