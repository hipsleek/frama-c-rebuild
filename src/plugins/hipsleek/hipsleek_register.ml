let run () =
  if Hipsleek_parameters.Enable.get () then begin
    Hipsleek_parameters.feedback "Running HipSleek plugin...";
    Ast.compute ();
    let file = Ast.get () in
    let t = Hipsleek_translate.translate file in
    let (results, proof_logs, _ss_path) =
      Hipsleek_run.run
        ~ss_content:t.Hipsleek_translate.full_ss
        ~ss_spans:t.Hipsleek_translate.ss_spans
    in
    Hipsleek_acsl.attach_all
      ~functions:t.Hipsleek_translate.functions
      ~preds:t.Hipsleek_translate.preds
      ~results
      ~proof_logs
      ~fidelity:t.Hipsleek_translate.fidelity
  end

let () = Boot.Main.extend run
