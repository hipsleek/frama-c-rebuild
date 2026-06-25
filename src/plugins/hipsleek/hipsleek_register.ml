let run () =
  if Hipsleek_parameters.Enable.get () then begin
    Hipsleek_parameters.feedback "Running HipSleek plugin...";
    Ast.compute ();
    let file = Ast.get () in
    let t = Hipsleek_translate.translate file in
    let (results, proof_logs, proof_info, _ss_path) =
      Hipsleek_run.run
        ~ss_content:t.Hipsleek_translate.full_ss
        ~ss_spans:t.Hipsleek_translate.ss_spans
        ~linemaps:t.Hipsleek_translate.linemaps
    in
    Hipsleek_acsl.attach_all
      ~functions:t.Hipsleek_translate.functions
      ~preds:t.Hipsleek_translate.preds
      ~results
      ~proof_logs
      ~fidelity:t.Hipsleek_translate.fidelity;
    (* Populate the store read by the server panel (Hipsleek_server). *)
    Hipsleek_store.clear ();
    let verdict_of name =
      match
        List.find_opt (fun r -> r.Hipsleek_run.func_name = name) results
      with
      | Some { Hipsleek_run.verdict = Hipsleek_run.Success; _ } -> "SUCCESS"
      | Some { Hipsleek_run.verdict = Hipsleek_run.Fail; _ }    -> "FAIL"
      | Some { Hipsleek_run.verdict = Hipsleek_run.Error _; _ } -> "ERROR"
      | None -> "UNKNOWN"
    in
    List.iter (fun (name, _sl_opt, _proc) ->
        let obligations =
          match List.assoc_opt name proof_info with Some o -> o | None -> []
        in
        let fidelity =
          match List.assoc_opt name t.Hipsleek_translate.fidelity with
          | Some w -> w | None -> []
        in
        Hipsleek_store.set name
          { Hipsleek_store.verdict = verdict_of name; obligations; fidelity }
      ) t.Hipsleek_translate.functions
  end

let () = Boot.Main.extend run
