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
      (* Not "SUCCESS": the proof assumes a loop spec that did not verify. The
         panel maps anything outside SUCCESS/FAIL/ERROR to its amber "unknown"
         badge, which is the right reading -- neither established nor refuted. *)
      | Some { Hipsleek_run.verdict = Hipsleek_run.Success_assuming _; _ } ->
        "ASSUMED"
      | Some { Hipsleek_run.verdict = Hipsleek_run.Fail; _ }    -> "FAIL"
      | Some { Hipsleek_run.verdict = Hipsleek_run.Error _; _ } -> "ERROR"
      | None -> "UNKNOWN"
    in
    List.iter (fun (name, _sl_opt, proc) ->
        let obligations =
          match List.assoc_opt name proof_info with Some o -> o | None -> []
        in
        let fidelity =
          match List.assoc_opt name t.Hipsleek_translate.fidelity with
          | Some w -> w | None -> []
        in
        (* C source line for each .ss line of [proc], so the GUI can link the
           generated .ss to the source (and thus to the obligations, which are
           also keyed by C line). [proc]'s lines are 1-based and relative, so
           cline_of with lo=1 maps line i to its C source line. *)
        let linemap =
          match List.assoc_opt name t.Hipsleek_translate.linemaps with
          | Some m -> m | None -> []
        in
        let nlines = List.length (String.split_on_char '\n' proc) in
        let ss_clines =
          List.init nlines (fun i -> Hipsleek_run.cline_of linemap ~lo:1 (i + 1))
        in
        Hipsleek_store.set name
          { Hipsleek_store.verdict = verdict_of name; obligations; fidelity;
            ss = proc; ss_clines }
      ) t.Hipsleek_translate.functions
  end

let () = Boot.Main.extend run
