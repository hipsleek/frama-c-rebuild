type verdict = Success | Fail | Error of string

type result = {
  func_name : string;
  verdict   : verdict;
}

(* ------------------------------------------------------------------ *)
(* hip.exe path resolution                                             *)
(* ------------------------------------------------------------------ *)

let resolve_hip_path () =
  let explicit = Hipsleek_parameters.HipPath.get () in
  if explicit <> "" then explicit
  else
    (* frama-c is at _build/default/src/init/boot/empty_file.exe;
       hip.exe is at _build/default/hipsleek/hip.exe (3 dirs up). *)
    let candidates = [
      Filename.concat
        (Filename.dirname Sys.executable_name)
        "../../../hipsleek/hip.exe" ;
      "hip.exe" ;
    ] in
    match List.find_opt Sys.file_exists candidates with
    | Some p -> p
    | None   -> "hip.exe"

(* ------------------------------------------------------------------ *)
(* Output parser                                                        *)
(* ------------------------------------------------------------------ *)

(* HipSleek emits lines like:
     Procedure append$node*~node* SUCCESS
     Procedure length$node*       FAIL
   We extract the procedure base-name (before '$') and the verdict. *)

let parse_output output =
  let lines = String.split_on_char '\n' output in
  List.filter_map (fun line ->
      let line = String.trim line in
      let prefix = "Procedure " in
      let plen = String.length prefix in
      if String.length line > plen
      && String.sub line 0 plen = prefix then begin
        let rest = String.sub line plen (String.length line - plen) in
        (* rest = "funcname$sig SUCCESS" or "funcname$sig FAIL" *)
        let parts = String.split_on_char ' ' rest in
        match List.rev parts with
        | verdict_str :: name_parts ->
          let mangled = String.concat " " (List.rev name_parts) in
          let func_name =
            match String.index_opt mangled '$' with
            | Some i -> String.sub mangled 0 i
            | None   -> mangled
          in
          let verdict_str =
            String.trim (String.concat "" (String.split_on_char '.' verdict_str))
          in
          let starts s prefix =
            let n = String.length prefix in
            String.length s >= n && String.sub s 0 n = prefix
          in
          let verdict = match verdict_str with
            | s when starts s "SUCCESS" -> Success
            | s when starts s "FAIL"    -> Fail
            | s                         -> Error s
          in
          Some { func_name; verdict }
        | [] -> None
      end
      else None
    ) lines

(* ------------------------------------------------------------------ *)
(* Report results as Frama-C messages                                  *)
(* ------------------------------------------------------------------ *)

let report results =
  List.iter (fun r ->
      match r.verdict with
      | Success ->
        Hipsleek_parameters.result "[HipSleek] %s: SUCCESS" r.func_name
      | Fail ->
        Hipsleek_parameters.warning "[HipSleek] %s: FAIL" r.func_name
      | Error s ->
        Hipsleek_parameters.error "[HipSleek] %s: %s" r.func_name s
    ) results

(* ------------------------------------------------------------------ *)
(* Subprocess invocation                                                *)
(* ------------------------------------------------------------------ *)

let run_hip ~ss_file =
  let hip = resolve_hip_path () in
  if not (Sys.file_exists hip) then begin
    Hipsleek_parameters.error "hip.exe not found (tried: %s)" hip;
    []
  end else begin
    Hipsleek_parameters.feedback "Invoking: %s %s" hip ss_file;
    let cmd = hip ^ " " ^ Filename.quote ss_file ^ " 2>&1" in
    let ic = Unix.open_process_in cmd in
    let buf = Buffer.create 512 in
    (try while true do
           Buffer.add_string buf (input_line ic);
           Buffer.add_char buf '\n'
         done
     with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    let output = Buffer.contents buf in
    Hipsleek_parameters.debug "hip output:@\n%s" output;
    parse_output output
  end

(* ------------------------------------------------------------------ *)
(* Entry point: write .ss file, run hip.exe, report                    *)
(* ------------------------------------------------------------------ *)

let run ~ss_content =
  let dir =
    let d = Hipsleek_parameters.OutputDir.get () in
    if d <> "" then d else Filename.get_temp_dir_name ()
  in
  let ss_file = Filename.concat dir "hipsleek_out.ss" in
  let oc = open_out ss_file in
  output_string oc ss_content;
  close_out oc;
  Hipsleek_parameters.feedback "Generated .ss file: %s" ss_file;
  let results = run_hip ~ss_file in
  report results;
  results
