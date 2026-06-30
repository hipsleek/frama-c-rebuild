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

let starts_with s prefix =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

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

(* Verdicts are turned into ACSL extensions + property statuses by
   Hipsleek_acsl (so the SL spec shows on the AST and the verdict becomes a
   real property). This module only runs hip and parses the verdicts. *)

(* ------------------------------------------------------------------ *)
(* ESL proof-log parsing (-hipsleek-proof-log)                         *)
(*                                                                     *)
(* With --esl --dump-slk-proof, HipSleek writes a SLEEK entailment log *)
(* to logs/sleek_log_<mangled>.txt (relative to its CWD). Each entry:  *)
(*                                                                     *)
(*   id: 23; ... line: 28; ... kind: POST; ...                         *)
(*    checkentail <ante> |-  <conseq>.                                 *)
(*   ... res:  1[ <residual> ]                                          *)
(*                                                                     *)
(* line: indexes the generated .ss; we bucket each obligation into the *)
(* function whose .ss line span contains it. Kinds PRE/POST/BIND/      *)
(* PRE_REC are real proof obligations; Pred_Check_Inv (prelude) falls  *)
(* outside all function spans and is dropped.                          *)
(* ------------------------------------------------------------------ *)

(* Non-alphanumeric -> '_', matching how HipSleek names the log file. *)
let mangle s =
  String.map (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
      || (c >= '0' && c <= '9') then c else '_') s

(* Index of substring [sub] in [s] starting at [from], or None. *)
let find_sub s sub from =
  let ls = String.length s and lsub = String.length sub in
  let rec go i =
    if i + lsub > ls then None
    else if String.sub s i lsub = sub then Some i
    else go (i + 1)
  in
  if lsub = 0 then Some from else go from

(* Read the integer following "key" in a header line, e.g. key="line: ". *)
let field_int header key =
  match find_sub header key 0 with
  | None -> None
  | Some i ->
    let start = i + String.length key in
    let j = ref start in
    let n = String.length header in
    while !j < n && header.[!j] >= '0' && header.[!j] <= '9' do incr j done;
    if !j > start then int_of_string_opt (String.sub header start (!j - start))
    else None

(* Read the token following "key" up to ';', e.g. key="kind: ". *)
let field_str header key =
  match find_sub header key 0 with
  | None -> None
  | Some i ->
    let start = i + String.length key in
    let j = ref start in
    let n = String.length header in
    while !j < n && header.[!j] <> ';' do incr j done;
    Some (String.trim (String.sub header start (!j - start)))

(* Collapse runs of whitespace/newlines into single spaces. *)
let squeeze s =
  let b = Buffer.create (String.length s) in
  let prev_space = ref false in
  String.iter (fun c ->
      let is_space = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
      if is_space then
        (if not !prev_space then Buffer.add_char b ' '; prev_space := true)
      else (Buffer.add_char b c; prev_space := false)
    ) s;
  String.trim (Buffer.contents b)

(* Replace all (non-overlapping) occurrences of [sub] in [s] with [rep]. *)
let replace_all s sub rep =
  if sub = "" then s
  else begin
    let b = Buffer.create (String.length s) in
    let lsub = String.length sub in
    let i = ref 0 in
    let n = String.length s in
    while !i < n do
      if !i + lsub <= n && String.sub s !i lsub = sub then
        (Buffer.add_string b rep; i := !i + lsub)
      else (Buffer.add_char b s.[!i]; incr i)
    done;
    Buffer.contents b
  end

(* Strip HipSleek's internal noise from an entailment so it reads closer to the
   separation-logic the user wrote: drop {FLOW,...} blocks and the
   termination markers MayLoop[]/Term[], then tidy leftover separators. *)
let clean_entail s =
  (* drop "{FLOW,...}" blocks and a directly-following "[]" *)
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if !i + 6 <= n && String.sub s !i 6 = "{FLOW," then begin
      let j = ref (!i + 6) in
      while !j < n && s.[!j] <> '}' do incr j done;
      i := (if !j < n then !j + 1 else n);
      if !i + 1 < n && s.[!i] = '[' && s.[!i + 1] = ']' then i := !i + 2
    end else (Buffer.add_char b s.[!i]; incr i)
  done;
  let s = Buffer.contents b in
  let s = replace_all s "MayLoop[]" "" in
  let s = replace_all s "Term[]" "" in
  let s = squeeze s in
  (* tidy leftover '&' separator artifacts *)
  let s = replace_all s "&&" "&" in
  let s = replace_all s "& &" "&" in
  let s = replace_all s "& ." "." in
  let s = replace_all s "&." "." in
  let s = replace_all s "& )" ")" in
  let s = replace_all s "&)" ")" in
  let s = replace_all s "& |-" "|-" in
  let s = replace_all s "&|-" "|-" in
  let s = replace_all s "|- &" "|-" in
  let s = squeeze s in
  (* drop the "checkentail " lead-in and the trailing '.'/'&' *)
  let s =
    let p = "checkentail " in let lp = String.length p in
    if String.length s >= lp && String.sub s 0 lp = p
    then String.sub s lp (String.length s - lp) else s
  in
  let s = String.trim s in
  let drop_last s =
    let n = String.length s in
    if n > 0 && (s.[n-1] = '.' || s.[n-1] = '&')
    then String.trim (String.sub s 0 (n-1)) else s
  in
  drop_last (drop_last s)

type entry = { e_line : int; e_kind : string; e_oblig : string; e_proved : bool }

(* Parse one entry block (the "id:" header + the lines that follow it). *)
let parse_entry header body_lines =
  match field_int header "line: ", field_str header "kind: " with
  | Some line, Some kind ->
    let body = String.concat "\n" body_lines in
    (* Obligation: from "checkentail" up to where "ho_vars"/"res:" begins. *)
    let oblig =
      match find_sub body "checkentail" 0 with
      | None -> ""
      | Some i ->
        let rest = String.sub body i (String.length body - i) in
        let cut =
          match find_sub rest "ho_vars" 0 with
          | Some k -> k
          | None ->
            (match find_sub rest "\nres:" 0 with
             | Some k -> k | None -> String.length rest)
        in
        squeeze (String.sub rest 0 cut)
    in
    (* Proved iff the residual context list is non-empty (res:  N[...], N>=1).
       Anchor to the start-of-line "res:" result line: the consequent of an
       entailment over "res" (e.g. "|- res::ll<>") also contains "res:", so a
       naive search would match the conseq instead of the actual result. *)
    let proved =
      let idx =
        match find_sub body "\nres:" 0 with
        | Some i -> Some (i + 1)
        | None ->
          if String.length body >= 4 && String.sub body 0 4 = "res:"
          then Some 0 else None
      in
      match idx with
      | None -> false
      | Some i ->
        let after =
          String.trim (String.sub body (i + 4) (String.length body - i - 4)) in
        String.length after > 0 && after.[0] >= '1' && after.[0] <= '9'
    in
    Some { e_line = line; e_kind = kind; e_oblig = oblig; e_proved = proved }
  | _ -> None

let parse_sleek_log content =
  let lines = String.split_on_char '\n' content in
  let is_header l = String.length l >= 4 && String.sub l 0 4 = "id: " in
  let entries = ref [] in
  let cur_header = ref None in
  let cur_body = ref [] in
  let flush () =
    (match !cur_header with
     | Some h ->
       (match parse_entry h (List.rev !cur_body) with
        | Some e -> entries := e :: !entries
        | None -> ())
     | None -> ());
    cur_body := []
  in
  List.iter (fun l ->
      if is_header l then (flush (); cur_header := Some l)
      else cur_body := l :: !cur_body
    ) lines;
  flush ();
  List.rev !entries

(* Keep only real proof obligations (drop prelude invariant checks etc.). *)
let is_obligation_kind = function
  | "PRE" | "POST" | "BIND" | "PRE_REC" | "ASSERT" -> true
  | _ -> false

(* Public, structured per-obligation record (consumed by Hipsleek_store /
   the server panel as well as the textual CLI surface). [cline] is the
   originating C source line (0 if unknown); [line] is the generated .ss line. *)
type obligation =
  { kind : string; line : int; cline : int; entail : string; proved : bool }

(* Map an absolute .ss line within a function (span start [lo]) to the C source
   line, via the per-function (ss_line_relative, c_line) map: take the C line of
   the greatest relative entry <= the obligation's relative line. *)
let cline_of linemap ~lo abs =
  let rel = abs - lo + 1 in
  List.fold_left (fun best (ssrel, cl) ->
      if ssrel <= rel then
        (match best with Some (b, _) when b >= ssrel -> best | _ -> Some (ssrel, cl))
      else best)
    None linemap
  |> function Some (_, cl) -> cl | None -> 0

(* Build per-function structured obligations from the sleek log, keyed by the
   per-function .ss line spans (name, start, end) and C-source line maps. *)
let proof_info_of_spans ~spans ~linemaps content : (string * obligation list) list =
  let entries = parse_sleek_log content in
  List.filter_map (fun (name, lo, hi) ->
      let linemap = match List.assoc_opt name linemaps with Some m -> m | None -> [] in
      let es =
        List.filter (fun e ->
            is_obligation_kind e.e_kind && e.e_line >= lo && e.e_line <= hi)
          entries
      in
      (* HipSleek records each entailment twice; drop exact duplicates. *)
      let es =
        let seen = Hashtbl.create 16 in
        List.filter (fun e ->
            let k = (e.e_kind, e.e_line, e.e_oblig) in
            if Hashtbl.mem seen k then false
            else (Hashtbl.add seen k (); true)) es
      in
      if es = [] then None
      else
        let obls =
          List.map (fun e ->
              { kind = e.e_kind; line = e.e_line;
                cline = cline_of linemap ~lo e.e_line;
                entail = clean_entail e.e_oblig; proved = e.e_proved }) es
        in
        Some (name, obls)
    ) spans

(* Flatten one function's obligations into the textual form used by the
   ip_other property / CLI -report. *)
let text_of_obligations obls =
  let b = Buffer.create 256 in
  Buffer.add_string b
    (Printf.sprintf "HipSleek proof (%d obligation(s)):" (List.length obls));
  List.iter (fun o ->
      let where =
        if o.cline > 0 then Printf.sprintf "line %d" o.cline
        else Printf.sprintf ".ss %d" o.line in
      Buffer.add_string b
        (Printf.sprintf "\n  %s (%s): %s  [%s]"
           o.kind where o.entail (if o.proved then "proved" else "unproved"))
    ) obls;
  Buffer.contents b

let proof_logs_of_info info =
  List.map (fun (name, obls) -> (name, text_of_obligations obls)) info

(* HipSleek desugars each while loop into a method named while_<ssline>_<col>,
   where <ssline> is the loop's line in the generated .ss. Relabel such verdicts
   with the loop's C source line (via the span + linemap that contains that .ss
   line) so the user sees their own code instead of generated-.ss coordinates. *)
let humanize_loop_results ~spans ~linemaps results =
  let locate abs =
    match List.find_opt (fun (_, lo, hi) -> abs >= lo && abs <= hi) spans with
    | None -> None
    | Some (name, lo, _) ->
      let lm = match List.assoc_opt name linemaps with Some m -> m | None -> [] in
      Some (name, cline_of lm ~lo abs)
  in
  List.map (fun r ->
      if not (starts_with r.func_name "while_") then r
      else
        let rest = String.sub r.func_name 6 (String.length r.func_name - 6) in
        match String.split_on_char '_' rest with
        | ssline_str :: _ ->
          (match int_of_string_opt ssline_str with
           | Some abs ->
             (match locate abs with
              | Some (fn, cl) when cl > 0 ->
                { r with func_name =
                    Printf.sprintf "while loop at line %d (in %s)" cl fn }
              | _ -> r)
           | None -> r)
        | [] -> r)
    results

(* ------------------------------------------------------------------ *)
(* Subprocess invocation                                                *)
(* ------------------------------------------------------------------ *)

let run_hip ~dir ~ss_file =
  let hip = resolve_hip_path () in
  if not (Sys.file_exists hip) then begin
    Hipsleek_parameters.error "hip.exe not found (tried: %s)" hip;
    []
  end else begin
    (* Make the hip path absolute so changing CWD to [dir] is safe. *)
    let hip_abs =
      if Filename.is_relative hip then Filename.concat (Sys.getcwd ()) hip
      else hip
    in
    let base = Filename.basename ss_file in
    let proof = Hipsleek_parameters.ProofLog.get () in
    let flags = if proof then "--esl --dump-slk-proof " else "" in
    Hipsleek_parameters.feedback "Invoking: %s %s%s" hip_abs flags ss_file;
    let cmd =
      Printf.sprintf "cd %s && %s %s%s 2>&1"
        (Filename.quote dir) (Filename.quote hip_abs)
        flags (Filename.quote base)
    in
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

(* Read the SLEEK log file HipSleek wrote under [dir]/logs for [ss_file]. *)
let read_sleek_log ~dir ~ss_file =
  let name = "sleek_log_" ^ mangle (Filename.basename ss_file) ^ ".txt" in
  let path = Filename.concat (Filename.concat dir "logs") name in
  if not (Sys.file_exists path) then begin
    Hipsleek_parameters.debug "sleek log not found at %s" path; None
  end else begin
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Some s
  end

(* ------------------------------------------------------------------ *)
(* Entry point: write .ss file, run hip.exe, report                    *)
(* ------------------------------------------------------------------ *)

let run ~ss_content ~ss_spans ~linemaps =
  let dir =
    let d = Hipsleek_parameters.OutputDir.get () in
    if d <> "" then d else Filename.get_temp_dir_name ()
  in
  let ss_file = Filename.concat dir "hipsleek_out.ss" in
  let oc = open_out ss_file in
  output_string oc ss_content;
  close_out oc;
  Hipsleek_parameters.feedback "Generated .ss file: %s" ss_file;
  let results =
    humanize_loop_results ~spans:ss_spans ~linemaps (run_hip ~dir ~ss_file) in
  report results;
  let proof_info =
    if Hipsleek_parameters.ProofLog.get () then
      match read_sleek_log ~dir ~ss_file with
      | Some content -> proof_info_of_spans ~spans:ss_spans ~linemaps content
      | None -> []
    else []
  in
  let proof_logs = proof_logs_of_info proof_info in
  (results, proof_logs, proof_info, ss_file)
