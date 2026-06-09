(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Command-line flags *)
let verbosity = ref 1

let cmp_cmd = "diff --new-file -q"
let diff_cmd = "diff --new-file"

(* ------------------------------- *)

module Filename = struct
  include Filename
  let concat =
    if Sys.os_type = "Win32" then
      fun a b -> a ^ "/" ^ b
    else
      concat
end

(* ------------------------------- *)

let pp_json fmt json =
  Format.fprintf fmt "%% dune build @@%S" json

let fail ~json s info =
  Format.printf "%a: Error - %s@.%% Aborting: %s@." pp_json json s info ;
  exit 2

let output_unix_error (exn : exn) =
  match exn with
  | Unix.Unix_error (error, _function, arg) ->
    let message = Unix.error_message error in
    if arg = "" then
      Format.eprintf "%s@." message
    else
      Format.eprintf "%s: %s@." arg message
  | _ -> assert false

let unlink ?(silent = true) file =
  let open Unix in
  try
    Unix.unlink file
  with
  | Unix_error _ when silent -> ()
  | Unix_error (ENOENT,_,_) -> () (* Ignore "No such file or directory" *)
  | Unix_error _ as e -> output_unix_error e

let system =
  if Sys.os_type = "Win32" then
    fun f ->
      Unix.system (Format.sprintf "bash -c %S" f)
  else
    fun f ->
      Unix.system f

let print_file dir_info file =
  Format.printf "%% Generated output file: %s/%s@." dir_info file;
  try
    let cin = open_in file in
    try
      while true do
        let line = input_line cin in
        Format.printf "%s\n" line
      done
    with _ ->
      close_in cin
  with _ ->
    Format.printf "%% Cannot open file: %s@." file

(* ------------------------------- *)

let example_msg =
  Format.sprintf
    "@.@[<v 0>\
     Wrapper to run test command.@."

let umsg = "Usage: frama-c-wtests [options] <json-config> <test-command>*"

let rec argspec =
  [
    ("-v", Arg.Unit (fun () -> incr verbosity),
     "Increase verbosity (up to twice)") ;
    ("-brief", Arg.Unit (fun () -> verbosity := 0),
     "Brief report only on test failure") ;
  ]
and help_msg () = Arg.usage (Arg.align argspec) umsg

let parse_args () =
  let suites = ref [] in
  let add_test_suite s = suites := s :: !suites in
  Arg.parse
    ((Arg.align
        (List.sort
           (fun (optname1, _, _) (optname2, _, _) ->
              compare optname1 optname2
           ) argspec)
     ) @ ["", Arg.Unit (fun () -> ()), example_msg;])
    add_test_suite
    umsg;
  List.rev !suites

(* ------------------------------- *)

let launch command_string =
  let result = system command_string in
  match result with
  | Unix.WEXITED 127 ->
    Format.printf "%% Couldn't execute command.:@\n%s@\nStopping@."
      command_string ;
    exit 1
  | Unix.WEXITED r -> r
  | Unix.WSIGNALED s ->
    Format.printf
      "%% SIGNAL %d received while executing command:@\n%s@\nStopping@."
      s command_string ;
    exit 1
  | Unix.WSTOPPED s ->
    Format.printf
      "%% STOP %d received while executing command:@\n%s@\nStopping@."
      s command_string;
    exit 1

let rm_filter_result (_cmd,stdfile) = unlink stdfile

let filter (cmd,_stdfile) =
  if !verbosity > 0 then Format.printf "%% Run filter command: %s@." cmd;
  let ret_code = launch cmd in
  if !verbosity > 0 && ret_code <> 0 then Format.printf "%% note: the filter command returned an error code (%d)@." ret_code

let compare_files ~json ~error ~result oracle =
  let not_generated = not (Sys.file_exists result) in
  if not_generated then
    Format.printf "%a: missing target %S@." pp_json json result;
  let cmd = Format.sprintf "%s %S %S" (if !verbosity > 0  then diff_cmd else cmp_cmd) oracle result in
  if !verbosity > 0  then Format.printf "%% Run compare command: %s@." cmd;
  let ret_code = launch cmd in
  let is_ko = ret_code <> 0 in
  if is_ko then Format.printf "%a: diff failure on diff:@. (cd _build/default/%s && %s %S %S)@."
      pp_json json (Filename.dirname json) diff_cmd result oracle ;
  error || is_ko

let compare_std ~json error = function
  | "" -> fun _ -> error
  | result -> compare_files ~json ~error ~result

(* [oracle_dir] should be set to "." to look at the current directory *)
let compare_log ~json oracle_dir error result =
  if String.equal oracle_dir "" then
    (Format.printf "%a: missing oracle_dir for %S@." pp_json json result;
     false)
  else
    compare_files ~json ~error ~result (Filename.concat oracle_dir result)

let remove file = if file <> "" then unlink file

let extract filters targets = function
  | "" -> fun _ _ -> filters,targets
  | stdfile -> function
    | ""  -> fun _   -> filters,(stdfile::targets)
    | tmp -> fun cmd -> ((cmd,stdfile)::filters),(tmp::stdfile::targets)

type wtest = {
  info: (string [@default ""]); (* info *)
  dir: (string [@default ""]); (* test directory *)
  cmd: (string [@default "echo unknown command"]);
  ret_code: (int [@default 0]);
  out: (string [@default "" (* bin target built by the command *) ]); (* sdtout target *)
  err: (string [@default "" (* bin target built by the command *) ]); (* stderr target *)
  tmpout: (string [@default ""]); (* temporary file to filter stdout result *)
  tmperr: (string [@default ""]); (* temporary file to filter stderr result *)
  sedout: (string [@default ""]); (* filter command for the stdout result *)
  sederr: (string [@default ""]); (* filter command for the stderr result *)
  bin: (string list [@default []]); (* binary targets (without oracles) *)
  log: (string list [@default []]); (* log targets (compared to log oracles *)
  oracle_dir: (string [@default ""]); (* directory containing the oracle of the log files *)
  oracle_out: (string [@default "" ]); (* oracle of the stdout target *)
  oracle_err: (string [@default "" ]); (* oracle of the stderr target *)
}
[@@deriving of_yojson]

let wrapper json test =
  let sed,logs = extract [] test.log test.out test.tmpout test.sedout in
  let sed,logs = extract sed    logs test.err test.tmperr test.sederr in
  if logs <> [] || test.bin <> [] then begin
    if !verbosity > 0 then Format.printf "%% Clean targets...@.";
    List.iter remove logs;
    List.iter remove test.bin
  end;
  if !verbosity > 0 then Format.printf "%% Run test command: %s@." test.cmd;
  let ret_code = launch test.cmd in
  let error = ret_code <> test.ret_code in
  if error || !verbosity > 0 then begin
    if test.out <> "" then print_file test.dir (if test.tmpout = "" then test.out else test.tmpout) ;
    if test.err <> "" then print_file test.dir (if test.tmperr = "" then test.err else test.tmperr) ;
    List.iter (print_file test.dir) test.log
  end;
  if error then begin
    Format.printf "%a: return code (%d) differs from the requested code (%d) for the command:%s@."
      pp_json json ret_code test.ret_code test.cmd;
    true
  end else begin
    List.iter filter sed ;
    let is_cmp_ko = compare_std ~json false     test.out test.oracle_out in
    let is_cmp_ko = compare_std ~json is_cmp_ko test.err test.oracle_err in
    let is_cmp_ko  = List.fold_left (compare_log ~json test.oracle_dir) is_cmp_ko test.log in
    if !verbosity = 0 then
      (* In `-brief` mode  (used by the `dune` file generated by `frama-c-ptests`),
           the filtered result file are removed
           in order to let `dune` applying the filters. *)
      List.iter rm_filter_result sed ;
    (* In `-brief` mode,
         the comparison failures are not reported with an error code
         in order to let `dune` reporting them with the textual differences.  *)
    (if !verbosity > 1 then is_cmp_ko else false)
  end

let parse ~json =
  if !verbosity > 0 then Format.printf "%% Parsing Jsonjson...@.";
  match wtest_of_yojson (Yojson.Safe.from_file json) with
  | Error txt ->  fail ~json txt "Json file cannot be parsed"
  | Ok r -> r

let wrapper ~json =
  try
    let test = parse ~json in
    let json = test.dir ^ "/" ^ json in
    if !verbosity > 0 then
      Format.printf "%% Wrapping info: %s@." test.info ;
    match test with
    | { info; out=""; tmpout=tmp; sedout=sed; _} when tmp <> "" || sed <> "" -> fail ~json "StdOut filter cannot be applied" info
    | { info; err=""; tmperr=tmp; sederr=sed; _} when tmp <> "" || sed <> "" -> fail ~json "StdErr filter cannot be applied" info
    | { info; out=std; oracle_out=oracle; _} when (std <> "") <> (oracle <> "") -> fail ~json "StdOut file cannot be compared to an oracle" info
    | { info; err=std; oracle_err=oracle; _} when (std <> "") <> (oracle <> "")  -> fail ~json "StdErr file cannot be compared to an oracle" info
    | { info; tmpout=tmp; sedout=sed; _} when (tmp <> "") <> (sed <> "") -> fail ~json "StdOut filter cannot be applied" info
    | { info; tmperr=tmp; sederr=sed; _} when (tmp <> "") <> (sed <> "") -> fail ~json "StdErr filter cannot be applied" info
    | _ ->
      if wrapper json test then
        fail ~json "Test failed" test.info
  with
  | Yojson.Json_error txt
  | Sys_error txt -> fail ~json txt "Json file cannot be parsed"


let () =
  let args = parse_args () in
  (* verbosity := 1; *)
  match args with
  | json::commands ->
    if !verbosity > 0 then begin
      Format.printf "%% Wrapping from json file: %S@." json ;
      match commands with
      | cmd::filters ->
        Format.printf "%% Wrapped command: %s@." cmd ;
        List.iter (fun s -> Format.printf "%% Wrapped filter: %s@." s) filters;
      | _ -> ()
    end;
    wrapper ~json
  | _ -> help_msg () ; exit 1
