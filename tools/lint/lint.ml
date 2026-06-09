(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type tool_cmds =
  { kind: (string [@default "Misc"]) ;
    extensions: (string list [@default []]);
    name: string ;
    available_cmd: (string [@default ""]) ; (* leave it empty to set it as unavailable *)
    check_cmd: (string [@default ""]) ; (* leave it empty if there is no check command *)
    update_cmd: (string [@default ""]) ; (* leave it empty if there is no updating command *)
    version_cmd: (string [@default ""]) (* leave it empty if there is no version check command *)
  }
[@@deriving yojson]

(**************************************************************************)
(** The only part to modify for adding a new external formatters *)

(** Supported indent formatters *)
(* if you add formatters or change targeted versions, be sure
   to keep the CI configuration in sync in nix/pkgs.nix.
*)
let external_formatters = [
  { kind = "C";
    extensions = [ ".c" ; ".i"; ".h"; ".cpp"; ".hpp"; ".ii"; ".cc" ];
    name = "clang-format";
    available_cmd = "clang-format --version > /dev/null 2> /dev/null";
    check_cmd = "clang-format --dry-run -Werror" ;
    update_cmd = "clang-format -i" ;
    version_cmd = "clang-format --version | grep -E '(19|20)\\.[0-9]+\\.[0-9]+'"
  }
  ;
  { kind = "Python";
    extensions = [ ".py" ];
    name = "black";
    available_cmd = "black --version > /dev/null 2> /dev/null";
    check_cmd = "black --quiet --line-length 100 --check" ;
    update_cmd = "black --quiet --line-length 100" ;
    version_cmd  = "black --version | grep black | grep -E '25\\.[0-9]+\\.[0-9]+' > /dev/null 2> /dev/null"
  }
]

(**************************************************************************)
(* Warning/Error *)

let strict = ref false

let res = ref true (* impact the exit value *)

let warn ftext =
  if !strict then
    res := false ;
  Format.eprintf "Warning: ";
  Format.eprintf ftext

let error ftext =
  res := false ;
  Format.eprintf ftext

(**************************************************************************)
(* Utils *)

let read_buffered channel =
  let buffer = Buffer.create 0 in
  let size = 4096 in
  let load = Bytes.create size in
  let read = ref @@ input channel load 0 size in
  while !read <> 0 do
    Buffer.add_subbytes buffer load 0 !read ;
    read := input channel load 0 size
  done ;
  Buffer.to_bytes buffer

let rec lines_from_buffer acc buffer start =
  if start = Bytes.length buffer then acc
  else
    let line_end = Bytes.index_from buffer start '\000' in
    let len = line_end - start in
    let line = Bytes.sub_string buffer start len in
    lines_from_buffer (line :: acc) buffer (line_end + 1)

let lines_from_in channel =
  let content = read_buffered channel in
  let acc = lines_from_buffer [] content 0 in
  List.rev acc

(**************************************************************************)
type available_tools =
  { mutable is_available: bool option ;
    tool_cmds: tool_cmds
  }

type indent_formatter = Ocp_indent | Tool of available_tools

(* from formatter name *)
let external_tbl = Hashtbl.create 13

(* from file extension *)
let default_tbl = Hashtbl.create 13

let updates_tbl external_tools =
  List.iter (fun formatter ->
      let tool = Tool { is_available = None; tool_cmds = formatter } in
      List.iter (fun extension ->
          Hashtbl.replace default_tbl extension tool)
        formatter.extensions;
      Hashtbl.add external_tbl formatter.name tool)
    external_tools

let () = updates_tbl external_formatters

type tools = tool_cmds list
[@@deriving yojson]

let parse_config config_file =
  if config_file <> "" then
    let config_tools =
      try
        tools_of_yojson (Yojson.Safe.from_file config_file)
      with Yojson.Json_error txt ->
        Error txt
    in match config_tools with
    | Result.Ok external_tools -> updates_tbl external_tools
    | Result.Error txt ->
      warn "Parse error:%s:%s@." config_file txt

(************************)

let ml_indent_formatter = Ocp_indent

type indent_check = NoCheck | Check of indent_formatter option

let parse_indent_formatter ~file ~attr ~value = match value with
  | "unset" -> NoCheck
  | "set"   -> Check None (* use the default formatter *)
  | _ ->
    match Hashtbl.find_opt external_tbl value with
    | None ->
      if value = "ocp-indent" then
        (* "ocp-indent" is not overloaded: using the built-in configuration *)
        Check (Some ml_indent_formatter)
      else (warn "Unsupported indent formatter: %s %s=%s@."
              file attr value;
            NoCheck)
    | res -> Check res

(**************************************************************************)
(* Available Checks and corresponding attributes *)

type checks =
  { eoleof : bool
  ; indent : indent_check
  ; syntax : bool
  ; utf8 : bool
  }

let no_checks =
  { eoleof = false
  ; indent = NoCheck
  ; syntax = false
  ; utf8 = false
  }

let add_attr ~file ~attr ~value checks =
  let is_set = function
    | "set" -> true
    | "unset" -> false
    | _ -> warn "Invalid attribute value: %s %s=%s@." file attr value ; false
  in
  match attr with
  | "check-eoleof" -> { checks with eoleof = is_set value }
  | "check-syntax" -> { checks with syntax = is_set value }
  | "check-utf8"   -> { checks with utf8 = is_set value }
  | "check-indent" -> { checks with
                        indent = parse_indent_formatter ~file ~attr ~value }
  | _ -> warn "Unknown attribute: %s %s=%s@." file attr value;
    checks

let handled_attr s =
  s = "check-eoleof" || s = "check-indent" ||
  s = "check-syntax" || s = "check-utf8"

let ignored_attr s =
  not (handled_attr s)

(**************************************************************************)
(* Table of the files to control *)

let table = Hashtbl.create 1031

let get file =
  try Hashtbl.find table file
  with Not_found -> no_checks

let rec collect = function
  | _file :: attr :: _value :: tl when ignored_attr attr ->
    collect tl
  | file :: attr :: value :: tl ->
    let checks = get file in
    Hashtbl.replace table file (add_attr ~file ~attr ~value checks) ;
    collect tl
  | [] -> ()
  | [ file ; attr ] -> warn "Missing attribute value: %s %s=?@." file attr
  | [ file ] -> warn "Missing attribute name for file: %s@." file

(**************************************************************************)
(* Functions used to check lint *)

(* Syntax *)

let check_syntax ~update content =
  let size = Bytes.length content in
  let out = Buffer.create 0 in
  let exception Bad_syntax in
  try
    let i = ref 0 in
    let blank = ref (-1) in
    while !i < size do
      let byte = Bytes.get content !i in
      if byte = '\t' then begin
        if not update then raise Bad_syntax ;
        if !blank = -1 then blank := Buffer.length out ;
        Buffer.add_string out "  "
      end
      else if byte = ' ' then begin
        if !blank = -1 then blank := Buffer.length out ;
        Buffer.add_char out ' '
      end
      else if byte = '\n' && !blank <> -1 then begin
        if not update then raise Bad_syntax ;
        Buffer.truncate out !blank ;
        Buffer.add_char out '\n' ;
        blank := -1
      end
      else begin
        Buffer.add_char out byte ;
        blank := -1
      end ;
      incr i
    done ;
    if !blank <> -1 then
      Buffer.truncate out !blank ;
    let out = Buffer.to_bytes out in
    if not @@ Bytes.equal out content
    then out, false
    else content, true
  with Bad_syntax ->
    content, false

(* EOL/EOF *)

let check_eoleof ~update content =
  let length = Bytes.length content in
  if length = 0 then content, true
  else if '\n' = Bytes.get content (length - 1) then content, true
  else if update then begin
    let new_content = Bytes.extend content 0 1 in
    Bytes.set new_content length '\n' ;
    new_content, false
  end else
    content,false

(* Indentation *)

(* ML(I) *)

(* Basically this is OCP-Indent main where all elements related to options have
   been removed and the printer changed so that it prints into a buffer and not
   a file.
*)

let global_config = ref None
let config () =
  match !global_config with
  | None ->
    let config, syntaxes, dlink = IndentConfig.local_default () in
    IndentLoader.load ~debug:false dlink ;
    Approx_lexer.disable_extensions ();
    List.iter
      (fun stx ->
         try Approx_lexer.enable_extension stx
         with IndentExtend.Syntax_not_found name ->
           warn "Unknown syntax extension %S@." name)
      syntaxes ;
    global_config := Some config ;
    config
  | Some config -> config

let ocp_indent channel =
  let config = config () in
  let buffer = Buffer.create 0 in
  let out = IndentPrinter.{
      debug = false; config = config; indent_empty = false; adaptive = true;
      in_lines = (fun _ -> true);
      kind = Print (fun s b -> Buffer.add_string b s ; b);
    }
  in
  let stream = Nstream.of_channel channel in
  Buffer.to_bytes (IndentPrinter.proceed out stream IndentBlock.empty buffer)

let check_ml_indent ~update file =
  let input = open_in file in
  let original = read_buffered input in
  seek_in input 0 ;
  let modified = ocp_indent input in
  close_in input ;
  let result = Bytes.equal original modified in
  if update && not result then
    let output = open_out file in
    output_bytes output modified ;
    close_out output ;
    true
  else
    result

(* C/H *)

(* returns true if the string command is empty *)
let cmd_result ?file cmd =
  let command = match file with
    | None -> cmd
    | Some file -> Format.sprintf "%s \"%s\"" cmd file
  in
  (cmd = "") || (0 = Sys.command command)

let is_formatter_available ~file indent_formatter =
  match indent_formatter.is_available with
  | None ->
    let is_enabled =
      (indent_formatter.tool_cmds.update_cmd <> "") ||
      (indent_formatter.tool_cmds.check_cmd <> "")
    in
    let is_available =
      is_enabled
      && (cmd_result indent_formatter.tool_cmds.available_cmd)
      && (cmd_result indent_formatter.tool_cmds.version_cmd)
    in
    indent_formatter.is_available <- Some is_available;
    if not is_enabled then
      (* [check_cmd] and [update_cmd] fields are empty *)
      warn
        "%s is disabled for checking/updating indentation of \
         some %s files (i.e. %s)@."
        indent_formatter.tool_cmds.name indent_formatter.tool_cmds.kind file
    else if not is_available then
      warn
        "%s is unavailable (or with incompatible version) for checking/updating \
         indentation of some %s files (i.e. %s)@."
        indent_formatter.tool_cmds.name indent_formatter.tool_cmds.kind file;
    is_available
  | Some is_available -> is_available

exception Bad_ext

let check_indent ~indent_formatter ~update file =
  let tool = match indent_formatter with
    | Some tool -> tool
    | None -> (* uses the default formatter *)
      let extension = Filename.extension file in
      match Hashtbl.find_opt default_tbl extension with
      | Some tool -> tool
      | None -> match extension with
        | ".ml" | ".mli" -> ml_indent_formatter
        | _ -> raise Bad_ext
  in match tool with
  | Ocp_indent -> check_ml_indent ~update file
  | Tool indent_formatter ->
    if not @@ is_formatter_available ~file indent_formatter then true
    else if not update then
      cmd_result ~file indent_formatter.tool_cmds.check_cmd
    else
      cmd_result ~file indent_formatter.tool_cmds.update_cmd

(* Main checks *)

let check ~count ~verbose ~update file params =
  if verbose then
    Format.printf "Checking %s@." file ;
  if Sys .is_directory file then ()
  else begin
    incr count;
    let in_chan = open_in file in
    let content = read_buffered in_chan in
    close_in in_chan ;
    (* UTF8 *)
    if params.utf8 then
      Option.iter (fun (line,p,i) -> error "Bad encoding (not UTF8) for %s:%i:%i@." file line (i-p))
        (UTF8.validate (Bytes.to_string content))  ;
    (* Blanks *)
    let rewrite = ref false in
    let syntactic_check checker content message  =
      let new_content, was_ok = checker ~update content in
      if update && not was_ok
      then begin rewrite := true ; new_content end
      else if not was_ok then begin
        error "%s for %s@." message file ;
        new_content
      end
      else new_content
    in
    let content =
      if params.syntax
      then syntactic_check check_syntax content "Bad syntax"
      else content
    in
    let content =
      if params.eoleof || params.syntax
      then syntactic_check check_eoleof content "Bad EOF"
      else content
    in
    if !rewrite then begin
      let out_chan = open_out file in
      output_bytes out_chan content ;
      close_out out_chan
    end ;
    (* Indentation *)
    try
      begin
        match params.indent with
        | NoCheck -> ()
        | Check indent_formatter ->
          if not @@ check_indent ~indent_formatter ~update file then
            error "Bad indentation for %s@." file ;
      end ;
    with Bad_ext ->
      error "Don't know how to (check) indent %s@." file
  end

(**************************************************************************)
(* Options *)

let exec_name = Sys.argv.(0)

let update = ref false
let verbose = ref false
let config_file = ref ""
let extract_config = ref false

let argspec = [
  "-u", Arg.Set update, " Update ill-formed files (does not handle UTF8 update)" ;
  "-v", Arg.Set verbose, " Verbose mode" ;
  "-s", Arg.Set strict, " Considers warnings as errors for the exit value" ;
  "-c", Arg.String (fun s -> config_file := s), "<json-config-file> Reads the JSON configuration file (allows to overload the default configuration)" ;
  "-e", Arg.Set extract_config, " Prints default JSON configuration" ;
]
let sort argspec =
  List.sort (fun (name1, _, _) (name2, _, _) -> String.compare name1 name2)
    argspec

(**************************************************************************)
(* Main *)

let () =
  Arg.parse
    (Arg.align (sort argspec))
    (fun s -> warn "Unknown argument: %s@." s)
    ("Usage: git ls-files -z | git check-attr --stdin -z -a | " ^ exec_name ^ " [options]\n"
     ^"\nChecks or updates files in relation to lint constraints specified by these git attributes:\n"
     ^"  check-eoleof, check-syntax, check-utf8 and check-indent.\n"
     ^"\nOptions:");
  if !extract_config then
    Format.printf "Default JSON configuration:@.%a@."
      (Yojson.Safe.pretty_print ~std:false) (tools_to_yojson external_formatters)
  else begin
    updates_tbl external_formatters ;
    parse_config !config_file;
    collect @@ lines_from_in stdin ;
    let count = ref 0 in
    Hashtbl.iter (check ~count ~verbose:!verbose ~update:!update) table ;
    Format.printf "Lint %d file(s)@." !count;
    if not !res then exit 1
  end
