(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type spec_format = Sep1Line1 (* <space>* FileName <space>* `:` <space>* HeaderId <space>* <eol> *)
                 | Sep2Line1 (* <space>* FileName <space>* `:` <space>* AttributeName <space>*`:` <space>*HeaderId <space>* <eol> *)
                 | Line3     (* FileName <eol> AttributeName <eol> HeaderId <eol> *)
                 | Zero3     (* FileName <null> AttributeName <null> HeaderId <null> *)
(* Sep1Line1
   > cat headers/header_spec.txt | headers/hdrck --stdin -spec-format=2-fields-by-line -header-dirs headers/open-source
*)
(* Sep2Line1
   > cat headers/header_spec.txt | tr ':' '\n' | xargs -n 2 printf " %s : header_spec : %s \n" > x-3-fields-by-line.txt
   > cat x-3-fields-by-line.txt | headers/hdrck --stdin -spec-format=3-fields-by-line -header-dirs headers/open-source
*)
(* Line3
   > cat headers/header_spec.txt | tr ':' '\n' | xargs -n 2 printf "%s\nheader_spec\n%s\n" > x-3-lines.txt
   > cat x-3-lines.txt  | headers/hdrck --stdin -spec-format=3-lines -header-dirs headers/open-source
*)
(* Zero
   > cat headers/header_spec.txt | tr ':' '\n' | xargs -n 2 printf "%s\nheader_spec\n%s\n" | tr '\n' '\0' > x-3-zeros.txt
   > cat x-3-zeros.txt  | headers/hdrck --stdin -z -header-dirs headers/open-source
*)

(* From the git archive
   > git ls-files -z | git check-attr --stdin -z header_spec \
     | headers/hdrck --stdin -z -header-dirs headers/open-source -header-dirs src/plugins/e-acsl/headers/open-source
*)

(* Parameters settable from command line and/or environment variables *)
let debug_level =
  let envvar_level =
    match Sys.getenv_opt "HDRCK_DEBUG" with
    | None -> 0
    | Some v ->
      begin
        match Stdlib.int_of_string_opt v with
        | None -> 1
        | Some i -> i
      end
  in
  ref envvar_level
and spec_files = ref []
and from_stdin = ref false
and zero_stdin = ref false
and spec_format = ref Sep1Line1
and header_dirs = ref []
and forbidden_headers = ref []
and root_dir = ref (Sys.getcwd ())
and distrib_file = ref None
and header_except_file = ref None
and headache_config_file = ref [] (* empty -> headache_config_file_default *)
and headache_config_file_default = "headers/headache_config.txt"
and exit_on_warning = ref false
and exit_on_error = ref true (* only settable to false for debugging purposes *)
and quiet = ref true

type mode =
  | Check
  | Update

let mode = ref Check

(** Temporary directory management **)

let tmp_dirname = ref None
let remove_tmp_dirname () = match !tmp_dirname with
  | None -> ()
  | Some dirname -> if !debug_level <= 0 then Unix.rmdir dirname

(** Utilities for message printing **)

let is_first_job_line = ref false

let job_head fmt =
  is_first_job_line:=true;
  Format.printf fmt

let job_done () =
  if not !quiet then
    Format.printf "done@."

let pp_job_first_line () =
  if !is_first_job_line then
    begin
      is_first_job_line := false;
      Format.printf "@."
    end

let debug ?(level=1) fmt =
  if !debug_level >= level then begin
    pp_job_first_line ();
    Format.printf "- [debug] ";
    Format.printf fmt
  end
  else Format.ifprintf Format.std_formatter  fmt

let has_no_warning_nor_error = ref true

let info fmt =
  pp_job_first_line ();
  Format.printf "- [info] ";
  Format.printf fmt

let warn fmt =
  pp_job_first_line ();
  if !exit_on_warning then
    has_no_warning_nor_error := false ;
  Format.printf "- [warning] ";
  Format.printf fmt

let error_fmt fmt =
  pp_job_first_line ();
  has_no_warning_nor_error := false ;
  Format.printf "- [error] ";
  Format.printf fmt

let error ~exit_value =
  let exit_fmt ~exit_value =
    pp_job_first_line ();
    has_no_warning_nor_error := false ;
    Format.printf "- [fatal] ";
    Format.kfprintf
      (fun fmt ->
         Format.pp_print_flush fmt () ;
         remove_tmp_dirname () ;
         exit exit_value)
      Format.std_formatter
  in
  if !exit_on_error then exit_fmt ~exit_value else error_fmt

(* We deliberately do _not_ use Filename.concat, since it has issues on Cygwin;
   due to the possibility of mixing directory separators (e.g., '\' coming from
   Windows-style paths, and '/' from Unix-style paths, such as the ones written
   in the header_spec.txt files.
*)
let path_concat p1 p2 =
  if String.ends_with ~suffix:"/" p1 then p1 ^ p2 else p1 ^ "/" ^ p2

(* Temporary directory management (cont.) *)
let get_tmp_dirname () = match !tmp_dirname with
  | None ->
    let dirname = path_concat (Filename.get_temp_dir_name ()) ".hdck" in
    debug "Using temporary directory: %s@." dirname;
    if not (Sys.file_exists dirname) then Unix.mkdir dirname 0o740;
    tmp_dirname := Some dirname;
    dirname
  | Some dirname -> dirname

let get_string_null (ic:in_channel) =
  let rec aux acc =
    let c = input_char ic in
    if c <> '\000' then aux (c :: acc) else acc
  in
  let tab = Array.of_list (List.rev (aux [])) in
  String.init (Array.length tab) (Array.get tab)


(* Reads [nlines] lines of a file named [filename].
 *
 * Defaults to reading the file entirely since any integer will ever be greater
 * or equal than [max_int].
*)
let read_lines ?nlines:(nlines=max_int) get_line filename =
  let lines = ref [] in
  let ic = if filename = "--stdin" then stdin else open_in filename in
  let n = ref 1 in
  try
    while !n <= nlines do
      lines := get_line ic :: !lines;
      incr n
    done;
    close_in ic;
    List.rev !lines
  with
  | End_of_file ->
    close_in ic;
    List.rev !lines

let sub_dir_reg_exp = Str.regexp "/./"
let extract_sub_dir filename =
  match Str.split sub_dir_reg_exp filename with
  | sub_dir :: _ :: _ -> sub_dir
  | _ -> ""

let split_line_entry =
  let colon_reg_exp = Str.regexp ":" in
  fun (line:string) ->
    List.map String.trim (Str.split colon_reg_exp line)

module StringSet = struct
  include Set.Make(struct type t = string let compare = String.compare end)

  let pp fmt set =
    Format.fprintf fmt "@[<v 0>";
    iter (fun name -> Format.fprintf fmt "- %s@ " name) set;
    Format.fprintf fmt "@]"
end

(* Checks that the file name is a new entry or else has the same license name.
   Given the license name, updates the hashtable or else
   returns the new set of ignored files
   @param spec_tab ([filename] -> [license_name]) hashtable to update
   @param ignored_files set of ignored files to update.
   @param filename
   @param license_name
*)
let add_spec_entry (ignored_files: StringSet.t ref) (spec_tab: (string, string) Hashtbl.t)
    idx ~(file_name : string) ~(license_name: string) =
  debug ~level:2 "add_spec_entry: file_name: %s, license_name: %s@." file_name license_name;
  match license_name with
  | ("set" | "unset" | "unspecified") ->
    warn (* error ~exit_value:9 *)
      "%s: invalid specification (%d) for that file (git attribute value=%s)@."
      file_name idx license_name
  | ".ignore" -> begin
      try
        let previous_entry = Hashtbl.find spec_tab file_name in
        error ~exit_value:6
          "%s: specification duplicated (%d) with a different license name (%s and %s)@."
          file_name idx previous_entry ".ignore"
      with Not_found ->
        if StringSet.mem file_name !ignored_files then
          warn "%s: specification duplicated (%d)@." file_name idx
        else ignored_files := StringSet.add file_name !ignored_files
    end
  | _ -> begin
      try
        let previous_entry = Hashtbl.find spec_tab file_name in
        if license_name <> previous_entry then
          error ~exit_value:6
            "%s: specification duplicated (%d) with a different license name (%s and %s)@."
            file_name idx license_name previous_entry
        else if StringSet.mem file_name !ignored_files then
          error ~exit_value:6
            "%s: specification duplicated (%d) with a different license name (%s and %s)@."
            file_name idx license_name ".ignore"
        else warn "%s: specification duplicated (%d)@." file_name idx
      with Not_found ->
        if StringSet.mem file_name !ignored_files then
          error ~exit_value:6
            "%s: specification duplicated (%d) with a different license name (%s and %s)@."
            file_name idx license_name ".ignore"
        else Hashtbl.add spec_tab file_name license_name
    end

(* Reads the contents of the specification.
   Each line of the file using the [spec_format].
   Lines that do not match this pattern are ignored.

   @param spec_tab (file -> license header name) hashtable to update
   @param ignored_files set of ignored files to update.
*)
let read_specs spec_format (ignored_files: StringSet.t ref) (spec_tab: (string, string) Hashtbl.t) (spec_file : string option) =
  let spec_fname = match spec_file with None -> "--stdin" | Some filename -> filename in
  debug "Specification file: %s@." spec_fname ;
  if not !quiet then
    job_head "Checking format of specification file %s... @?" spec_fname;
  let sub_dir = extract_sub_dir spec_fname in
  let add_spec, get_line =
    let add_spec_item i ~file_name ~license_name =
      let file_name =
        if sub_dir <> "" then path_concat sub_dir file_name else file_name
      in
      let file_name = path_concat !root_dir file_name in
      add_spec_entry ignored_files spec_tab i ~file_name ~license_name
    in
    let add_spec_Sep1Line1 spec_lines =
      List.iteri
        (fun i spec_line ->
           match split_line_entry spec_line with
           | file_name :: [license_name] ->
             add_spec_item i ~file_name ~license_name
           | _ -> warn "%s (%d): bad line format@." spec_fname (i+1)
        ) spec_lines
    and add_spec_Sep2Line1 spec_lines =
      List.iteri
        (fun i spec_line ->
           match split_line_entry spec_line with
           | file_name :: "header_spec" :: [license_name] ->
             add_spec_item i ~file_name ~license_name
           | _ :: attr :: [_] -> warn "%s (%d): bad attribute name: %s@." spec_fname (i+1) attr
           | _ ->                warn "%s (%d): bad line format@." spec_fname (i+1)
        ) spec_lines
    and add_spec_Sep0Line3 spec_lines =
      let rec add_spec i = function
        | [] -> ()
        | file_name :: "header_spec" :: license_name :: spec_lines ->
          add_spec_item i ~file_name ~license_name ;
          add_spec (i+1) spec_lines
        | _ :: attr :: _ :: _ ->
          warn "%s (%d): (3-upplet: %d) attribute name: %s@." spec_fname ((3*i)+1) (i+1) attr
        | _ -> warn "%s (%d): (3-upplet: %d) bad format@." spec_fname ((3*i)+1) (i+1)
      in add_spec 0 spec_lines
    in match spec_format with
    | Sep1Line1 -> add_spec_Sep1Line1,input_line
    | Sep2Line1 -> add_spec_Sep2Line1,input_line
    | Line3     -> add_spec_Sep0Line3,input_line
    | Zero3     -> add_spec_Sep0Line3, get_string_null
  in
  let spec_lines = read_lines get_line spec_fname in
  add_spec spec_lines;
  job_done ()

let coma_reg_exp = Str.regexp ","
let set_cumulative ~(name:string) (value: string list ref) ~(set : string) =
  debug "Register cumulative %s option: %s@." name set;
  value := List.fold_left
      (fun acc v -> let v = String.trim v in if v="" then acc else v::acc)
      !value (Str.split coma_reg_exp set);
  debug "Registered value: %a@." (Format.pp_print_list Format.pp_print_string) !value

let get_header_dirs =
  let first_time = ref true in
  (fun () ->
     if !first_time then begin
       first_time := false ;
       header_dirs := if !header_dirs = [] then [ Sys.getcwd () ] else List.rev !header_dirs
     end;
     debug "Reordered header directories: %a@." (Format.pp_print_list Format.pp_print_string) !header_dirs;
     !header_dirs)

let get_forbidden_headers () =
  List.fold_left (fun acc v -> StringSet.add v acc) StringSet.empty !forbidden_headers

(* Reads all directories defined in variable [header_dirs].
   @assumes each file in said directories is a valid header definition.
   @assumes filenames in directories are license names
   @return a filename -> filepath hashtable
*)
let get_header_files ?directories:(dirs=(get_header_dirs ())) () :
  (string, string) Hashtbl.t =
  let license_path_tbl = Hashtbl.create 23 in
  List.iter
    (fun dir ->
       if not !quiet then
         job_head "Reading license header definition files from directory %s... @?" dir;
       if Sys.file_exists dir && Sys.is_directory dir then begin
         Array.iter
           (fun filename ->
              let license_name = filename in
              let filepath = path_concat dir filename in
              (try (* Checks that the license name is a new entry
                      or else that their related files have the same content. *)
                 let previous_entry = Hashtbl.find license_path_tbl license_name in
                 let cmd = Format.sprintf "diff -q %s %s > /dev/null" filepath previous_entry in
                 let ret = Sys.command cmd in (* files must still be present *)
                 if ret <> 0 then
                   if ret = 255 then
                     (* Ctrl+C pressed; abort execution *)
                     exit 255
                   else
                     error ~exit_value:7
                       "%s: duplicated license name (contents differs to file: %s)@." filepath previous_entry
                 else
                   info "%s: duplicated license name (same contents as file: %s)@." filepath previous_entry
               with Not_found -> ());
              Hashtbl.add license_path_tbl license_name filepath;
           )
           (Sys.readdir dir)
       end
       else warn "Ignoring absent directory %s@." dir;
       job_done ();
    ) dirs;
  license_path_tbl

(* Checks that all license headers specified in a given specification have a
 * definition in a file of the file system.

   @requires ignored files have been filtered out the specifications
*)
let check_declared_headers specification headers =
  if not !quiet then
    job_head "Checking license specifications are defined... @?" ;
  Hashtbl.iter
    (fun file header_type ->
       if not (Hashtbl.mem headers header_type) then begin
         error ~exit_value:3 "%s : declaration for header %s not found"
           file header_type;
       end
    ) specification;
  job_done ()

(*  extract_header function is used in debug mode when there are discrepancies *)
let extract_header filename template_hdr =
  let dirname = get_tmp_dirname () in
  let hdr_filename = path_concat dirname (Filename.basename filename) in
  debug "%s: %s does not conform to %s@." filename hdr_filename template_hdr;
  let create_file filename = let oc = open_out filename in close_out oc in
  create_file hdr_filename;
  let config_file_opts = if !headache_config_file = [] then Format.sprintf "-c %s" headache_config_file_default
    else String.concat "-c " !headache_config_file
  in
  let cmd = Format.sprintf "headache -c %s -e %s > %s"
      config_file_opts filename hdr_filename in
  let ret = Sys.command cmd in
  if ret <> 0 then
    if ret = 255 then
      (* Ctrl+C pressed; abort execution *)
      exit 255
    else
      debug "%s : error during header template generations@." filename

(* Check, for each file, if its license header specification corresponds to what
 * exists at the beginning of the file. If any discrepancy between the
 * specification and what the file contains is detected, a summary of all such
 * events is printed before exiting.
 *
 * @param specs a file -> license header hashtable
 * @param headers a license header -> template header file hashtable
 * @requires all files in specs exist
 * @requires all header specifications have a corresponding existing template
*)
let check_spec_discrepancies
    ~config_file_opts
    (specs: (string, string) Hashtbl.t)
    (headers: (string, string) Hashtbl.t) : unit =
  let eq_header orig_file template_hdr =
    let cmd = Format.sprintf "headache -c %s -e %s | diff -b -B -q - %s > /dev/null"
        config_file_opts orig_file template_hdr
    in
    debug ~level:2 "Running: %s@." cmd;
    let ret = Sys.command cmd in
    if ret = 255 then
      (* Ctrl+C pressed, abort execution *)
      exit 255
    else
    if ret <> 0 && !debug_level >= 1 then extract_header orig_file template_hdr ;
    ret = 0
  in
  if not !quiet then
    job_head "Checking specification discrepancies... @?";
  let n = ref 0 in
  let discrepancies = ref [] in
  Hashtbl.iter
    (fun file hdr_type ->
       if Sys.file_exists file then begin
         let hdr_file_spec = Hashtbl.find headers hdr_type in
         (* Guaranteed to exists after check_declared_headers *)
         if not (eq_header file hdr_file_spec) then begin
           discrepancies := (file, hdr_type) :: !discrepancies;
           incr n;
         end;
       end
    ) specs ;
  if !n > 0 then begin
    error ~exit_value:4 "@[<v 2>%a%d / %d files with bad headers@]@."
      (fun _ppf l ->
         List.iter
           (fun (file, hdr_type) ->
              error_fmt "%s : header differs from spec %s@."
                file hdr_type
           ) l) !discrepancies
      !n
      (Hashtbl.length specs) ;
  end;
  job_done ();
  remove_tmp_dirname ()

let check_forbidden_headers (forbidden_headers:StringSet.t) header_specifications (distributed_files:StringSet.t) =
  if not (StringSet.is_empty forbidden_headers) then begin
    if not !quiet then
      job_head "Checking that all distributed files have no forbidden header specification... @?";
    let forbidden = ref [] in
    let n = ref 0 in
    StringSet.iter
      (fun file -> try
          let license = Hashtbl.find header_specifications file in
          if StringSet.mem license forbidden_headers then
            forbidden := (file, license)::!forbidden;
          incr n;
        with Not_found -> ())
      distributed_files;
    if !forbidden <> [] then
      error ~exit_value:4 "@[<v 2>%a%d / %d files with bad headers@]@."
        (fun _ppf l ->
           List.iter
             (fun (file, hdr_type) ->
                error_fmt "%s : forbidden header %s@."
                  file hdr_type
             ) l) !forbidden
        !n
        (StringSet.cardinal distributed_files);
    job_done ()
  end

(* This is the main check. It checks that all distributed files, minus
 * exceptions, have a header specification, then launches other verifications.
 *
 * @param files_ignored -> set of files to ignore about headers
 * @param header_specifications file -> license header name hashtable
 * @param distributed_files a set of files considered for distribution
 * @param exceptions a set of files distributed but that should not be checked
*)
let check files_ignored header_specifications distributed_files exceptions =
  if not !quiet then
    job_head "Checking that all distributed files do exist... @?";
  let nonexistent_files =
    StringSet.filter (fun f -> not (Sys.file_exists f)) distributed_files
  in
  if not (StringSet.is_empty nonexistent_files) then
    error ~exit_value:5
      "@[<v 2># Non-existing files listed as distributed:@ %a@]@."
      StringSet.pp nonexistent_files;
  job_done ();
  if not !quiet then
    job_head "Checking that distributed exception files have no license header specification... @?";
  let files_licencied =
    Hashtbl.fold
      (fun file _ set -> StringSet.add file set)
      header_specifications StringSet.empty
  in
  let specified_exceptions = StringSet.inter exceptions files_licencied in
  if not (StringSet.is_empty specified_exceptions) then
    warn
      "@[<v 2># Files distributed with an header exception (even having a header specification):@ %a@]@."
      StringSet.pp specified_exceptions;
  let ignored_exceptions = StringSet.inter exceptions files_ignored in
  if not (StringSet.is_empty ignored_exceptions) then
    warn
      "@[<v 2># Files distributed with an header exception (even having to be ignored):@ %a@]@."
      StringSet.pp ignored_exceptions;
  job_done ();
  if not !quiet then
    job_head "Checking that other distributed files have a license header specification... @?";
  let files_to_check = StringSet.diff distributed_files exceptions in
  let files_specified = StringSet.union files_licencied files_ignored in
  let distributed_unspecified = StringSet.diff files_to_check files_specified in
  if not (StringSet.is_empty distributed_unspecified) then
    error ~exit_value:2
      "@[<v 2># Files distributed without specified header:@ %a@]@."
      StringSet.pp distributed_unspecified;
  job_done ();
  if not !quiet then
    job_head "Checking presence of source files having an header specification... @?" ;
  StringSet.iter
    (fun filename ->
       if not (Sys.file_exists filename) then
         warn "%s: specified but does not exist!@." filename;
    ) files_specified;
  job_done ();
  (* Other verifications start here *)
  let forbidden_headers = get_forbidden_headers () in
  check_forbidden_headers forbidden_headers header_specifications distributed_files;
  let headers = get_header_files () in
  check_declared_headers header_specifications headers;
  (* Check differences between declared headers and those found in the file *)
  check_spec_discrepancies header_specifications headers

(* Update headers according to header specifications
 * The headers are simply overwritten.
 * No warning is emitted if the new license is not the same as the old license.
 *
 * @param ~config_file_opts -> headache options setting the list of config files
 * @param header_specifications file -> license header name hashtable
 * @requires: files and licenses appearing in [header_specifications] exists
*)
let update_headers ~config_file_opts header_specifications =
  let headers = get_header_files () in
  check_declared_headers header_specifications headers;
  let update filename header =
    debug "Updating %s with license %s@." filename header;
    let cmd = Format.sprintf "headache -r -c %s -h %s %s"
        config_file_opts header filename in
    let ret = Sys.command cmd in
    if ret <> 0 then
      if ret = 255 then
        (* Ctrl+C pressed; abort execution *)
        exit 255
      else
        debug "%s : error updating header@." filename
  in
  if not !quiet then
    job_head "Updating header files ... @?";
  Hashtbl.iter
    (fun filename header_name ->
       if Sys.file_exists filename then begin
         let header_file = Hashtbl.find headers header_name in
         update filename header_file
       end
    )
    header_specifications;
  job_done ()


let check_headache_config_file () =
  let config_files =
    if !headache_config_file = [] then [ headache_config_file_default ]
    else !headache_config_file
  in List.iter (fun file ->
      if not (Sys.file_exists file) then
        error ~exit_value:5
          "Headache configuration file %s does not appear to exist@."
          file) config_files ;
  String.concat " -c " config_files

(** Option management (cont.) **)

let set_opt (var:'a option ref) (value:'a) = var := Some value

let executable_name = Sys.argv.(0)

let umsg =
  Format.sprintf "Usage: %s [options] <header spec files>@.%s"
    executable_name
    ("The default format of each <header spec files> is \"2-fields-by-line\".\n" ^
     "The different formats are:\n" ^
     "- \"2-fields-by-line\" format:\n\t<space>* <source file> <space>* ':' <space>* <license definition> <space>* <eol>\n" ^
     "- \"3-fields-by-line\" format:\n\t<space>* <source file> <space>* ':' <space>* 'header_spec' <space>* ':' <space>* <license definition> <space>* <eol>\n" ^
     "- \"3-lines\" format:\n\t<source file> <eol> 'header_spec' <eol> <license definition> <eol>\n" ^
     "- \"3-zeros\" format:\n\t<source file> <zero> 'header_spec' <zero> <license definition> <zero>\n" ^
     "where <license definition> is '.ignore' or a license definition file.\n" ^
     "The location directory of the license definitions can be specified using the -header-dirs option.\n" ^
     "When the name of a <header spec file> has the form 'path/./header-spec-file', "^
     "then the <source file> names that it contains " ^
     "are considered relative to the given 'path'.\n" ^
     "That is done before processing the option '-C <dir>'.'\n" ^
     "List of the options:")

let rec argspec = [
  "--help", Arg.Unit print_usage ,
  " print this option list and exits";
  "--stdin", Arg.Set from_stdin,
  " extract an header spec from the standard input in addition to the given header spec files";
  "-help", Arg.Unit print_usage ,
  " print this option list and exits";
  "-debug", Arg.Int (fun l -> debug_level := l),
  " enable debug messages if set to >= 1. Can also be set via environment variable HDRCK_DEBUG";
  "-quiet", Arg.Set quiet,
  "disable most messages";
  "-verbose", Arg.Unit (fun () -> quiet := false),
  "print some informative messages";

  "-forbidden-headers", Arg.String (fun set -> set_cumulative ~name:"-forbidden-headers" forbidden_headers ~set) ,
  "<license name>,... \t none of the checked files may have one of the <license name> []";
  "-header-dirs", Arg.String (fun set -> set_cumulative ~name:"-header-dirs" header_dirs ~set),
  "<directory>,... \t list of <directory> to search for license header definitions []";
  "-distrib-file", Arg.String (set_opt distrib_file),
  "<filename> \t considers only the files listed into the <filename>";
  "-header-except-file", Arg.String (set_opt header_except_file),
  "<filename> \t does not look at the files listed into the <filename>";
  "-headache-config-file", Arg.String (fun set -> set_cumulative ~name:"-headache-config-file" headache_config_file ~set),
  Format.sprintf "<filenames> \t set the list of headache configuration files [%s]" headache_config_file_default;
  "-no-exit-on-error", Arg.Unit (fun () -> exit_on_error := false),
  " does not exit on errors ";
  "-exit-on-warning", Arg.Set exit_on_warning,
  " considers warnings as errors (anyway, forces exit on errors too)";
  "-update", Arg.Unit (fun () -> mode := Update),
  " updates headers w.r.t to the <header spec file>";
  "-C", Arg.Set_string root_dir,
  Format.sprintf
    "<dir> \t prepends <dir> to filenames in header specification [%s] "
    !root_dir;
  "-spec-format", Arg.String (function
      | "2-fields-by-line" -> spec_format := Sep1Line1
      | "3-fields-by-line" -> spec_format := Sep2Line1
      | "3-lines" ->  spec_format := Line3
      | "3-zeros" ->  spec_format := Zero3
      | s -> Format.printf "invalid spec format: %s@." s ; print_usage ()),
  "<format>\t \"2-fields-by-line\"|\"3-fields-by-line\"|\"3-lines\"|\"3-zeros\"";
  "-z", Arg.Set zero_stdin,
  " force to use the spec format \"3-zeros\" when reading from stdin";
]

and sort argspec =
  List.sort (fun (name1, _, _) (name2, _, _) -> String.compare name1 name2)
    argspec

and print_usage () =
  Arg.usage (Arg.align (sort argspec)) umsg;
  exit 0

(** MAIN **)

let _ =
  (* Test if headache is in the path *)
  if Sys.command "headache -e" <> 0 then
    (Format.eprintf "error: 'headache' command not in PATH or incompatible \
                     version (option -e unsupported)@."; exit 6);
  Arg.parse (Arg.align (sort argspec)) (fun s -> spec_files := s::!spec_files) umsg;
  let config_file_opts = check_headache_config_file () in
  begin
    match !spec_files, !distrib_file, !header_except_file with
    | [], _, _ when not !from_stdin ->
      Format.printf "Please set a specification file@\n@.";
      print_usage ();
    | spec_files, distrib_file_opt, header_except_opt ->
      let specified_files = Hashtbl.create 256 in
      let ignored_files = ref StringSet.empty in
      if !from_stdin then read_specs (if !zero_stdin then Zero3 else !spec_format) ignored_files specified_files None;
      List.iter (fun f -> read_specs !spec_format ignored_files specified_files (Some f)) spec_files;
      if not !quiet then
        Format.printf "- ignored=%d@.- specified=%d@." (StringSet.cardinal !ignored_files) (Hashtbl.length specified_files);
      match !mode with
      | Check ->
        let stringset_from_opt_file = function
          | None -> StringSet.empty
          | Some file ->
            let lines = read_lines input_line file in
            List.fold_left
              (fun s l -> StringSet.add (path_concat !root_dir l) s)
              StringSet.empty lines
        in
        let distributed_files = stringset_from_opt_file distrib_file_opt in
        let header_exception_files = stringset_from_opt_file header_except_opt in
        if not !quiet then
          Format.printf "- excepted=%d@.- distributed=%d@." (StringSet.cardinal header_exception_files) (StringSet.cardinal distributed_files);
        check ~config_file_opts !ignored_files specified_files distributed_files header_exception_files
      | Update ->
        update_headers ~config_file_opts specified_files;
  end;
  if !exit_on_warning && not !has_no_warning_nor_error then exit 8
