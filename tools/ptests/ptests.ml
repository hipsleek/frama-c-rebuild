(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Command-line flags *)

let verbosity = ref 0
let check_oracles = ref false
let create_missing_oracles = ref false
let remove_empty_oracles = ref false
let nb_dune_files = ref 0
let nb_ignores = ref 0
let ignored_suites = ref []

let config_filter = ref None
let default_config = "DEFAULT"

let debug ~level msg =
  if !verbosity >= level then Format.printf msg
  else Format.ifprintf Format.std_formatter msg

(* Set to an empty string to use no wrapper *)
let wrapper_cmd = ref "frama-c-wtests -brief"

type env_t = {
  config: string
; dir: string
; dune_alias: string
; absolute_cwd: string
}

module Filename = struct
  include Filename
  let concat =
    if Sys.os_type = "Win32" then fun a b -> a ^ "/" ^ b else concat

  let cygpath r =
    let cmd =
      Format.sprintf
        "bash -c \"cygpath -m %s\""
        (String.escaped (String.escaped r))
    in
    let in_channel  = Unix.open_process_in cmd in
    let result = input_line in_channel in
    ignore(Unix.close_process_in in_channel);
    result

  let temp_file =
    if Sys.os_type = "Win32" then
      fun a b -> cygpath (temp_file a b)
    else
      fun a b -> temp_file a b
  [@@ warning "-32"]

  let sanitize f = String.escaped f

  let sanitize_with_space =
    let regexp = Str.regexp "[\\] " in
    let subst = Str.global_replace regexp " " in
    subst

  let remove_extension_opt suffixes name =
    let ext = extension name in
    if (String.equal "" ext) || not (List.mem ext suffixes) then name
    else remove_extension name
end

let str_string_match1 regexp line pos =
  if Str.string_match regexp line pos then
    try
      Some (Str.matched_group 1 line)
    with Not_found -> None
  else None

let str_string_match2 regexp line pos =
  if Str.string_match regexp line pos then
    try
      Some ((Str.matched_group 1 line),(Str.matched_group 2 line))
    with Not_found -> None
  else None

(* removes first blanks *)
let trim_right s =
  let n = ref (String.length s - 1) in
  let last_char_to_keep =
    try
      while !n > 0 do
        if String.get s !n <> ' ' then raise Exit;
        n := !n - 1
      done;
      0
    with Exit -> !n
  in
  String.sub s 0 (last_char_to_keep+1)

(** the pattern that ends the parsing of options in a test file *)
let end_comment = Str.regexp ".*\\*/"

let output_unix_error (exn : exn) =
  match exn with
  | Unix.Unix_error (error, _function, arg) ->
    let message = Unix.error_message error in
    if arg = "" then
      Format.eprintf "%s@." message
    else
      Format.eprintf "%s: %s@." arg message
  | _ -> assert false

let mv src dest =
  try
    Unix.rename src dest
  with Unix.Unix_error _ as e ->
    output_unix_error e
[@@ warning "-32"]

let unlink ?(silent = true) file =
  let open Unix in
  try
    Unix.unlink file
  with
  | Unix_error _ when silent -> ()
  | Unix_error (ENOENT,_,_) -> () (* Ignore "No such file or directory" *)
  | Unix_error _ as e -> output_unix_error e

let is_file_empty_or_nonexisting filename =
  let open Unix in
  try
    (Unix.stat filename).st_size = 0
  with
  | Unix_error (UnixLabels.ENOENT, _, _) -> (* file does not exist *)
    true
  | Unix_error _ as e ->
    output_unix_error e;
    raise e

(** special configuration, with associated oracles *)
let config_name ~env name =
  if env.config = "" then name else name ^ "_" ^ env.config

let macro_post_options = ref "" (* value set to @PTEST_POST_OPTIONS@ macro *)
let macro_pre_options  = ref "" (* value set to @PTEST_PRE_OPTIONS@  macro *)
let macro_options = ref "@PTEST_PRE_OPTIONS@ @PTEST_OPT@ @PTEST_POST_OPTIONS@"
let macro_default_options = ref "-check -no-autoload-plugins -add-symbolic-path=\"@PTEST_SESSION@:.\""

let macro_frama_c_exe = ref "frama-c"
let macro_frama_c_cmd = ref "@frama-c-exe@ @PTEST_DEFAULT_OPTIONS@"
let macro_frama_c = ref "@frama-c-exe@ @PTEST_DEFAULT_OPTIONS@ @PTEST_LOAD_OPTIONS@"
let macro_frama_c_share = ref "../../../../install/default/share/frama-c/share"

let never_disabled = ref false

let default_toplevel = ref "@frama-c@"

(** the files in [suites] whose name matches
      the pattern [test_file_regexp] will be considered as test files *)
let test_file_regexp = ".*\\.\\(c\\|i\\)$"

let example_msg =
  Format.sprintf
    "@.@[<v 0>\
     Create dune files to run the test suites in the specified directories (defaults to ./tests).@ @ \
     @[<v 1>\
     Directives of the \"ptests_config\" files:@  \
     # <comment>                 @[<v 0># Just a comment line.@]@  \
     DEFAULT_SUITES = <suite>... @[<v 0># Cumulative list of subdirectories containing test suites (specified by \"test_config\" files).@]@  \
     <mode>_SUITES = <suite>...  @[<v 0># Cumulative list of subdirectories containing test suites (specified by \"test_config_<mode>\" files).@]@  \
     IGNORE = <mode>_SUITE = <suite>...   @[<v 0># Cumulative list of ignored test suites.@]@  \
     DUNE_ALIAS = <alias-name>   @[<v 0># Dune alias @@<alias-name> has to be used to execute the next defined suites (defaults to \"ptests\").@]@  \
     @]@ \
     @[<v 1>\
     Directives of \"test_config[_<mode>]\" files:@  \
     COMMENT: <comment>  @[<v 0># Just a comment line.@]@  \
     FILEREG: <regexp>   @[<v 0># Only files matching this regex are considered as tests (default: .*\\.\\(c\\|i\\)).@]@  \
     DONTRUN:            @[<v 0># Ignores the file containing this directive.@]@  \
     EXECNOW: ([LOG|BIN] <file>)+ <command>  @[<v 0># Defines the command to execute to build a 'LOG' (text) or 'BIN' (binary) targets.@ \
     # Note: text targets are compared to oracles.@]@  \
     DEPS: <file>...     @[<v 0># Adds a dependency to next sub-test and execnow commands.@ \
     # Notes: a dependency to the included file can be added with this directive.@ \
     # That is not necessary for files mentioned into the command or options when using the %%{dep:<file>} feature of dune.@]@  \
     ENABLED_IF: <cond>  @[<v 0># Condition for following sub-tests and execnow commands to be enabled (defaults to \"true\").@]@  \
     BIN: <file>...      @[<v 0># Defines binary targets built by the next sub-test command.@]@  \
     LOG: <file>...      @[<v 0># Defines text targets built by the next sub-test command.@ \
     # Note: the text targets are compared to oracles.@]@  \
     CMD: <command>      @[<v 0># Defines the command to execute for all tests in order to get results to be compared to oracles.@]@  \
     OPT: <options>      @[<v 0># Defines a sub-test using the 'CMD' definition: <command> <options>@]@  \
     STDOPT: +<extra>    @[<v 0># Defines a sub-test and appends <extra> to the current options.@]@  \
     STDOPT: #<extra>    @[<v 0># Defines a sub-test and prepends <extra> to the current options.@]@  \
     PLUGIN: <plugin>... @[<v 0># Adds a dependency and sets macro @@PTEST_PLUGIN@@ defining the '-load-plugins' option used in the macro @@PTEST_LOAD_OPTIONS@@.@]@  \
     LIBRARY: <pkg.lib>... @[<v 0># Adds a dependency and sets macro @@PTEST_LIBRARY@@ defining the '-load-library' option used in the macro @@PTEST_LOAD_OPTIONS@@.@]@  \
     MODULE: <module>... @[<v 0># Adds a dependency and adds the corresponding '-load-module' option to the macro @@PTEST_LOAD_OPTIONS@@.@]@  \
     LIBS: <module>...   @[<v 0># Like 'MODULE' directive, but for modules that can be shared between several test files.@]@  \
     EXIT: <number>      @[<v 0># Defines the exit code required for the following sub-test commands.@]@  \
     FILTER: <cmd>       @[<v 0># Performs a transformation on the test result files before comparing with oracles.@ \
     # The oracle will be compared from the standard output of the command: cat <test-output-file> | <cmd> .@ \
     # Chaining multiple filter commands is possible by defining several FILTER directives.@ \
     # An empty command drops previous FILTER directives.@ \
     # Note: in such a command, the macros @@PTEST_ORACLE@@ and @@PTEST_RESULT@@ are set respectively to the basename of the oracle and the basename of the result.@ \
     # This allows running a 'diff' command with the oracle/result of another test configuration:@ \
     #    FILTER: diff --new-file %%{dep:@@PTEST_SUITE_DIR@@/result_configuration/@@PTEST_RESULT@@} - @]@  \
     # It is recommended to diff against the result of another configuration so that the dependencies between configurations are correctly set up.
     TIMEOUT: <delay>    @[<v 0># Set a timeout for all sub-tests.@]@  \
     NOFRAMAC:           @[<v 0># Drops previous sub-test definitions and considers that there is no defined default sub-test.@]@  \
     MACRO: <name> <def> @[<v 0># Defines macro @@<name>@@ as <def>.@]@  \
     ENV: <name> <value> @[<v 0># Defines environment variable <name> with value <value>.@]@  \
     @]@ \
     @[<v 1>\
     Some predefined macros can be used in test directives:@  \
     @@PTEST_DIR@@             # Path to the test file from the execution directory (./).@  \
     @@PTEST_FILE@@            # Test filename.@  \
     @@PTEST_NAME@@            # Basename of the test file.@  \
     @@PTEST_NUMBER@@          # Test command number.@  \
     @@PTEST_CONFIG@@          # Test configuration suffix.@  \
     @@PTEST_SUITE_DIR@@       # Path to the directory containing the source of the test file (../).@  \
     @@PTEST_RESULT_DIR@@      # Shorthand alias to @@PTEST_SUITE_DIR@@/result@@PTEST_CONFIG@@ (the result directory dedicated to the tested configuration).@  \
     @@PTEST_ORACLE@@          # Basename of the current oracle file (macro only usable in FILTER directives).@  \
     @@PTEST_RESULT@@          # Basename of the current result file (macro only usable in FILTER directives).@  \
     @@PTEST_DEFAULT_OPTIONS@@ # Default option list: %s@  \
     @@PTEST_LIBS@@            # Current list of modules defined by the LIBS directive.@  \
     @@PTEST_LIBRARY@@         # Current list of modules defined by the LIBRARY directive.@  \
     @@PTEST_DEPS@@            # Current list of dependencies defined by the DEPS directive.@  \
     @@PTEST_ENABLED_IF@@      # Current value of ENABLED_IF directive.@  \
     @@PTEST_MODULE@@          # Current list of modules defined by the MODULE directive.@  \
     @@PTEST_PLUGIN@@          # Current list of plugins set by the PLUGIN directive.@  \
     @@PTEST_SHARE_DIR@@       # Path to the share directory of the related plugin.@  \
     @@FRAMAC_SHARE@@          # Shortcut defined as follows: %s@  \
     @@DEV_NULL@@              # Set to 'NUL' for Windows platforms and to '/dev/null' otherwise.@  \
     @]@ \
     @[<v 1>\
     Other macros can only be used in test commands (CMD and EXECNOW directives):@  \
     @@PTEST_LOAD_OPTIONS@@ # Crrent list of options related to PLUGIN, LIBRARY, MODULE and LIBS to load.@  \
     @@PTEST_OPTIONS@@  # Current list of options related to OPT and STDOPT directives (for CMD directives).@  \
     @@frama-c-exe@@    # Shortcut defined as follows: %s@  \
     @@frama-c@@        # Shortcut defined as follows: %s@  \
     @@frama-c-cmd@@    # Shortcut defined as follows: %s@  \
     @]@ \
     @[<v 1>\
     Default directive values:@  \
     FILEREG: %s@  \
     CMD:     %s@  \
     EXIT:    0@  \
     @]@ \
     @[<v 1>\
     Dune aliases related to the test:@  \
     @@<alias-name>                         # Tests all configurations related to the <alias-name>@  \
     @@<alias-name>_config                  # Tests only the default configuration.@  \
     @@<alias-name>_config_<configuration>  # Tests only the specified <configuration>.@  \
     @@<PTEST_FILE>                         # Force to reproduce the corresponding test and prints the outputs.@  \
     @@<PTEST_NAME>.wtests                        # Tests the specified file.@  \
     @@<PTEST_NAME>.<PTEST_NUMBER>.exec.wtests    # Tests the specified sub-test command.@  \
     @@<PTEST_NAME>.<PTEST_NUMBER>.execnow.wtests # Tests the specified execnow command.@  \
     @@<PTEST_NAME>.diff                          # Prints differences with the oracles related to the specified file.@  \
     @@<PTEST_NAME>.<PTEST_NUMBER>.exec.diff      # Prints differences with the oracles related to the specified sub-test command.@  \
     @@<PTEST_NAME>.<PTEST_NUMBER>.execnow.diff   # Prints differences with the oracles related to the specified execnow command.@  \
     @@<PTEST_NAME>.<PTEST_NUMBER>.exec.show      # Prints the related sub-test command.@  \
     @@<PTEST_NAME>.<PTEST_NUMBER>.execnow.show   # Prints the related execnow command.@  \
     Note: the <alias-name> defaults to 'ptests'. It can be specified in different ways:@  \
     - from the command line option '-dune-alias <alias-name>'@  \
     - from directives in 'ptests_config' files such as 'DUNE_ALIAS = <alias-name>'@  \
     Note: 'dune build @<alias-name>' can be restricted to a test subdirectory in using:@  \
     - 'dune build @<subdirectory>/<alias-name>'@  \
     @]@ \
     @]"
    !macro_default_options
    !macro_frama_c_share
    !macro_frama_c_exe
    !macro_frama_c
    !macro_frama_c_cmd
    test_file_regexp
    !default_toplevel

let umsg = "Usage: frama-c-ptests [options] [names of test suites]"

let default_dune_alias = ref "ptests"
let argspec =
  [
    ("-v", Arg.Unit (fun () -> incr verbosity),
     "Increase verbosity (up to  twice)") ;

    ("-check-oracles", Arg.Set check_oracles,
     " warn on missing or empty oracles") ;

    ("-create-missing-oracles", Arg.Set create_missing_oracles,
     " creates missing oracles to allow the use of dune promote") ;

    ("-config", Arg.String (function
         | "" | "''" -> config_filter := Some ""
         | s when String.equal s default_config -> config_filter := Some ""
         | s -> config_filter := Some s),
     "<configuration> Skip the other configurations") ;

    ("-remove-empty-oracles", Arg.Set remove_empty_oracles,
     " remove empty oracles") ;

    ("-wrapper" , Arg.String (function
         | "''" -> wrapper_cmd := ""
         | s -> wrapper_cmd := s),
     " <command> Uses a wrapper to executes tests (defaults to "^ !wrapper_cmd ^")");

    ("-adds-default-options" , Arg.String (fun s -> macro_default_options := !macro_default_options ^ " " ^ s),
     " <options> Appends the <options> to the default value of the @PTEST_DEFAULT_OPTIONS@ macro");
    ("-add-options-pre", Arg.String (fun s -> macro_pre_options := !macro_pre_options ^ " " ^ s),
     "<options> Add additional options to be passed to the toplevels \
      that will be launched. <options> are added before standard test options.");
    ("-add-options-post", Arg.String (fun s -> macro_post_options := !macro_post_options ^ " " ^ s),
     "<options> Add additional options to be passed to the toplevels \
      that will be launched. <options> are added after standard test options");

    ("-macro-default-options" , Arg.String (fun s -> macro_default_options := s),
     " <value> Set the default value of the @PTEST_DEFAULT_OPTIONS@ macro (defaults to "^ !macro_default_options ^")");
    ("-macro-frama-c-exe", Arg.String (fun s -> macro_frama_c_exe := s),
     " <value> Set the default value of the @frama-c-exe@ macro (defaults to "^ !macro_frama_c_exe ^")");
    ("-macro-frama-c-cmd", Arg.String (fun s -> macro_frama_c_cmd := s),
     " <value> Set the default value of the @frama-c-cmd@ macro (defaults to "^ !macro_frama_c_cmd ^")");
    ("-macro-frama-c", Arg.String (fun s -> macro_frama_c := s),
     " <value> Set the @frama-c@ macro (defaults to "^ !macro_frama_c ^")");
    ("-macro-frama-c-share", Arg.String (fun s -> macro_frama_c_share := s),
     " <value> Set the @FRAMAC_SHARE@ macro (defaults to "^ !macro_frama_c_share ^")");

    ("-never-disabled", Arg.Set never_disabled,
     " disable the generation of the enabled_if dune field: tests are never disabled (for CI purpose)");

    ("-dune-alias", Arg.String (fun s -> default_dune_alias := s),
     " <name> Use @<name> as dune alias to execute tests (defaults to "^ !default_dune_alias ^")");
  ]

let fail s =
  Format.printf "Error: %s@.Aborting (CWD=%s).@." s (Sys.getcwd());
  exit 2

module StringMap = Map.Make (String)
(* parses the [tests/ptests_config] file (prefers the one related to the expected configuration name*)
module Ptests_config: sig

  type alias = { alias : string }
  val parse: dir:string -> alias StringMap.t StringMap.t (* config_name -> suite_name -> dune_alias *)

end = struct

  type alias = { alias : string }

  (** parses the [dir/ptests_config] file  *)
  let parse =
    let split_blank = Str.split (Str.regexp "[ ]+") in
    let regexp = Str.regexp " *\\([^#=][^= ]*\\) *=\\(.*\\)" in
    let regexp_config = Str.regexp "\\([a-zA-Z_]+\\)_SUITES" in
    let regexp_comment = Str.regexp " *#" in
    let get_key_value s = str_string_match2 regexp s 0 in
    let get_config_suites (key,value) =
      Option.bind (str_string_match1 regexp_config key 0)
        (function
          | s when String.equal s default_config -> Some ("", (split_blank value))
          | config -> Some (config, (split_blank value)))
    in
    fun ~dir ->
      let default_suites = ref StringMap.empty in
      let dune_alias = ref {alias = !default_dune_alias } in
      let ptests_config = Filename.concat dir "ptests_config" in
      let add_suite config map s =
        let add_to suites =
          Some (StringMap.update s (function | None -> Some !dune_alias
                                             | Some a ->
                                               if a.alias <> !dune_alias.alias then begin
                                                 Format.eprintf "ERROR: %s: %s_SUITES contains already %s suite (with %s as dune alias).@"
                                                   ptests_config config s !dune_alias.alias;
                                                 exit 2
                                               end;
                                               Some a) suites)
        in
        StringMap.update config (function | None -> Some (StringMap.singleton s !dune_alias)
                                          | Some suites -> add_to suites) map
      in
      let parse_config_line (key,value) =
        match get_config_suites (key,value) with
        | Some (config, suites) ->
          default_suites := List.fold_left (add_suite config) !default_suites suites
        | None -> match key with
          | "DUNE_ALIAS" -> (match split_blank value with
              | [ alias ] -> dune_alias := { alias }
              | _  ->
                Format.eprintf "ERROR: %s: %s=%s@." ptests_config key value;
                exit 2)
          | "IGNORE" ->
            (match Option.bind (get_key_value value) get_config_suites with
             | Some (_config,suites) -> nb_ignores := !nb_ignores + List.length suites
             | None -> incr nb_ignores
            );
            ignored_suites := (ptests_config ^ ":" ^ value)::!ignored_suites;
            if !verbosity >= 2 then Format.eprintf "%s: %s=%s@." ptests_config key value
          | _ ->  Format.eprintf "%s: (DEPRECATED): %s=%s@." ptests_config key value;
      in
      if Sys.file_exists ptests_config then begin
        let ch = open_in ptests_config in
        try
          (*Parse the plugin configuration file for tests. Format is 'Key=value' *)
          while true do
            let line = input_line ch in
            match get_key_value line with
            | Some (key, value) -> parse_config_line (key,value)
            | None ->
              if not ((Str.string_match regexp_comment line 0) || (split_blank line = [])) then begin
                close_in ch;
                Format.eprintf "Cannot interpret line '%s' in file %s. Aborting (CWD=%s).@." line ptests_config (Sys.getcwd());
                exit 1
              end
          done
        with
        | End_of_file -> close_in ch ;
      end
      else begin
        Format.eprintf
          "Cannot find configuration file %s. (CWD=%s).@." ptests_config (Sys.getcwd()) ;
      end;
      !default_suites
end

module SubDir: sig
  type t

  val get: t -> string

  val create: with_subdir:bool -> env:env_t -> string (** dirname *) -> t
  (** creates the needed subdirectories if [with_subdir=true].
      Anyway, fails if the given dirname doesn't exists *)

  val make_file: t -> string -> string

  val oracle_subdir: env:env_t -> t -> t
  val result_subdir: env:env_t -> t -> t

  val get_oracle_dir: env:env_t -> string
  val oracle_dir: env:env_t -> t

  val pp_file: dir:t -> Format.formatter -> string -> unit
end = struct
  type t = string

  let get s = s

  let create_if_absent dir =
    if not (Sys.file_exists dir)
    then Unix.mkdir dir 0o750 (* rwxr-w--- *)
    else if not (Sys.is_directory dir)
    then fail (Printf.sprintf "the file %s exists but is not a directory" dir)

  let oracle_subdir ~env dir = Filename.concat dir (config_name ~env "oracle")
  let result_subdir ~env dir = Filename.concat dir (config_name ~env "result")

  let make_file = Filename.concat

  let oracle_dir ~env = oracle_subdir ~env ".."
  let get_oracle_dir = oracle_dir

  let create ~with_subdir ~env dir =
    if not (Sys.file_exists dir && Sys.is_directory dir)
    then fail (Printf.sprintf "the directory %s must be an existing directory" dir);
    if (with_subdir) then begin
      create_if_absent (result_subdir ~env dir);
      create_if_absent (oracle_subdir ~env dir)
    end;
    dir

  let pp_file ~dir fmt s = Format.fprintf fmt "%s/%s" dir s
end

let dev_null = if Sys.os_type = "Win32" then "NUL" else "/dev/null"

module Macros = struct

  type t = string StringMap.t

  let add_defaults ~defaults macros =
    StringMap.merge (fun _k default cur ->
        match cur with
        | Some _ -> cur
        | _ -> default) defaults macros

  let empty = StringMap.empty

  type deprecation =
    { macro: string;
      msg: string;
      action: [ `Fail | `Continue ]; }
  let pretty_deprecation fmt deprecation =
    Format.fprintf fmt "Macro %s is deprecated. %s."
      deprecation.macro deprecation.msg
  let deprecated =
    let deprecate macro msg action deprecated =
      StringMap.add macro { macro; msg; action; } deprecated
    in
    (* ignore "unused variable deprecate" warning if no macros are currently
       deprecated *)
    ignore deprecate;
    StringMap.empty
    |> deprecate "PTEST_RESULT_FILE" "Please use PTEST_RESULT instead" `Fail
  (* Use deprecate function to add deprecated macro. The last argument is
     either `Fail or `Continue depending on if ptests should stop immediately
     or continue:
     |> deprecate "MACRO" "Message to display" `Fail *)

  let warn_if_deprecated file macro =
    try
      let deprecation = StringMap.find macro deprecated in
      Format.eprintf "%% %s: %a\n" file pretty_deprecation deprecation;
      if deprecation.action = `Fail then begin
        Format.eprintf "%% Exiting ptests. Please fix deprecation issue.\n";
        exit 1
      end
    with Not_found -> ()

  let pp_macros fmt macros =
    Format.fprintf fmt "Macros (%d):@."  (StringMap.cardinal macros);
    StringMap.iter (fun key data -> Format.fprintf fmt "- %s -> %s@." key data) macros;
    Format.fprintf fmt "End macros@."

  type does_expand = {
    has_ptest_file : bool;
    has_ptest_opt : bool;
    has_frama_c_exe : bool;
  }


  let does_expand ~file =
    let macro_regex = Str.regexp "@\\([-A-Za-z_0-9]+\\)@" in
    fun macros s ->
      let has_ptest_file = ref false in
      let has_ptest_opt = ref false in
      let has_ptest_options = ref false in
      let has_frama_c_exe = ref false in
      debug ~level:2 "%% %s: Expand: %s@." file s;
      debug ~level:5 "%a" pp_macros macros;
      let nb_loops = ref 0 in
      let rec aux s =
        if !nb_loops > 100 then
          fail (file ^ ": possible infinite recursion in macro expansion: "^ s)
        else incr nb_loops ;
        let expand_macro = function
          | Str.Text s -> s
          | Str.Delim s ->
            match str_string_match1 macro_regex s 0  with
            | Some macro -> begin
                warn_if_deprecated file macro;
                (match macro with
                 | "PTEST_FILE" -> has_ptest_file := true
                 | "PTEST_OPT" -> has_ptest_opt := true
                 | "PTEST_OPTIONS" -> has_ptest_options := true
                 | "frama-c-exe" -> has_frama_c_exe := true
                 | _ -> ());
                debug ~level:5 "%% %s:     - macro is %s\n%!" file macro;
                try
                  let replacement = StringMap.find macro macros in
                  debug ~level:4 "%% %s:     - replacement for %s is %s\n%!" file macro replacement;
                  aux replacement
                with Not_found -> s
              end
            | None -> s
        in
        String.concat "" (List.map expand_macro (Str.full_split macro_regex s))
      in
      let r =
        try aux s
        with e ->
          Format.eprintf "%s: uncaught exception %s\n%!" file (Printexc.to_string e);
          raise e
      in
      debug ~level:4 "%% %s: Expansion result: %s@." file r;
      { has_ptest_file= !has_ptest_file;
        has_ptest_opt= !has_ptest_opt;
        has_frama_c_exe= !has_frama_c_exe;
      }, r

  let expand ~file (macros:t) s =
    snd (does_expand ~file macros s)

  (* Same as expand but make sure the string is never empty by adding quotes if needed. *)
  let expand_not_empty ~file (macros:t) s =
    let v = snd (does_expand ~file macros s) in
    if v = "" then "\"\"" else v

  (* Removes the expansions to an empty string from the list (for DEPS,PLUGIN,MODULE,BIN,LOG *)
  let expand_list ~file (macros:t) ls =
    List.filter_map (fun s ->
        let s = expand ~file macros s in
        if String.equal s "" then None else Some s) ls

  let expand_enabled_if ~file (macros:t) enabled_if =
    Option.map (fun s ->
        let s = String.trim (expand ~file macros s) in
        if s = "" then "true" else s) enabled_if

  let expand_env ~file macros env =
    List.map (fun (k, v) -> k, expand_not_empty ~file macros v) env

  let add_list l map =
    List.fold_left (fun acc (k,v) -> StringMap.add k v acc) map l

  let add_expand ~file name def macros =
    StringMap.add name (expand ~file macros def) macros

  let default_macros () = add_list
      [ "frama-c-exe", !macro_frama_c_exe;
        "frama-c-cmd", !macro_frama_c_cmd;
        "frama-c",     !macro_frama_c;
        "DEV_NULL",    dev_null;

        "FRAMAC_SHARE",!macro_frama_c_share;

        "PTEST_DEFAULT_OPTIONS", !macro_default_options;
        "PTEST_OPTIONS",         !macro_options;
        "PTEST_PRE_OPTIONS",     !macro_pre_options;
        "PTEST_POST_OPTIONS",    !macro_post_options;

        "PTEST_DEPS",   "";
        "PTEST_LIBS",   "";
        "PTEST_MODULE", "";
        "PTEST_PLUGIN", "";
        "PTEST_LIBRARY", "";
        "PTEST_ENABLED_IF", "true";
      ] empty

end

module StringSet = Set.Make(String)
type deps =  {
  load_plugin: string list option;
  load_library: string list option;
  load_libs: string list option;
  load_module: string list option;
  deps_cmd: string list option;
  enabled_if: string option;
}

type execnow =
  { ex_cmd: string;      (** command to launch *)
    ex_log: string list; (** log files *)
    ex_bin: string list; (** bin files *)
    ex_timeout: string;
    ex_deps: deps;
    ex_env_var: (string * string) list;
    ex_exit_code: string option
  }


(** configuration of a directory/test. *)
type cmd = {
  toplevel: string;
  opts: string;
  macros: Macros.t;
  env_var: (string * string) list;
  exit_code: string option;
  logs: string list;
  bins: string list;
  deps: deps;
  timeout: string
}

type config =
  {
    dc_test_regexp: string; (** regexp of test files. *)
    dc_execnow    : execnow list; (** command to be launched before
                                       the toplevel(s)
                                  *)
    dc_libs : string list option; (** libraries to compile *)
    dc_deps : string list option ; (** deps *)
    dc_enabled_if : string option ; (** enabled if condition *)
    dc_plugin : string list option; (** only plugins to load *)
    dc_library : string list option; (** additional libraries to load *)
    dc_module : string list option; (** module to load *)
    dc_macros: Macros.t; (** existing macros. *)
    dc_env_var : (string * string) list; (** env *)
    dc_default_toplevel   : string;
    (** full path of the default toplevel. *)
    dc_filter     : string option; (** optional filter to apply to
                                       standard output *)
    dc_exit_code  : string option; (** required exit code *)
    dc_commands   : cmd list;
    (** toplevel full path, options to launch the toplevel on, and list
        of output files to monitor beyond stdout and stderr. *)
    dc_dont_run   : bool;
    dc_framac     : bool;
    dc_default_log: string list;
    dc_default_bin: string list;
    dc_timeout: string
  }

(* Scans the test directives (from test_config or test files) and expands macros as soon as possible *)
module Test_config: sig

  (** The [test_config] filename related to the expected configuration name *)
  val filename: string
  val current_config: env:env_t -> SubDir.t -> config

  val scan_directives:  drop:bool ->
    SubDir.t -> file:string -> Scanf.Scanning.in_channel -> config -> config
  val scan_test_file: env:env_t -> SubDir.t -> file:string -> config -> config

  (* updates the configuration directives that do not depend of the test number and
     returns a getter of the PTEST_xxx variables including the one depending on the test number *)
  val ptest_vars: env:env_t -> file:string -> string * (nth:int -> Macros.t -> Macros.t)

  (** Split a string on spaces, tabs and commas, except if they are escaped with
      '\'. *)
  val split_list: string -> string list
end = struct

  let ptest_vars ~env ~file =
    let ptest_config = config_name ~env "" in
    let ptest_file = Filename.sanitize file in
    let ptest_name = Filename.remove_extension file in
    let ptest_session = Filename.dirname env.dir in
    let ptest_session = env.absolute_cwd ^ "/_build/default" ^ (if ptest_session = "." then "" else "/" ^ ptest_session) in
    let ptest_vars =
      [ "PTEST_SESSION", ptest_session ;
        "PTEST_CONFIG", ptest_config;
        "PTEST_DIR", ".";
        "PTEST_SHARE_DIR", "../../../share";
        "PTEST_RESULT_DIR", ".";
        "PTEST_SUITE_DIR", "..";
        "PTEST_FILE", ptest_file;
        "PTEST_NAME", ptest_name;
      ] in
    ptest_name,
    fun ~nth macros ->
      Macros.add_list (("PTEST_NUMBER", string_of_int nth)::ptest_vars) macros

  (** the name of the directory-wide configuration file*)
  let filename = "test_config"

  let default_commands config =
    [ { toplevel=config.dc_default_toplevel;
        opts="";
        exit_code=None;
        macros=config.dc_macros;
        env_var=[];
        logs=[];
        bins=[];
        deps={ load_plugin=None;
               load_library=None;
               load_libs=None;
               load_module=None;
               deps_cmd=None;
               enabled_if=None;
             };
        timeout=""
      } ]

  let default_config () =
    { dc_test_regexp = test_file_regexp;
      dc_macros = Macros.default_macros ();
      dc_env_var = [];
      dc_execnow = [];
      dc_libs = None;
      dc_deps = None;
      dc_enabled_if = None;
      dc_plugin = None;
      dc_library = None;
      dc_module = None;
      dc_filter = None ;
      dc_exit_code = None;
      dc_default_toplevel = !default_toplevel;
      dc_commands = [];
      dc_dont_run = false;
      dc_framac = true;
      dc_default_log = [];
      dc_default_bin = [];
      dc_timeout = "";
    }

  let scan_execnow ~file ~once ex_exit_code ex_timeout ex_deps ex_env_var (s:string) =
    if once=false then
      Format.eprintf "%s: using EXEC directive (DEPRECATED): %s@."
        file s;
    let rec aux (s:execnow) =
      try
        Scanf.sscanf s.ex_cmd "%_[ ]LOG%_[ ]%[-A-Za-z0-9_',+=:.\\@@]%_[ ]%s@\n"
          (fun name cmd ->
             if name = "" then fail (file ^": EXEC"^ (if once then "NOW" else "") ^ " directive with an invalid LOG filename: " ^ s.ex_cmd);
             aux { s with ex_cmd = cmd; ex_log = name :: s.ex_log })
      with Scanf.Scan_failure _ ->
      try
        Scanf.sscanf s.ex_cmd "%_[ ]BIN%_[ ]%[A-Za-z0-9_.\\\"-@@]%_[ ]%s@\n"
          (fun name cmd ->
             if name = "" then fail (file ^": EXEC"^ (if once then "NOW" else "") ^ " directive with an invalid BIN filename: " ^ s.ex_cmd);
             aux { s with ex_cmd = cmd; ex_bin = name :: s.ex_bin })
      with Scanf.Scan_failure _ ->
      try
        Scanf.sscanf s.ex_cmd "%_[ ]make%_[ ]%s@\n"
          (fun cmd ->
             (* It should be better to use a specific macro into the command (such as @MAKE@) for that. *)
             Format.eprintf "%s: EXEC%s directive with a make command (DEPRECATED): %s@."
               file (if once then "NOW" else "") cmd;
             let s = aux ({ s with ex_cmd = cmd; }) in
             { s with ex_cmd = "make "^cmd; } )
      with Scanf.Scan_failure _ ->
        s
    in
    let execnow = aux
        { ex_cmd = s;
          ex_log = [];
          ex_bin = [];
          ex_deps;
          ex_env_var;
          ex_timeout;
          ex_exit_code;
        }
    in
    if execnow.ex_log = [] && execnow.ex_bin = [] then
      (* Cannot detect the problem when the LOG/BIN is a macro expanded later into an @EMPTY_STRING@ *)
      Format.eprintf "%s: EXEC%s without LOG nor BIN target (DEPRECATED): %s@."
        file (if once then "NOW" else "") s;
    execnow

  let make_custom_opts =
    let space = Str.regexp " " in
    fun ~file:_ ~dir:_ stdopts s ->
      let rec aux opts s =
        try
          Scanf.sscanf s "%_[ ]%1[+#\\-]%_[ ]%S%_[ ]%s@\n"
            (fun c opt rem ->
               match c with
               | "+" -> aux (opt :: opts) rem
               | "#" -> aux (opts @ [ opt ]) rem
               | "-" -> aux (List.filter (fun x -> x <> opt) opts) rem
               | _ -> assert false (* format of scanned string disallow it *))
        with
        | Scanf.Scan_failure _ ->
          if s <> "" then s::opts else opts
        | End_of_file -> opts
      in
      (* NB: current settings does not allow to remove a multiple-argument
         option (e.g. -verbose 2).
      *)
      (* revert the initial list, as it will be reverted back in the end. *)
      let opts = aux (List.rev (Str.split space stdopts)) s in
      (* preserve options ordering *)
      List.fold_right (fun x s -> s ^ " " ^ x) opts ""

  let deps_of_config ?(deps={load_module=None;load_library=None;load_libs=None;load_plugin=None;deps_cmd=None;enabled_if=None}) config =
    let select ~prev ~config = match config with
      | None -> prev
      | _ -> config
    in
    { load_module = select ~prev:deps.load_module ~config:config.dc_module;
      load_plugin = select ~prev:deps.load_plugin ~config:config.dc_plugin;
      load_library = select ~prev:deps.load_library ~config:config.dc_library;
      load_libs= select ~prev:deps.load_libs ~config:config.dc_libs;
      deps_cmd = select ~prev:deps.deps_cmd ~config:config.dc_deps;
      enabled_if = select ~prev:deps.enabled_if ~config:config.dc_enabled_if
    }

  let config_exec ~once ~drop:_ ~file s current =
    let s = Macros.expand ~file current.dc_macros s in
    { current with
      dc_execnow =
        scan_execnow ~file ~once
          current.dc_exit_code current.dc_timeout (deps_of_config current) current.dc_env_var
          s :: current.dc_execnow
    }

  let split_list =
    (* considers blanks (not preceded by '\'), tabs and commas as separators *)
    let nonsep_regexp = Str.regexp "[\\] " in (* removed for being reintroduced *)
    let sep_regexp = Str.regexp "[\t ,]+" in
    fun s -> (* splits on '\ ' first then on ' ' or ',' *)
      let r = List.fold_left (fun acc -> function
          | Str.Text s -> List.rev_append (Str.full_split sep_regexp s) acc
          | (Str.Delim _ as delim) -> delim::acc)
          []
          (Str.full_split nonsep_regexp s)
      in (* [r] is in the reverse order and the next [fold] restores the order *)
      let add s (glue,prev,curr) =
        if glue then false,(s^prev),curr
        else false,s,(if prev = "" then curr else prev::curr)
      in
      let acc = List.fold_left (fun ((_,prev,curr) as acc) -> function
          | Str.Delim ("\\ " as nonsep) -> true,(nonsep^prev),curr (* restore '\ ' *)
          | Str.Delim _ -> add "" acc (* separator *)
          | Str.Text s -> add s acc) (false,"",[]) r
      in
      let _,_,res = (add "" acc) in
      res

  let config_enabled_if ~drop:_ ~file ~dir:_ s current =
    let s = Macros.expand ~file current.dc_macros s in
    let s = if s = "" then "true" else s in
    { current with
      dc_enabled_if = Some s;
      dc_macros = Macros.add_list ["PTEST_ENABLED_IF", s] current.dc_macros }

  let config_libs ~drop:_ ~file ~dir:_ s current =
    let s = Macros.expand ~file current.dc_macros s in
    let l = List.map (fun s -> Filename.remove_extension_opt [ ".cmxs" ; ".cma" ; ".ml" ] s) (split_list s) in
    { current with
      dc_libs = Some l;
      dc_macros = Macros.add_list ["PTEST_LIBS", s] current.dc_macros }

  let config_gen var_name update_field =
    fun ~drop:_ ~file ~dir:_ s current ->
    Macros.warn_if_deprecated file var_name;
    let s = Macros.expand ~file current.dc_macros s in
    let l = split_list s in
    let current = update_field current (Some l) in
    { current with dc_macros = Macros.add_list [var_name, s] current.dc_macros }

  let config_deps =
    config_gen "PTEST_DEPS" (fun c dc_deps -> { c with dc_deps })

  let config_plugin =
    config_gen "PTEST_PLUGIN" (fun c dc_plugin -> { c with dc_plugin })

  let config_library =
    config_gen "PTEST_LIBRARY" (fun c dc_library -> { c with dc_library })

  let config_module macro_name ~drop:_ ~file ~dir:_ s current =
    Macros.warn_if_deprecated file macro_name;
    let s = Macros.expand ~file current.dc_macros s in
    let l = List.map (fun s -> (Filename.remove_extension_opt [".cmxs"; ".cmxo"; ".ml"] s) ^ ".cmxs") (split_list s) in
    { current with
      dc_module = Some l;
      dc_macros = Macros.add_list [macro_name, s] current.dc_macros }

  let macro_name_regex = "[-_0-9a-zA-Z]+"
  let env_name_regex = "[_0-9a-zA-Z]+"

  let parse_split_space name_regex s =
    let regex_str = Format.sprintf "[ \t]*\\(%s\\)\\([ \t]+.*\\)?$" name_regex in
    let regex = Str.regexp regex_str in
    match str_string_match1 regex s 0 with
    | None -> None
    | Some name ->
      let value =
        try Str.matched_group 2 s |> String.trim with Not_found -> (* empty text *) ""
      in Some (name, value)

  let config_macro ~drop:_ ~file ~dir s current =
    match parse_split_space macro_name_regex s with
    | None ->
      Format.eprintf "%a: cannot understand MACRO definition: %s@." (SubDir.pp_file ~dir) file s;
      current
    | Some (name, def) ->
      debug ~level:4 "%%   - New macro %s with definition %s@." name def;
      { current with dc_macros = Macros.add_expand ~file name def current.dc_macros }

  let config_env ~drop:_ ~file ~dir s current =
    match parse_split_space env_name_regex s with
    | None ->
      Format.eprintf "%a: cannot understand ENV definition: %s@." (SubDir.pp_file ~dir) file s;
      current
    | Some (name, value) ->
      (* Make quotes explicit in dune file if the string is empty. *)
      let value = Macros.expand_not_empty ~file current.dc_macros value in
      debug ~level:4 "%%   - New ENV variable %s with value %s@." name value;
      let dc_env_var =
        (* If the environment variable is already set, we replace its value. *)
        let old, others = List.partition (fun (name', _) -> name = name') current.dc_env_var in
        match old with
        | _ :: _ :: _ -> assert false (* should not happen *)
        | [] -> others @ [(name, value)]
        | [ _, value' ] ->
          debug ~level:4 "%%   - Replacing previously set ENV variable %s=%s with value %s@." name value' value;
          others @ [(name, value)]
      in
      { current with dc_env_var }

  type parsing_env = {
    current_default_log: string list;
    current_default_bin: string list;
    current_default_cmds: cmd list;
  }

  let default_parsing_env = ref {
      current_default_log = [] ;
      current_default_bin = [] ;
      current_default_cmds = []
    }

  let set_default_parsing_env config =
    default_parsing_env := {
      current_default_log = config.dc_default_log;
      current_default_bin = config.dc_default_bin;
      current_default_cmds = List.rev config.dc_commands;
    }

  let config_options =
    [ "CMD",
      (fun ~drop:_ ~file ~dir:_ s current ->
         let s = Macros.expand ~file current.dc_macros s in
         { current with dc_default_toplevel = s});

      "OPT",
      (fun ~drop ~file ~dir:_ s current ->
         if not (drop || current.dc_framac) then
           Format.eprintf "%s: a NOFRAMAC directive has been defined before a sub-test defined by an 'OPT' directive (That NOFRAMAC directive could be misleading.).@."
             file;
         let s = Macros.expand ~file current.dc_macros s in
         let t =
           { toplevel = current.dc_default_toplevel;
             opts = s;
             macros = current.dc_macros ;
             exit_code = current.dc_exit_code ;
             logs = current.dc_default_log;
             bins = current.dc_default_bin;
             timeout = current.dc_timeout;
             deps = deps_of_config current;
             env_var = current.dc_env_var;
           }
         in
         { current with
           dc_default_log = !default_parsing_env.current_default_log;
           dc_default_bin = !default_parsing_env.current_default_bin;
           dc_commands = t :: current.dc_commands });

      "STDOPT",
      (fun ~drop ~file ~dir s current ->
         if not (drop || current.dc_framac) then
           Format.eprintf "%s: a NOFRAMAC directive has been defined before a sub-test defined by a 'STDOPT' directive (That NOFRAMAC directive could be misleading.).@."
             file;
         let s = Macros.expand ~file current.dc_macros s in
         let new_top =
           List.map
             (fun command ->
                { toplevel= current.dc_default_toplevel;
                  opts= make_custom_opts ~file ~dir command.opts s;
                  macros= current.dc_macros;
                  exit_code = current.dc_exit_code;
                  logs= command.logs @ current.dc_default_log;
                  bins= command.bins @ current.dc_default_bin;
                  timeout= current.dc_timeout;
                  deps = deps_of_config ~deps:command.deps current;
                  env_var = current.dc_env_var;
                })
             (if !default_parsing_env.current_default_cmds = [] then
                default_commands current
              else !default_parsing_env.current_default_cmds)
         in
         { current with dc_commands = new_top @ current.dc_commands;
                        dc_default_log = !default_parsing_env.current_default_log @
                                         current.dc_default_log;
                        dc_default_bin = !default_parsing_env.current_default_bin @
                                         current.dc_default_bin });
      "FILEREG",
      (fun ~drop:_ ~file ~dir:_ s current ->
         let s = Macros.expand ~file current.dc_macros s in
         { current with dc_test_regexp = s });

      "FILTER",
      (fun ~drop:_ ~file ~dir:_ s current ->
         let s = Macros.expand ~file current.dc_macros s in
         let s = trim_right s in
         match current.dc_filter with
         | None when s="" -> { current with dc_filter = None }
         | None           -> { current with dc_filter = Some s }
         | Some filter    -> { current with dc_filter = Some (s ^ " | " ^ filter) });

      "EXIT",
      (fun ~drop:_ ~file ~dir:_ s current ->
         let s = Macros.expand ~file current.dc_macros s in
         { current with dc_exit_code = Some s });

      "COMMENT",
      (fun ~drop:_ ~file:_ ~dir:_ _ acc -> acc);

      "DONTRUN",
      (fun ~drop:_ ~file:_ ~dir:_ _ current ->
         { current with dc_dont_run = true });

      "EXECNOW",
      (fun ~drop ~file ~dir:_ s acc ->
         config_exec ~once:true ~file ~drop s acc);
      "EXEC",
      (fun ~drop ~file ~dir:_ s acc ->
         config_exec ~once:false ~file ~drop s acc);

      "MACRO", config_macro;
      "ENV", config_env;
      "LIBS", config_libs;
      "DEPS", config_deps;
      "ENABLED_IF", config_enabled_if;
      "MODULE", config_module "PTEST_MODULE";
      "PLUGIN", config_plugin;
      "LIBRARY", config_library;

      "LOG",
      (fun ~drop:_ ~file ~dir:_ s current ->
         let s = Macros.expand ~file current.dc_macros s in
         let l = split_list s in
         { current with dc_default_log = current.dc_default_log @ l});

      "BIN",
      (fun ~drop:_ ~file ~dir:_ s current ->
         let s = Macros.expand ~file current.dc_macros s in
         let l = split_list s in
         { current with dc_default_bin = current.dc_default_bin @ l});

      "TIMEOUT",
      (fun ~drop:_ ~file ~dir:_ s current ->
         let s = Macros.expand ~file current.dc_macros s in
         { current with dc_timeout = s });

      "NOFRAMAC",
      (fun ~drop ~file ~dir:_ _ current ->
         if not drop && current.dc_commands <> [] && current.dc_framac then
           Format.eprintf "%s: a NOFRAMAC directive has the effect of ignoring previous defined sub-tests (by some 'OPT' or 'STDOPT' directives that seems misleading). @."
             file;
         { current with dc_commands = []; dc_framac = false; });
    ]

  let scan_directives ~drop dir ~file scan_buffer default =
    set_default_parsing_env default;
    let r = ref { default with dc_commands = [] } in
    let treat_line s =
      try
        Scanf.sscanf s "%[ *]%[_A-Za-z0-9]: %s@\n"
          (fun _ name opt ->
             try
               r := (List.assoc name config_options) ~drop ~file ~dir opt !r
             with Not_found ->
               Format.eprintf "@[%s: unknown directive: %s@.@]" file name)
      with
      | Scanf.Scan_failure _ ->
        if Str.string_match end_comment s 0
        then raise End_of_file
        else ()
      | End_of_file -> (* ignore blank lines. *) ()
    in
    try
      while true do
        if Scanf.Scanning.end_of_input scan_buffer then raise End_of_file;
        Scanf.bscanf scan_buffer "%s@\n" treat_line
      done;
      assert false
    with
    | End_of_file ->
      (match !r.dc_commands with
       | [] when !r.dc_framac -> { !r with dc_commands = default.dc_commands }
       | l -> { !r with dc_commands = List.rev l })

  (* test for a possible toplevel configuration. *)
  let current_config ~env dir =
    let default_config = default_config () in
    let general_config_file = SubDir.make_file dir (config_name ~env filename) in
    if Sys.file_exists general_config_file
    then begin
      debug ~level:2 "%% Parsing global config file=%s@." general_config_file;
      let scan_buffer = Scanf.Scanning.from_file general_config_file in
      scan_directives ~drop:false
        (SubDir.create ~env ~with_subdir:false Filename.current_dir_name)
        ~file:general_config_file
        scan_buffer
        default_config
    end
    else begin
      debug ~level:2 "%% There is no global config file=%s@." general_config_file;
      default_config
    end

  let split_config s = Str.split (Str.regexp ",[ ]*") s

  let is_config name =
    let prefix = "run.config" in
    let len = String.length prefix in
    String.length name >= len && String.sub name 0 len = prefix

  let scan_test_file ~env dir ~file default =
    let f = SubDir.make_file dir file in
    let exists_as_file =
      try
        (Unix.lstat f).Unix.st_kind = Unix.S_REG
      with | Unix.Unix_error _ | Sys_error _ -> false
    in
    if exists_as_file then begin
      let scan_buffer = Scanf.Scanning.open_in f in
      let rec scan_config () =
        (* space in format string matches any number of whitespace *)
        Scanf.bscanf scan_buffer " /* %s@\n"
          (fun names ->
             let is_current_config name =
               name = "run.config*" ||
               name = "run.config" && env.config = ""  ||
               name = "run.config_" ^ env.config
             in
             let configs = split_config (String.trim names) in
             match List.find_opt is_current_config configs with
             | Some name ->
               (* Found options for current config! *)
               debug ~level:2 "%% Parsing %s of file=%s@." name f ;
               scan_directives ~drop:false dir ~file:f scan_buffer default
             | None -> begin
                 (* config name does not match: eat config and continue.
                     But only if the comment is still opened by the end of
                     the line and we are indeed reading a config
                 *)
                 (if List.exists is_config configs &&
                     not (Str.string_match end_comment names 0) then
                    ignore (scan_directives ~drop:true dir ~file:f scan_buffer default);
                  scan_config ())
               end)
      in
      let config =
        try
          let options =  scan_config () in
          Scanf.Scanning.close_in scan_buffer;
          options
        with End_of_file | Scanf.Scan_failure _ ->
          Scanf.Scanning.close_in scan_buffer;
          debug ~level:2 "%% No run.config directives in file=%s@." f ;
          default
      in
      if config.dc_commands = [] && config.dc_framac
      then { config with dc_commands = default_commands config }
      else config
    end else
      (* if the file has disappeared, don't try to run it... *)
      { default with dc_dont_run = true }

end

type toplevel_command =
  { macros: Macros.t;
    env_var: (string*string) list;
    log_files: string list;
    bin_files: string list;
    test_name : string ;
    file : string ;
    options : string ;
    toplevel: string ;
    filter : string option ;
    exit_code : int ;
    directory : SubDir.t ;
    nth : int;
    execnow:bool;
    timeout: string;
    deps: deps;
  }

let name_without_extension command =
  try
    Filename.chop_extension command.file
  with
    Invalid_argument _ ->
    fail ("this test file does not have any extension: " ^
          command.file)

let make_result_file ~env:_ x = x
let make_oracle_file ~env x = Filename.concat (config_name ~env "oracle") x

let gen_prefix gen_file cmd =
  let prefix = gen_file (name_without_extension cmd) in
  prefix ^ "." ^ (string_of_int cmd.nth)

let oracle_prefix ~env = gen_prefix (make_oracle_file ~env)
let log_prefix ~env = gen_prefix (make_result_file ~env)

let list_of_deps = function
  | None -> []
  | Some l -> l

let basic_command_string command =
  let plugins_options =
    let load_option opt deps = match list_of_deps deps with
      | [] -> ""
      | l -> Printf.sprintf "%s=%s" opt (String.concat "," l)
    in
    let opt_plugin = load_option "-load-plugin" command.deps.load_plugin in
    let opt_library = load_option "-load-library" command.deps.load_library in
    let opt_libs = load_option "-load-module" command.deps.load_libs in
    let opt_modules =  load_option "-load-module" command.deps.load_module in
    String.concat " " [opt_plugin; opt_library; opt_libs; opt_modules]
  in
  let macros = (* set expanded macros that can be used into CMD directives *)
    Macros.add_list [
      "PTEST_OPT", Macros.expand ~file:command.file command.macros command.options;
      "PTEST_LOAD_OPTIONS", plugins_options;
    ] command.macros in
  let toplevel =
    let in_toplevel,toplevel = Macros.does_expand ~file:command.file macros command.toplevel in
    if command.execnow || in_toplevel.has_ptest_opt then toplevel
    else begin
      let has_ptest_file,options =
        let in_option,options = Macros.does_expand ~file:command.file macros command.options in
        (in_option.has_ptest_file || in_toplevel.has_ptest_file),
        (if in_toplevel.has_frama_c_exe then
           [ Macros.expand ~file:command.file macros "@PTEST_PRE_OPTIONS@" ;
             options ;
             Macros.expand ~file:command.file macros "@PTEST_POST_OPTIONS@" ;
           ]
         else [ options ])
      in
      let options = List.filter (fun s -> s <> "") options in
      let options = if has_ptest_file then options
        else (Filename.sanitize (Filename.basename command.file))::options
      in
      String.concat " " (toplevel::options)
    end
  in
  let raw_command = if command.timeout = "" then toplevel
    else "ulimit -t " ^ command.timeout ^ " && " ^ toplevel
  in raw_command

let pp_plugin_name fmt s =
  Format.fprintf fmt "frama-c-%s" s

let pp_plugin_as_lib fmt s =
  Format.fprintf fmt "%a.core" pp_plugin_name s

let pp_list fmt l = List.iter (Format.fprintf fmt " %S") l
module Fmt = struct
  let pp_plugin_as_package fmt s =
    let base =
      if String.contains s '.' then
        String.sub s 0 (String.index s '.')
      else s
    in
    Format.fprintf fmt "%a" pp_plugin_name base
  let pp_library_as_package fmt s =
    let base =
      if String.contains s '.' then
        String.sub s 0 (String.index s '.')
      else s
    in
    Format.fprintf fmt "%s" base
  let quote pr fmt s = Format.fprintf fmt "%S" (Format.asprintf "%a" pr s)
  let list pr fmt l = List.iter (fun s -> Format.fprintf fmt " %a" pr s) l
  let var_libavailable pr fmt s = Format.fprintf fmt "%%{lib-available:%a}" pr s
  let package_as_deps pr fmt s = Format.fprintf fmt "(package %a)" pr s
end

let home_regexp = Str.regexp "^\\(~\\|[$]HOME\\|[$]\\{HOME\\}\\)\\(/.*\\)"
let get_home_env () =
  try
    Unix.getenv "HOME"
  with Not_found -> "~"
let replace_home_env s =
  match str_string_match2 home_regexp s 0 with
  | None -> s
  | Some (_,subdir) -> (get_home_env ()) ^ subdir

let deps_regexp = Str.regexp "^\\([^:]*\\):\\(.*\\)"

let pp_list_deps fmt l =
  let l =
    List.map Test_config.split_list l
    |> List.flatten
  in
  List.iter (fun s ->
      let s = Filename.sanitize_with_space s in
      match str_string_match2 deps_regexp s 0 with
      | None ->
        let s = replace_home_env s in
        if String.contains s '*' then
          (* If no dependency kind has been found, automatically add glob_files
             if we detect a glob file pattern. *)
          Format.fprintf fmt " (glob_files %S)" s
        else
          Format.fprintf fmt " %S" s
      | Some (kind,deps) ->
        let deps = replace_home_env deps in
        (* kind={env_var,source_tree,glob_files,...} *)
        Format.fprintf fmt " (%s %S)" kind deps) l



let update_enabled_if ~enabled_if deps =
  (* code similar to pp_enabled_if_content *)
  Option.iter (fun cond -> enabled_if := StringSet.add cond !enabled_if) deps.enabled_if;
  let iter_enabled_if pp lib =
    let cond = Format.asprintf "%a" Fmt.(var_libavailable pp) lib in
    enabled_if := StringSet.add cond !enabled_if
  in
  List.iter (iter_enabled_if pp_plugin_as_lib)
    (list_of_deps deps.load_plugin);
  List.iter (iter_enabled_if Format.pp_print_string)
    (list_of_deps deps.load_library)

let pp_enabled_if_content fmt deps =
  Format.fprintf fmt "(and %s%a%a)"
    (Option.value ~default:"true" deps.enabled_if)
    Fmt.(list (var_libavailable pp_plugin_as_lib)) (list_of_deps deps.load_plugin)
    Fmt.(list (var_libavailable Format.pp_print_string))
    (list_of_deps deps.load_library)

let pp_enabled_if fmt deps =
  Format.fprintf fmt "%s(enabled_if %a)"
    (if !never_disabled then ";" else "")
    pp_enabled_if_content deps

let pp_command_deps fmt command =
  Format.fprintf fmt "%S %a (package frama-c) %a %a"
    (* the test file *)
    command.file
    (* from DEPS: LIBS: and MODULE: directives *)
    pp_list_deps (list_of_deps command.deps.deps_cmd)
    (* from PLUGIN directives *)
    Fmt.(list (package_as_deps (quote pp_plugin_as_package))) (list_of_deps command.deps.load_plugin)
    (* from LIBRARY directives *)
    Fmt.(list (package_as_deps (quote pp_library_as_package)))
    (list_of_deps command.deps.load_library)

let show_cmd =
  let regexp_read = Str.regexp "%{read:\\([^}]+\\)}" in
  let subst_read = Str.global_replace regexp_read "$(cat \\1)" in
  let regexp_ignore = Str.regexp "%{[a-z]+:\\([^}]+\\)}" in
  let subst_ignore = Str.global_replace regexp_ignore "\\1" in
  fun x ->
    subst_ignore (subst_read x)

let redirection ?reslog ?errlog cmd =
  match reslog, errlog with
  | None, None         -> cmd
  | None, Some err     -> Format.sprintf "%s 2> %S" cmd err
  | Some res, None     -> Format.sprintf "%s > %S" cmd res
  | Some res, Some err -> Format.sprintf "%s > %S 2> %S" cmd res err

let ptests_alias ~env = config_name ~env (env.dune_alias ^ "_config")

type wtest = {
  dir: (string [@default ""]); (* information on the test directory *)
  info: (string [@default ""]); (* information *)
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
[@@deriving yojson]

let update_oracle_dir ~env wtest =
  if wtest.log = [] then wtest else
    { wtest with
      oracle_dir = SubDir.get_oracle_dir ~env
    }

let std = false
let pp_wtest ?(compacted=false) fmt wtest =
  let writer = (if compacted
                then (fun json -> Format.fprintf fmt "%s" (Yojson.Safe.to_string ~std json))
                else (fun json -> Format.fprintf fmt "%a" (Yojson.Safe.pretty_print ~std) json))
  in writer (wtest_to_yojson wtest)

let default_wtest = match wtest_of_yojson (Yojson.Safe.from_string "{}") with
  | Ok r -> r
  | _ -> assert false

let print_json_wrapper ~file wtest =
  (* Prints the JSON file for the wrapper *)
  debug ~level:2 "%% Generates %S wrapper file...@." file;
  let wrapper_cout = open_out file in
  let wrapper_fmt = Format.formatter_of_out_channel wrapper_cout  in
  Format.fprintf wrapper_fmt "%a@." (pp_wtest ~compacted:false) wtest;
  close_out wrapper_cout

let oracle_target oracle_fmt dir fname =
  let oracle = SubDir.make_file dir fname in
  if not (Sys.file_exists oracle) then begin
    if !create_missing_oracles then begin
      let code = Sys.command ("touch " ^ oracle) in
      if code <> 0 then
        Format.printf "  - cannot create missing oracle: %s@." oracle
      else
        Format.printf "  - creates missing oracle: %s@." oracle;
    end
    else if !check_oracles then
      Format.printf "  - missing oracle: %s@." oracle;
  end
  else if is_file_empty_or_nonexisting oracle then begin
    if !remove_empty_oracles then begin
      Format.printf "  - removes empty oracle: %s@." oracle;
      unlink ~silent:false oracle
    end
    else if !check_oracles then
      Format.printf "  - empty oracle: %s@." oracle;
  end;
  Format.fprintf oracle_fmt
    "(rule (target %S) (mode fallback) (action (write-file %S \"\")))\n" fname fname

let subtest_alias_prefix cmd =
  Format.sprintf "%s.%d.%s"
    cmd.test_name
    cmd.nth
    (if cmd.execnow then "execnow" else "exec")

let get_exit_code ~file = function
  | None -> 0
  | Some exit_code ->
    try int_of_string exit_code with
    | _ ->
      Format.eprintf "@[%s: integer required for directive EXIT: %s (defaults to 0)@]@." file exit_code ;
      0

let pp_accepted_exit_code fmt cmd =
  Format.fprintf fmt "with-accepted-exit-codes %d" cmd.exit_code

let pp_setenv fmt (var, value) =
  Format.fprintf fmt "(setenv %s %s " var value

let pp_env fmt l =
  List.iter (Format.fprintf fmt "%a" pp_setenv) l

let pp_close_env fmt l =
  List.iter (fun _ -> Format.fprintf fmt ")") l

let command_string ~env ~result_fmt ~oracle_fmt command =
  let log_prefix = log_prefix ~env command in
  let reslog = log_prefix ^ ".res.log" in
  let errlog = log_prefix ^ ".err.log" in
  let cmdreslog,cmderrlog = match command.filter with
    | None -> reslog,errlog
    | Some _ -> (log_prefix ^ ".res.unfiltered-log"),(log_prefix ^ ".err.unfiltered-log")
  in
  let command_string = basic_command_string command in
  let filter_res,filter_err,wtest =
    match command.filter with
    | None -> "","",default_wtest
    | Some filter ->
      let oracle_regexp = Str.regexp "@PTEST_ORACLE@" in
      let result_regexp = Str.regexp "@PTEST_RESULT@" in
      let filter_cmd funfiltred foracle fresult =
        let filter = Str.global_replace oracle_regexp foracle filter in
        let filter = Str.global_replace result_regexp fresult filter in
        Format.sprintf "cat %s | %s" funfiltred filter
      in
      let filter_res =
        filter_cmd cmdreslog (log_prefix ^ ".res.oracle") reslog
      in
      let filter_err =
        filter_cmd cmderrlog (log_prefix ^ ".err.oracle") errlog
      in
      filter_res,filter_err, { default_wtest with
                               sedout = redirection ~reslog filter_res ;
                               sederr = redirection ~reslog:errlog filter_err ;
                               tmpout = cmdreslog ;
                               tmperr = cmderrlog ;
                             }
  in
  let oracle_prefix = oracle_prefix ~env command in
  let wtest = update_oracle_dir ~env
      { wtest with
        dir = SubDir.get (SubDir.result_subdir ~env command.directory) ;
        info = Format.sprintf "TEST #%d OF TEST FILE %s/%s"
            command.nth (SubDir.get command.directory) command.file;
        cmd = redirection ~reslog:cmdreslog ~errlog:cmderrlog command_string ;
        out = reslog;
        err = errlog;
        ret_code = command.exit_code;
        log = command.log_files;
        bin = command.bin_files;
        oracle_out = Filename.concat ".." (oracle_prefix ^ ".res.oracle");
        oracle_err = Filename.concat ".." (oracle_prefix ^ ".err.oracle");
      }
  in
  let subtest_alias = subtest_alias_prefix command in
  let wrapper_basename = subtest_alias ^ ".wtests" in
  if !wrapper_cmd <> "" then begin
    Format.fprintf result_fmt
      "(rule ; %s\n  \
       (alias %S)\n  \
       (targets %S %S %a %a)\n  \
       (deps %S %S %S %a %a)\n  \
       %a\n\
       (action %a(run %s %S %S %a))%a\n\
       )@."
      (* rule: *)
      wtest.info
      (* alias: *)
      wrapper_basename
      (* targets: *)
      cmderrlog
      cmdreslog
      pp_list command.log_files
      pp_list command.bin_files
      (* deps: *)
      wrapper_basename
      wtest.oracle_out
      wtest.oracle_err
      pp_list (List.map (Filename.concat wtest.oracle_dir) command.log_files)
      pp_command_deps command
      (* enabled_if: *)
      pp_enabled_if command.deps
      (* action: *)
      pp_env command.env_var
      !wrapper_cmd
      wrapper_basename
      wtest.cmd
      pp_list (if command.filter = None then [] else [wtest.sedout ; wtest.sederr])
      pp_close_env command.env_var
    ;

    let wtest =
      { wtest with
        cmd = show_cmd wtest.cmd ;
        sedout = show_cmd wtest.sedout ;
        sederr = show_cmd wtest.sederr
      }
    in
    (* Prints the JSON file for the wrapper *)
    print_json_wrapper wtest
      ~file:(SubDir.make_file (SubDir.result_subdir ~env command.directory) wrapper_basename);
  end
  else begin
    Format.fprintf result_fmt
      "(rule ; %s\n  \
       (alias %S)\n  \
       (targets %S %S %a %a)\n  \
       (deps   %a)\n  \
       %a\n\
       (action %a(with-stderr-to %S (with-stdout-to %S (%a (system %S)))))%a\n\
       )@."
      (* rule: *)
      wtest.info
      (* alias: *)
      wrapper_basename
      (* targets: *)
      cmderrlog
      cmdreslog
      pp_list command.log_files
      pp_list command.bin_files
      (* deps: *)
      pp_command_deps command
      (* enabled_if: *)
      pp_enabled_if command.deps
      (* action: *)
      pp_env command.env_var
      cmderrlog
      cmdreslog
      pp_accepted_exit_code command
      command_string
      pp_close_env command.env_var
  end;
  let filter_rule txt fin fout cmd =
    if cmd <> "" then
      Format.fprintf result_fmt
        "(rule ; FILTER %s #%d OF TEST FILE %S\n  \
         (deps %S)
         %a\n\
         (action (with-stdout-to %S (with-accepted-exit-codes (or 0 1 2 125) (system %S))))\n\
         )@."
        (* rule: *)
        txt
        command.nth
        command.file
        (* deps: *)
        fin
        (* enabled_if: *)
        pp_enabled_if command.deps
        (* action: *)
        fout cmd
  in
  filter_rule "RES" cmdreslog reslog filter_res ;
  filter_rule "ERR" cmderrlog errlog filter_err ;
  List.iteri (fun n log ->
      Format.fprintf result_fmt
        "(rule ; COMPARE TARGET #%d OF TEST #%d FOR TEST FILE %S\n  \
         (alias %s)\n  \
         %a\n\
         (action (diff %S %S))\n\
         )@."
        (* rule: *)
        n command.nth command.file
        (* alias: *)
        (subtest_alias ^ ".diff")
        (* enabled_if: *)
        pp_enabled_if command.deps
        (* action: *)
        (SubDir.make_file (SubDir.oracle_dir ~env) log)
        log
    ) command.log_files;
  Format.fprintf result_fmt
    "(rule ; REPRODUCE TEST #%d OF TEST FILE %S\n  \
     (alias %S)\n  \
     (deps  %a (universe))\n  \
     %a\n\
     (action %a(%a (system %S)))%a\n\
     )@."
    (* rule: *)
    command.nth command.file
    (* alias: *)
    subtest_alias
    (* deps: *)
    pp_command_deps command
    (* enabled_if: *)
    pp_enabled_if command.deps
    (* action: *)
    pp_env command.env_var
    pp_accepted_exit_code command
    command_string
    pp_close_env command.env_var
  ;
  Format.fprintf result_fmt
    "(rule ; SHOW TEST COMMAND #%d OF TEST FILE %S\n  \
     (alias %S)\n  \
     (deps  %a (universe))\n  \
     %a\n\
     (action (system %S))\n\
     )@."
    (* rule: *)
    command.nth command.file
    (* alias: *)
    (subtest_alias ^ ".show")
    (* deps: *)
    pp_command_deps command (* to get an updated build even in case of using the result *)
    (* enabled_if: *)
    pp_enabled_if command.deps
    (* action: *)
    ("echo '" ^ show_cmd wtest.cmd ^"'");

  let diff_alias = subtest_alias ^ ".diff" in
  (* diff with oracles *)
  Format.fprintf result_fmt
    "(rule\n  \
     (alias %S)\n  \
     %a\n\
     (action (diff %S %S))\n\
     )@."
    (* alias: *)
    diff_alias
    (* enabled_if: *)
    pp_enabled_if command.deps
    (* action: *)
    wtest.oracle_out
    reslog;
  Format.fprintf result_fmt
    "(rule\n  \
     (alias %S)\n  \
     %a\n\
     (action (diff %S %S))\n\
     )@."
    (* alias: *)
    diff_alias
    (* enabled_if: *)
    pp_enabled_if command.deps
    (* action: *)
    wtest.oracle_err
    errlog;
  Format.fprintf result_fmt
    "(alias (name %S)\n  \
     (deps (alias %S))\n  \
     %a\n\
     )@."
    (command.test_name ^ ".diff")
    diff_alias
    pp_enabled_if command.deps;
  Format.fprintf result_fmt
    "(alias (name %S)\n  \
     (deps (alias %S))\n  \
     %a\n\
     )@."
    (ptests_alias ~env)
    (command.test_name ^ ".diff")
    pp_enabled_if command.deps
  ;
  let oracle_subdir = SubDir.oracle_subdir ~env command.directory in
  oracle_target oracle_fmt oracle_subdir (Filename.basename (oracle_prefix ^ ".err.oracle"));
  oracle_target oracle_fmt oracle_subdir (Filename.basename (oracle_prefix ^ ".res.oracle"));
  List.iter (oracle_target oracle_fmt oracle_subdir) command.log_files ;
  ()

let deps_command ~file macros deps =
  let subst = Macros.expand_list ~file macros in
  let enabled_if = Macros.expand_enabled_if ~file macros deps.enabled_if in
  let load_plugin = Option.map subst deps.load_plugin in
  let load_library = Option.map subst deps.load_library in
  let load_module = Option.map subst deps.load_module in
  let load_libs = Option.map (fun libs -> List.map (fun s -> s^".cmxs") (subst libs)) deps.load_libs in
  let deps_cmd = Option.map subst deps.deps_cmd in
  { enabled_if; load_plugin; load_library; load_module; load_libs;
    (* Merge LIBS: MODULE: and DEPS: directives as a dependency to files *)
    deps_cmd = Some ((list_of_deps load_libs) @ (list_of_deps load_module) @ (list_of_deps deps_cmd));
  }

let update_modules ~file ~modules deps =
  let load_module = list_of_deps deps.load_module in
  if load_module <> [] then begin
    let plugin_libs = List.fold_left (fun acc e -> StringSet.union acc (StringSet.of_list e)) StringSet.empty
        [(List.map (Format.asprintf "%a" pp_plugin_as_lib) (list_of_deps deps.load_plugin));
         (List.map (Format.asprintf "%s") (list_of_deps deps.load_library));
         (List.map (fun s -> Filename.remove_extension_opt [".cmxs"; ".cma"; ".ml"]  (Filename.basename s))
            (list_of_deps deps.load_libs))]
    in
    List.iter (fun cmxs ->
        let cmxs = Filename.remove_extension_opt [".cmxs"; ".cmo"; ".ml"] cmxs in
        modules := StringMap.update cmxs (function
            | None -> Some (plugin_libs,[file])
            | Some (set,files) -> Some ((StringSet.inter set plugin_libs),file::files)
          ) !modules) (StringSet.elements (StringSet.of_list load_module));
  end

(** process a test file *)
let process_file ~env ~result_fmt ~oracle_fmt file directory config ~modules ~enabled_if =
  let config = Test_config.scan_test_file ~env directory ~file config in
  if not config.dc_dont_run then
    let test_name, ptest_vars = Test_config.ptest_vars ~env ~file in
    let make_cmd =
      let i = ref 0 in
      fun { toplevel; opts=options; macros; exit_code; logs; bins; timeout; deps; env_var} ->
        let nth = !i in
        incr i ;
        let macros = ptest_vars ~nth macros in
        let macros = Macros.add_defaults ~defaults:config.dc_macros macros in
        let log_files = Macros.expand_list ~file macros logs in
        let bin_files = Macros.expand_list ~file macros bins in
        let env_var = Macros.expand_env ~file macros env_var in
        let deps = deps_command ~file macros deps in
        update_modules ~file ~modules deps;
        update_enabled_if ~enabled_if deps;
        command_string ~env ~result_fmt ~oracle_fmt
          { test_name ; file; options; toplevel; directory; nth; timeout;
            macros; log_files; bin_files;
            filter = (* from a global directive applied to all OPT tests  *)
              (match config.dc_filter with None -> None | Some s -> Some (Macros.expand ~file macros s));
            exit_code = get_exit_code ~file exit_code;
            execnow=false;
            deps;
            env_var;
          }
    in
    let make_execnow_cmd =
      let e = ref 0 in
      fun execnow ->
        let nth = !e in
        incr e ;
        let macros = ptest_vars ~nth Macros.empty in
        let macros = Macros.add_defaults ~defaults:config.dc_macros macros in
        let cmd =
          let deps = deps_command ~file macros execnow.ex_deps in
          let env_var = Macros.expand_env ~file macros execnow.ex_env_var in
          update_modules ~file ~modules deps;
          update_enabled_if ~enabled_if deps;
          { test_name; file; directory; nth;
            log_files = [];
            bin_files = [];
            options = "";
            toplevel = execnow.ex_cmd;
            exit_code = get_exit_code ~file execnow.ex_exit_code;
            timeout=execnow.ex_timeout;
            macros;
            filter = None; (* no FILTER applied to EXECNOW LOG *)
            execnow = true;
            deps = deps;
            env_var;
          }
        in
        let cmd_string = basic_command_string cmd in
        let wtest = update_oracle_dir ~env
            { default_wtest with
              dir = SubDir.get (SubDir.result_subdir ~env cmd.directory) ;
              info = Format.sprintf "EXECNOW #%d OF TEST FILE %s/%s"
                  nth (SubDir.get directory) file;
              cmd = cmd_string;
              ret_code = cmd.exit_code;
              log = Macros.expand_list ~file cmd.macros execnow.ex_log;
              bin = Macros.expand_list ~file cmd.macros execnow.ex_bin;
            }
        in
        if wtest.log = [] && wtest.bin = [] then
          (* Detect the problem even if the LOG/BIN is a macro expanded there into an @EMPTY_STRING@ *)
          Format.eprintf "%s: EXEC/EXECNOW#%d without LOG nor BIN target (DEPRECATED): %s@."
            file nth wtest.cmd;
        let subtest_alias = subtest_alias_prefix cmd in
        let wrapper_basename = subtest_alias ^ ".wtests" in
        if !wrapper_cmd <> "" then begin
          Format.fprintf result_fmt
            "(rule ; %s\n  \
             (alias %s)\n  \
             (deps %a %a)\n  \
             (targets %a %a)\n  \
             %a\n\
             (action %a(run %s %%{dep:%s} %S))%a\n\
             )@."
            (* rule: *)
            wtest.info
            (* alias: *)
            wrapper_basename
            (* deps: *)
            pp_list (List.map (Filename.concat wtest.oracle_dir) wtest.log)
            pp_command_deps cmd
            (* targets: *)
            pp_list wtest.log
            pp_list wtest.bin
            (* enabled_if: *)
            pp_enabled_if cmd.deps
            (* action: *)
            pp_env cmd.env_var
            !wrapper_cmd
            wrapper_basename
            wtest.cmd
            pp_close_env cmd.env_var;
          let wtest =
            { wtest with
              cmd = show_cmd wtest.cmd ;
            }
          in
          (* Prints the JSON file for the wrapper *)
          print_json_wrapper wtest
            ~file:(SubDir.make_file (SubDir.result_subdir ~env cmd.directory) wrapper_basename);
        end
        else begin
          Format.fprintf result_fmt
            "(rule ; %s\n  \
             (alias %s)\n  \
             (deps (package frama-c)%a)\n  \
             (targets %a %a)\n  \
             %a\n\
             (action %a(%a (system %S)))%a\n\
             )@."
            (* rule: *)
            wtest.info
            (* alias: *)
            wrapper_basename
            (* deps: *)
            pp_command_deps cmd
            (* targets: *)
            pp_list wtest.log
            pp_list wtest.bin
            (* enabled_if: *)
            pp_enabled_if cmd.deps
            (* action: *)
            pp_env cmd.env_var
            pp_accepted_exit_code cmd
            wtest.cmd
            pp_close_env cmd.env_var
        end;
        let oracle_subdir = SubDir.oracle_subdir ~env cmd.directory in
        List.iter (oracle_target oracle_fmt oracle_subdir) wtest.log ;
        List.iter (oracle_target oracle_fmt oracle_subdir) wtest.bin ;
        Format.fprintf result_fmt
          "(rule ; SHOW EXECNOW COMMAND #%d OF TEST FILE %S\n  \
           (alias %s)\n  \
           (deps  %a (universe))\n  \
           %a\n\
           (action (system %S))\n\
           )@."
          (* rule: *)
          nth file
          (* alias: *)
          (subtest_alias ^ ".show")
          (* deps: *)
          pp_command_deps cmd (* to get an updated build even in case of using the result *)
          (* enabled_if: *)
          pp_enabled_if cmd.deps
          (* action: *)
          ("echo '" ^ show_cmd wtest.cmd ^"'");
        ;
        let diff_subtest_alias = subtest_alias ^ ".diff" in
        List.iteri (fun n log ->
            Format.fprintf result_fmt
              "(rule ; COMPARE TARGET #%d OF EXECNOW #%d FOR TEST FILE %S\n  \
               (alias %s)\n  \
               %a\n\
               (action (diff %S %S))\n\
               )@."
              (* rule: *)
              n nth file
              (* alias: *)
              diff_subtest_alias
              (* enabled_if: *)
              pp_enabled_if cmd.deps
              (* action: *)
              (SubDir.make_file (SubDir.oracle_dir ~env) log)
              log
          ) wtest.log;
        if wtest.bin <> [] then
          (* for EXECNOW with unconsumed BIN and without LOG *)
          Format.fprintf result_fmt
            "(alias (name %S)\n  \
             (deps %a)\n  \
             %a\n\
             )@."
            diff_subtest_alias
            pp_list wtest.bin
            pp_enabled_if cmd.deps;
        if (wtest.log <> []) || (wtest.bin <> []) then begin
          Format.fprintf result_fmt
            "(alias (name %S)\n  \
             (deps (alias %S))\n  \
             %a\n\
             )@."
            (cmd.test_name ^ ".diff")
            diff_subtest_alias
            pp_enabled_if cmd.deps;
          Format.fprintf result_fmt
            "(alias (name %S)\n  \
             (deps (alias %S))\n  \
             %a\n\
             )@."
            (ptests_alias ~env)
            (cmd.test_name ^ ".diff")
            pp_enabled_if cmd.deps
        end
    in
    let config = { config with dc_execnow = List.rev config.dc_execnow } in
    if config.dc_commands <> [] || config.dc_execnow <> [] then begin
      let pp_list_alias fmt l = List.iter (Format.fprintf fmt "(alias %S)") l in
      Format.fprintf result_fmt
        "; TEST FILE %S\n\
         (alias (name %S)\n  \
         (deps %a%a)) ; to performs all sub-tests related to a file\n\
         (alias (name %S)\n  \
         (deps %a%a)) ; to reproduce and visualize the all sub-test outputs related to a file@."
        file
        (* alias #1 *)
        (Format.sprintf "%s.wtests" test_name)
        pp_list_alias (List.mapi (fun i _ -> Format.sprintf "%s.%d.exec.wtests" test_name i) config.dc_commands)
        pp_list_alias (List.mapi (fun i _ -> Format.sprintf "%s.%d.execnow.wtests" test_name i) config.dc_execnow)
        (* alias #2 *)
        file
        pp_list_alias (List.mapi (fun i _ -> Format.sprintf "%s.%d.exec" test_name i) config.dc_commands)
        pp_list_alias (List.mapi (fun i _ -> Format.sprintf "%s.%d.execnow.wtests" test_name i) config.dc_execnow);
    end ;
    List.iter make_cmd config.dc_commands;
    List.iter make_execnow_cmd config.dc_execnow;
    true
  else
    false

let test_pattern config =
  let regexp = Str.regexp config.dc_test_regexp in
  fun file -> Str.string_match regexp file 0

let build_modules fmt modules =
  (* Prints rules dedicated to the build of the MODULEs *)
  let n = ref 0 in
  StringMap.iter (fun cmxs (libs,files) ->
      let cmxs = Filename.basename cmxs in
      let files = StringSet.elements (StringSet.of_list files) in
      incr n;
      Format.fprintf fmt
        "(executable ; MODULE #%d FOR TEST FILES: %a\n  \
         (name %S)\n  \
         (modules %S)\n  \
         (modes plugin)\n  \
         (libraries frama-c.init frama-c.boot frama-c.kernel %a)\n  \
         (flags :standard -w -50-9-32-6-34 -open Frama_c_kernel)\n\
         )@."
        (* executable: *)
        !n pp_list files
        (* name: *)
        cmxs
        (* module: *)
        cmxs
        (* libraries: *)
        pp_list (StringSet.elements libs))
    modules

let warn_if_not_enabled =
  let dune_cond_regexp = Str.regexp "^(\\(.*\\))" in
  let doublequote_regexp = Str.regexp "\"" in
  let dune_var_regexp = Str.regexp "%{" in
  let paren_regexp = Str.regexp "[()]" in
  let dune_cond_item s = Str.global_replace dune_cond_regexp "\\1" s in
  let escaped_cond s =
    let s = Str.global_replace dune_var_regexp "\\%{" s in
    Str.global_replace doublequote_regexp "\\\"" s
  in
  let safe_cond s =
    Str.global_replace paren_regexp "\"\\0\"" s
  in
  fun ~env ~suite fmt enabled_if ->
    if not (StringSet.is_empty enabled_if) then begin
      Format.fprintf fmt
        "(alias (name disabled_%s)\n  \
         (deps (alias disabled_%s)))@."
        env.dune_alias (ptests_alias ~env);
      Format.fprintf fmt
        "(alias (name %s)\n  \
         (deps (alias disabled_%s)))@."
        env.dune_alias env.dune_alias;
      Format.fprintf fmt
        "(alias (name %s)\n  \
         (deps (alias disabled_%s)))@."
        (ptests_alias ~env) (ptests_alias ~env);
      let pp_disabled fmt cond = Format.fprintf fmt "(not %s)" cond in
      let pp_enabled  fmt cond =
        let cond = dune_cond_item cond in
        Format.fprintf fmt "              (echo \"- %s: \" %s \"\\n\")\n  "
          (escaped_cond cond) (safe_cond cond)
      in
      let conds = StringSet.elements enabled_if in
      Format.fprintf fmt
        "(rule ; Warns when some test conditions are disabled\n  \
         (alias disabled_%s)\n  \
         (deps (universe))\n  \
         (enabled_if (or false %a))\n  \
         (action (progn (echo \"WARNING: Enabling conditions of some tests are false for @@%s/%s\\n\")\n  \
         %a))\n\
         )@."
        (* alias: *)
        (ptests_alias ~env)
        (* enabled_if *)
        (Fmt.list pp_disabled) conds
        (* action: *)
        suite
        (ptests_alias ~env)
        (Fmt.list pp_enabled) conds
    end

let process ~env default_config (suites:Ptests_config.alias StringMap.t) =
  StringMap.iter
    (fun suite alias ->
       let env = { env with dune_alias = alias.Ptests_config.alias } in
       let suite = Filename.concat env.dir suite in
       let directory = SubDir.create ~with_subdir:true ~env suite in
       let result_dune_file = SubDir.make_file (SubDir.result_subdir ~env directory) "dune" in
       debug ~level:2 "%% Generates %S file for test suite %s%s and dune-alias=@@%s ...@."
         result_dune_file suite (if env.config = "" then "" else (", config=" ^ env.config)) env.dune_alias;
       let dir_config =
         let config = SubDir.make_file directory (config_name ~env Test_config.filename) in
         if Sys.file_exists config
         then begin
           let scan_buffer = Scanf.Scanning.from_file config in
           debug ~level:2 "%% Parsing suite config file=%s@." config ;
           Test_config.scan_directives ~drop:false directory ~file:config
             scan_buffer default_config
         end
         else begin
           debug ~level:2 "%% There is no suite config file=%s@." config ;
           default_config
         end
       in
       let result_cout = (open_out result_dune_file) in
       let result_fmt = Format.formatter_of_out_channel result_cout  in
       Format.fprintf result_fmt "(copy_files ../*.*)@.";
       Format.fprintf result_fmt
         "(alias (name %s)\n  \
          (deps (alias %s)))@."
         env.dune_alias (ptests_alias ~env);
       let oracle_dune_file = SubDir.make_file (SubDir.oracle_subdir ~env directory) "dune" in
       let oracle_cout = (open_out oracle_dune_file) in
       let oracle_fmt = Format.formatter_of_out_channel oracle_cout in
       let has_test = ref false in
       let modules = ref StringMap.empty in
       let enabled_if = ref StringSet.empty in
       let dir_files = Array.to_list (Sys.readdir (SubDir.get directory)) in
       (* ignore hidden files (starting with '.') *)
       let dir_files =
         List.filter (fun n -> String.get n 0 <> '.') dir_files
       in
       debug ~level:3 "%% - Look at %d entries of the directory...@." (List.length dir_files);
       List.iter
         (fun file ->
            assert (Filename.is_relative file);
            if test_pattern dir_config file
            then begin
              debug ~level:2 "%% - Process test file %s ...@." file;
              has_test := process_file ~env ~result_fmt ~oracle_fmt file directory dir_config ~modules ~enabled_if || !has_test;
            end;
         ) dir_files;
       build_modules result_fmt !modules;
       warn_if_not_enabled ~env ~suite result_fmt !enabled_if;
       Format.fprintf result_fmt "@.";
       Format.fprintf oracle_fmt "@.";
       close_out result_cout;
       close_out oracle_cout;
       if not !has_test then begin (* there is no test_command *)
         unlink ~silent:false result_dune_file;
         unlink ~silent:false oracle_dune_file
       end
       else nb_dune_files := !nb_dune_files +2
    ) suites

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
  !suites

let () =
  let suites = match parse_args () with
    | [] -> [ "tests" ]
    | l -> l
  in
  let absolute_cwd = Unix.getcwd () in
  List.iter (fun dir ->
      Format.printf "Test directory: %s@." dir;
      let suites = Ptests_config.parse ~dir in
      debug ~level:1 "%% Nb config= %d@." (StringMap.cardinal suites);
      let nb = !nb_dune_files in
      StringMap.iter (fun config_mode suites ->
          match !config_filter with
          | Some only when only <> config_mode ->
            let nbi = StringMap.cardinal suites in
            Format.printf "%% - %s_SUITES -> nb suites= %d (skipped config -> ignored)@."
              (match config_mode with "" -> default_config | s -> s) nbi;
            nb_ignores := !nb_ignores + nbi
          | _ ->
            debug ~level:1 "%% - %s_SUITES -> nb suites= %d@."
              (match config_mode with "" -> default_config | s -> s) (StringMap.cardinal suites);
            let env = { config = config_mode ; dir ; dune_alias = "" ; absolute_cwd} in
            let directory = SubDir.create ~with_subdir:false ~env dir in
            let config = Test_config.current_config ~env directory in
            process ~env config suites) suites ;
      let nbi = !nb_ignores in
      debug ~level:1 "%% Nb dune files= %d@." (!nb_dune_files-nb);
      if (!nb_ignores-nbi) <> 0 then Format.printf "- %d ignored suite(s)@." (!nb_ignores-nbi);
    ) suites ;
  Format.printf "Total number of generated dune files: %d@." !nb_dune_files;
  if !nb_ignores <> 0 then begin
    Format.printf "- ignored suites= %d@." !nb_ignores;
    List.iter (Format.printf "  - %s@.") !ignored_suites
  end
