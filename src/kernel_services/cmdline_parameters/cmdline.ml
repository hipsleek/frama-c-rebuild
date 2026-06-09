(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This file implements how the command line is parsed.

    The parsing of the Frama-C command line is done in several stages. The first
    one is done when this module is loaded by caml (that is very early).

    At each stage [s], each option [o] put on the command line are checked again
    the recognized options at stage [s]. If [o] is recognized, then its
    associated action is performed. Otherwise [o] will be proceed by the next
    stage.

    Complexity of this algorithm is [O(2*s*o)] where [s] is the number of stages
    and [o] is the number of options puts on the command line options. That is
    quite bad and that could be improved. However it should be good enough in
    practice because there are not so many options put on the command line and
    others Frama-C algorithms take much more time. Parsing the command line
    option is not the more difficult/longer stuff for Frama-C ;-). *)

(* ************************************************************************* *)
(** {2 Global declarations} *)
(* ************************************************************************* *)

let () =
  let seed = "Frama-C." ^ System_config.Version.id_and_codename in
  Project.init ~seed ~source:"kernel"

module Debug_level = Log.Make_level(struct let default = 0 end)
module Verbose_level = Log.Make_level(struct let default = 1 end)

let dkey = Kernel_log.dkey_cmdline

let quiet_ref = ref false
let tty = ref Unix.(isatty stdout && Ansi_escape.is_supported ())
let permissive = ref false

let set_tty isatty =
  tty := isatty;
  Log.reset_stdout ~isatty  ()

(* ************************************************************************* *)
(** {2 Handling errors} *)
(* ************************************************************************* *)

let long_plugin_name s =
  if s = Kernel_log.kernel_label_name then "Frama-C" else "Plug-in " ^ s

let get_backtrace () =
  (* Get the backtrace before potentially destroying it in the handler below *)
  let bt = Printexc.get_backtrace () in
  let current_src_string =
    let src_text =
      try
        let src = Log.get_current_source () in
        Format.asprintf "%a" Filepos.pretty src
      with Not_found -> "<unset>"
    in
    Format.asprintf "Current source was: %s@." src_text
  in
  current_src_string ^ "The full backtrace is:\n" ^ bt

let request_crash_report =
  Format.sprintf
    "Please report as 'crash' at https://git.frama-c.com/pub/frama-c/issues\n\
     Your Frama-C version is %s.\n\
     Note that a version and a backtrace alone often do not contain enough\n\
     information to understand the bug. Guidelines for reporting bugs are at:\n\
     https://git.frama-c.com/pub/frama-c/-/wikis/Guidelines-for-reporting-bugs\n"
    System_config.Version.id_and_codename

let protect = function
  | Sys.Break ->
    "User Interruption (Ctrl-C)"
    ^ if Kernel_log.Debug_level.get () > 0 then "\n" ^ get_backtrace () else ""
  | Sys_error s -> Printf.sprintf "System error: %s" s
  | Unix.Unix_error(err, a, b) ->
    let error = Printf.sprintf "System error: %s" (Unix.error_message err) in
    (match a, b with
     | "", "" -> error
     | "", t | t, "" -> Printf.sprintf "%s (%s)" error t
     | f, x -> Printf.sprintf "%s (%s %S)" error f x)
  | Log.AbortError p ->
    Printf.sprintf "%s aborted: invalid user input."
      (long_plugin_name p)
  | Log.AbortFatal p ->
    let bt = get_backtrace () in
    Printf.sprintf
      "%s\n%s aborted: internal error.\n%s"
      bt
      (long_plugin_name p)
      request_crash_report
  | Log.FeatureRequest(s, p, m) ->
    let name = long_plugin_name p in
    let pp_oloc fmt = function
      | None -> Format.fprintf fmt ""
      | Some pos -> Format.fprintf fmt "%a: " Filepos.pretty pos
    in
    Format.asprintf
      "%a%s aborted: unimplemented feature.\n\
       You may send a feature request at \
       https://git.frama-c.com/pub/frama-c/issues with:\n\
       '[%s] %s'."
      pp_oloc s name name m
  | e ->
    let bt = get_backtrace () in
    Printf.sprintf
      "%s\nUnexpected error (%s).\n%s"
      bt
      (Printexc.to_string e)
      request_crash_report

(* ************************************************************************* *)
(** {2 Exiting Frama-C} *)
(* ************************************************************************* *)

module NormalExit = Hook.Make()
let at_normal_exit = NormalExit.extend
let run_normal_exit_hook = NormalExit.apply

module ErrorExit = Hook.Build(struct type t = exn end)
let at_error_exit = ErrorExit.extend
let run_error_exit_hook = ErrorExit.apply
let error_occurred_ref = ref None
let error_occurred exn = error_occurred_ref := Some exn

type exit = unit
exception Exit
let nop = ()

let catch_at_toplevel = function
  | Log.AbortError _ -> true
  | Log.FeatureRequest _ -> true
  | _ -> Kernel_log.Debug_level.get () = 0

let exit_code = function
  | Log.AbortError _ -> 1
  | Sys.Break -> 2
  | Log.FeatureRequest _ -> 3
  | Log.AbortFatal _ -> 4
  | _ -> 125

let bail_out_ref = ref (fun _ -> assert false)
let bail_out () =
  !bail_out_ref (); (* bail_out_ref must exit 0 *)
  Kernel_log.fatal "Cmdline.bail_out must `exit 0'."

let catch_toplevel_run ~f ~at_normal_exit ~on_error =
  (* both functions below handle errors at exit hooks *)
  let run_at_normal_exit () =
    try
      at_normal_exit ();
      Log.clean ()
    with exn ->
      Kernel_log.feedback ~level:0
        "error occurring when exiting Frama-C: stopping exit procedure.\n%s@."
        (protect exn);
      exit 5
  in
  let run_on_error exn =
    try
      on_error exn;
      Log.clean ()
    with exn' ->
      Kernel_log.feedback ~level:0
        "error occurring when handling error: stopping error handling \
         procedure.\n%s@."
        (protect exn');
      exit 6
  in
  let cleanup () =
    (match !error_occurred_ref with
     | None -> run_at_normal_exit ()
     | Some exn ->
       run_on_error exn;
       (* even if an error occurred somewhere, Frama-C stops with
          error code 0. *)
       exit 0;
    );
  in
  let bail_out () =
    cleanup ();
    exit 0;
  in
  bail_out_ref := bail_out;
  try
    f ();
    (* write again on stdout *)
    Log.reset_stdout ~isatty:!tty ();
    cleanup ();
  with
  | Exit ->
    bail_out ()
  | exn when catch_at_toplevel exn ->
    Kernel_log.feedback ~level:0 "%s" (protect exn);
    run_on_error exn;
    exit (exit_code exn)
  | exn ->
    run_on_error exn;
    raise exn

(* ************************************************************************* *)
(** {2 Generic parsing way} *)
(* ************************************************************************* *)

type option_setting =
  | Unit of (unit -> unit)
  | Int of (int -> unit)
  | Float of (float -> unit)
  | String of (string -> unit)

let option_setting_and_warn warn = function
  | Unit f -> Unit (fun () -> warn (); f ())
  | Int f -> Int (fun i -> warn (); f i)
  | Float f -> Float (fun x -> warn (); f x)
  | String f -> String (fun s -> warn (); f s)

let option_setting_and_abort abort = function
  | Unit _ -> Unit (fun _ -> abort ())
  | Int _ -> Int (fun _ -> abort ())
  | Float _ -> Float (fun _ -> abort ())
  | String _ -> String (fun _ -> abort ())

exception Cannot_parse of string * string
let raise_error name because = raise (Cannot_parse(name, because))

let error name msg =
  let bin_name = Filepath.of_string Sys.argv.(0) in
  Kernel_log.abort
    "option `%s' %s.@\nuse `%a -help' for more information."
    name msg Filepath.pretty bin_name

let warning name msg =
  Kernel_log.warning
    "option `%s' %s, ignoring. [-permissive]@\n"
    name msg

let all_options = match Array.to_list Sys.argv with
  | [] -> assert false
  | _binary :: l ->
    let all =
      match Sys.getenv_opt "FRAMAC_DEVONLY_OPTIONS_PRE" with
      | Some s when s <> "" -> String.split_on_char ' ' s @ l
      | _ -> l
    in
    let all =
      match Sys.getenv_opt "FRAMAC_DEVONLY_OPTIONS_POST" with
      | Some s when s <> "" -> all @ String.split_on_char ' ' s
      | _ -> all
    in
    all

let get_option_and_arg option arg =
  try
    let k = String.index option '=' in
    let p = succ k in
    String.sub option 0 k ,
    String.sub option p (String.length option - p) , true
  with Not_found ->
    option, arg, false

type then_argument =
  | Default
  | Last
  | Replace
  | Name of string

let expand_commands_file filename next_options =
  Kernel_log.feedback "Expanding arguments from %s" filename;
  let expanded_args = Arg.read_arg filename in
  (* Merge lines finishing by '\' with the next line and ignore comments. *)
  let r_comment = Str.regexp "^[ \t]*#" in
  let r_backslash = Str.regexp {|\(.*\)\\$|} in
  let expanded_args =
    Array.fold_right
      (fun arg args ->
         if Str.string_match r_comment arg 0 then
           args
         else if Str.string_match r_backslash arg 0 then
           let arg = Str.matched_group 1 arg in
           match args with
           | hd :: tl -> (arg ^ hd) :: tl
           | [] -> [arg]
         else
           arg :: args)
      expanded_args
      []
  in
  (* Regex to match an option and its value regardless of the quoting *)
  let r = Str.regexp {|\([^ =]+\)\([ =]\("\(.*\)"\|\(.*\)\)\)?|} in
  (*                   ^         ^ ^   ^ ^       ^ ^- match an unquoted value
                       |         | |   | |       '- OR
                       |         | |   | '- match a quoted value and discard
                       |         | |   |    quotes
                       |         | |   '- start matching the actual value
                       |         | '- match either a space or an equal to
                       |         |    separate the argument name and its value
                       |         '- optionally start matching the value
                       '- match the argument name (string without a space or an
                          equal) *)
  (* Normalize every argument to '-opt-name' or '-opt-name=opt-value'. *)
  List.fold_right
    (fun arg opts ->
       let arg = String.trim arg in
       if Str.string_match r arg 0 then
         (* The argument name is in the first group, and we know that it is not
            empty since [Str.string_match] returned true. *)
         let opt_name = Str.matched_group 1 arg in
         let opt_value =
           (* The argument value is either in group 4 (quoted) or group 5
              (unquoted). Both can be empty if there is no value for the
              argument. *)
           try Str.matched_group 4 arg with Not_found ->
           try Str.matched_group 5 arg with Not_found -> ""
         in
         let opt_value = String.trim opt_value in
         let opt =
           if opt_value <> "" then
             (* Rebuild the option with the form [opt=arg] so that
                [get_option_and_arg] can see that we already know that
                [opt_value] is the entire value of the option and return
                [explicit=true]. *)
             opt_name ^ "=" ^ opt_value
           else
             opt_name
         in
         opt :: opts
       else
         opts)
    expanded_args
    next_options

let parse known_options_list then_expected options_list =
  let known_options = Hashtbl.create 17 in
  List.iter (fun (n, s) -> Hashtbl.add known_options n s) known_options_list;
  let parse_one_option unknown_options option arg =
    let option, arg, explicit = get_option_and_arg option arg in
    let check_string_argname () =
      if not explicit && (arg = "" || arg.[0] = '-') then
        raise_error option "requires a string as argument";
    in
    try
      let setting = Hashtbl.find known_options option in
      let use_arg = match setting with
        | Unit f ->
          if explicit then raise_error option "does not accept any argument";
          f ();
          false
        | Int f ->
          let n =
            try int_of_string arg
            with Failure _ ->
              raise_error option "requires an integer as argument"
          in
          f n;
          true
        | Float f ->
          let x =
            try float_of_string arg
            with Failure _ ->
              raise_error option "requires a decimal number as argument"
          in
          f x;
          true
        | String f ->
          check_string_argname ();
          f arg;
          true
      in
      unknown_options, use_arg && not explicit, true
    with Not_found ->
      let o = if explicit then option ^ "=" ^ arg else option in
      o :: unknown_options, false, false
  in
  let rec go unknown_options nb_used = function
    | [] -> unknown_options, nb_used, None
    | [ "-then" | "-then-last" | "-then-replace" as then_name ]
      when then_expected ->
      Kernel_log.warning "ignoring last option `%s'." then_name;
      unknown_options, nb_used, None
    | [ "-then-on" ] when then_expected ->
      raise_error "-then-on" "requires a string as argument."
    | [ "-commands-file" ] ->
      raise_error "-commands-file" "requires a string argument."
    | option :: next_options when
        String.starts_with ~prefix:"-commands-file=" option ->
      let option, filename, explicit = get_option_and_arg option "" in
      assert (String.equal option "-commands-file" && explicit);
      parse_commands_file unknown_options nb_used filename next_options
    | option :: then_options when
        String.starts_with ~prefix:"-then-on=" option
        && then_expected ->
      let option, project_name, explicit = get_option_and_arg option "" in
      assert (String.equal option "-then-on" && explicit);
      parse_then_on unknown_options nb_used project_name then_options
    | [ option ] ->
      let unknown, use_arg, is_used =
        parse_one_option unknown_options option ""
      in
      assert (not use_arg);
      unknown, (if is_used then succ nb_used else nb_used), None
    | "-then" :: then_options when then_expected ->
      unknown_options, nb_used, Some (then_options, Default)
    | "-then-last" :: then_options when then_expected ->
      unknown_options, nb_used, Some (then_options, Last)
    | "-then-replace" :: then_options when then_expected ->
      unknown_options, nb_used, Some (then_options, Replace)
    | "-then-on" :: project_name :: then_options when then_expected ->
      parse_then_on unknown_options nb_used project_name then_options
    | "-permissive" :: next_options ->
      permissive := true;
      go unknown_options nb_used next_options
    | "-commands-file" :: filename :: next_options ->
      parse_commands_file unknown_options nb_used filename next_options
    | option :: (arg :: next_options as arg_next) ->
      let unknown, use_arg, is_used =
        parse_one_option unknown_options option arg
      in
      let next = if use_arg then next_options else arg_next in
      go
        unknown
        (if is_used then succ nb_used else nb_used)
        next
  and parse_then_on unknown_options nb_used project_name then_options =
    unknown_options, nb_used, Some (then_options, Name project_name)
  and parse_commands_file unknown_options nb_used filename next_options =
    let next_options = expand_commands_file filename next_options in
    go unknown_options nb_used next_options
  in
  try
    let unknown_options, nb_used, then_options = go [] 0 options_list in
    List.rev unknown_options, nb_used, then_options
  with Cannot_parse(name, msg) ->
    error name msg

(* ************************************************************************* *)
(** {2 First parsing stage at the very beginning of the initialization step} *)
(* ************************************************************************* *)

let non_initial_options_ref = ref []

(* Configure the OCaml Garbage Collector. *)
let configure_ocaml_gc n =
  if n < 1 || n > 10 then
    Kernel_log.warning "Ignoring option -memory-footprint %i, \
                        its argument should be between 1 and 10." n
  else
    let gc_control = Gc.get () in
    let values = [| 24; 30; 40; 60; 90; 120; 150; 190; 240; 300 |] in
    let space_overhead = values.(n-1) in
    if space_overhead <> gc_control.space_overhead then
      Gc.set { gc_control with space_overhead }

let deprecated_deterministic () =
  Kernel_log.warning
    "'-deterministic' is deprecated and does nothing, use environment \
     variable 'FC_DETERMINISTIC=yes' instead."

let () =
  let first_parsing_stage () =
    parse
      [ "-quiet",
        Unit (fun () ->
            quiet_ref := true;
            Verbose_level.set 0;
            Debug_level.set 0);
        "-verbose", Int (fun n -> Verbose_level.set n);
        "-debug", Int (fun n -> Debug_level.set n);
        "-kernel-verbose", Int (fun n -> Kernel_log.Verbose_level.set n);
        "-kernel-debug", Int (fun n -> Kernel_log.Debug_level.set n);
        "-deterministic", Unit (fun () -> deprecated_deterministic ());
        "-tty", Unit (fun () -> set_tty true);
        "-no-tty", Unit (fun () -> set_tty false);
        "-permissive", Unit (fun () -> permissive := true);
        "-memory-footprint", Int configure_ocaml_gc;
        "-compress-saved-session", Unit (fun () ->
            Project.compress_saved_session := true);
        "-no-compress-saved-session", Unit (fun () ->
            Project.compress_saved_session := false)
      ]
      false
      all_options
  in
  (* Only useful for the toplevel version of Frama-C, so that OCaml does not
     try to parse those options. *)
  Arg.current := Array.length Sys.argv;
  catch_toplevel_run
    ~f:(fun () ->
        let remaining_options, _, _ = first_parsing_stage () in
        non_initial_options_ref := remaining_options)
    ~at_normal_exit:(fun () -> ())
    ~on_error:run_error_exit_hook

let quiet = !quiet_ref
let deterministic =
  Option.fold ~none:false ~some:(( = ) "yes")
    (Sys.getenv_opt "FC_DETERMINISTIC")

let tty = !tty
let permissive = !permissive

(* ************************************************************************* *)
(** {2 Plugin} *)
(* ************************************************************************* *)

type cmdline_option =
  { oname: string;
    argname: string;
    mutable ohelp: string;
    ovisible: bool;
    osafe: bool;
    ext_help: (unit,Format.formatter,unit) format;
    mutable setting: option_setting }

module Plugin: sig
  type t = private
    { name: string;
      help: string;
      short: string;
      groups: (string, cmdline_option list ref) Hashtbl.t }
  val all_plugins: unit -> t list
  val all_options: (string, cmdline_option) Hashtbl.t
  val add: ?short:string -> string -> help:string -> unit
  val add_group: ?memo:bool -> plugin:string -> string -> string * bool
  val add_option: string -> group:string -> cmdline_option -> unit
  val add_aliases:
    orig:string -> string -> group:string -> ?visible:bool -> ?deprecated:bool
    -> string list -> cmdline_option list
  val replace_option_setting:
    string -> plugin:string -> group:string -> option_setting -> unit
  val replace_option_help:
    string -> plugin:string -> group:string -> string -> unit
  val find: string -> t
  val find_option_aliases: cmdline_option -> cmdline_option list
  val is_option_alias: cmdline_option -> bool
end = struct

  type t =
    { name: string;
      help: string;
      short: string;
      groups: (string, cmdline_option list ref) Hashtbl.t }

  (* all the registered plug-ins indexed by their shortnames *)
  let plugins : (string, t) Hashtbl.t = Hashtbl.create 17

  (* all the registered options indexed by their name. *)
  let all_options : (string, cmdline_option) Hashtbl.t = Hashtbl.create 97

  let all_plugins () =
    let cmp p1 p2 = String.compare_ignore_case p1.name p2.name in
    List.sort cmp (Hashtbl.fold (fun _ p acc -> p :: acc) plugins [])

  let add ?short name ~help =
    let short = match short with None -> name | Some s -> s in
    if Hashtbl.mem plugins short then
      invalid_arg ("a plug-in " ^ short ^ " is already registered.");
    let groups = Hashtbl.create 7 in
    Hashtbl.add groups "" (ref []);
    Hashtbl.add
      plugins
      short
      { name = name; short = short; help = help; groups = groups }

  let find p =
    try Hashtbl.find plugins p
    with Not_found -> Kernel_log.fatal "Plug-in %s not found" p

  let add_group ?(memo=false) ~plugin name =
    let groups = (find plugin).groups in
    name,
    if Hashtbl.mem groups name then begin
      if not memo then
        Kernel_log.abort
          "A group of name %s already exists for plug-in %s" name plugin;
      false
    end else begin
      Hashtbl.add groups name (ref []);
      true
    end

  let find_group p g =
    try Hashtbl.find (find p).groups g
    with Not_found -> Kernel_log.fatal "Group %s not found for plug-in %s" g p

  module Option_names : sig
    val add: string -> bool -> unit
    val is_option_alias: string -> bool
  end = struct

    let tbl = Hashtbl.create 7

    let check s =
      if Hashtbl.mem tbl s then
        invalid_arg
          (Format.sprintf "an option with the name %S is already registered." s)

    let add s b =
      check s;
      Hashtbl.add tbl s b

    let is_option_alias s =
      try Hashtbl.find tbl s with Not_found -> assert false

  end

  let check_sandbox option =
    (* Prevent unsafe functions from being used in sandbox mode *)
    let sandbox_mode = Sys.getenv_opt "FRAMAC_SANDBOX" |> Option.is_some in
    if sandbox_mode && not option.osafe then
      let abort () =
        Kernel_log.abort
          "%s cannot be used in sandbox mode."
          option.oname
      in
      option.setting <- option_setting_and_abort abort option.setting

  let add_option shortname ~group option =
    assert (option.oname <> "");
    Hashtbl.replace all_options option.oname option;
    Option_names.add option.oname false;
    let g = find_group shortname group in
    check_sandbox option;
    g := option :: !g

  (* table name_of_the_original_option --> aliases *)
  let aliases_tbl = Hashtbl.create 7

  let add_aliases ~orig shortname ~group ?(visible=true) ?(deprecated=false) names =
    (* mostly inline [add_option] and perform additional actions *)
    let options_group = find_group shortname group in
    let option = List.find (fun o -> o.oname = orig) !options_group in
    let get_one name =
      if name = "" then invalid_arg "empty alias name";
      Hashtbl.replace all_options name option;
      Option_names.add name true;
      let setting =
        if deprecated
        then
          let warn () =
            Kernel_log.warning ~once:true
              "@[%s is@ a deprecated alias@ for option %s.@ \
               Please use %s instead.@]"
              name option.oname option.oname
          in
          option_setting_and_warn warn option.setting
        else option.setting
      in
      let alias = { option with oname = name; ovisible = visible; setting; } in
      options_group := alias :: !options_group;
      alias
    in
    let aliases = List.map get_one names in
    (try
       let l = Hashtbl.find aliases_tbl orig in
       l := aliases @ !l;
     with Not_found ->
       Hashtbl.add aliases_tbl orig (ref aliases));
    aliases

  let find_option_aliases o =
    try !(Hashtbl.find aliases_tbl o.oname) with Not_found -> []

  let is_option_alias o = Option_names.is_option_alias o.oname

  (* Applies the function [change] to the option of name [option_name]
     from [plugin] in [group]. *)
  let change_option option_name ~plugin ~group change =
    if option_name <> "" then
      let options_in_group = find_group plugin group in
      let rec replace = function
        | [] ->
          Kernel_log.fatal
            "no option %s in plugin %s ((group of options %s)."
            option_name plugin group
        | o :: _ when o.oname = option_name -> change o
        | _ :: l -> replace l
      in
      replace !options_in_group

  let replace_option_setting option ~plugin ~group setting =
    change_option option ~plugin ~group
      (fun o -> o.setting <- setting;
        check_sandbox o)

  let replace_option_help option ~plugin ~group help =
    change_option option ~plugin ~group (fun o -> o.ohelp <- help)

end

let add_plugin = Plugin.add

module Group = struct
  type t = string
  let add = Plugin.add_group
  let default = ""
  let name x = x
end

(* ************************************************************************* *)
(** {2 Parsing} *)
(* ************************************************************************* *)

module Make_Stage
    (S: sig
       val exclusive: bool
       val name: string
       val then_expected: bool
     end) =
struct

  let nb_actions = ref 0
  let is_going_to_run () = incr nb_actions

  module H = Hook.Make()

  let () = H.extend Log.treat_deferred_error

  let options  : (string, cmdline_option) Hashtbl.t = Hashtbl.create 17

  let add_for_parsing option = Hashtbl.add options option.oname option

  let add name plugin ?(argname="") help visible safe ext_help setting =
    (*    L.debug ~level:4 "Cmdline: [%s] registers %S for stage %s."
          plugin name S.name;*)
    let help = if help = "" then "undocumented" else help in
    let o =
      { oname = name;
        argname = argname;
        ohelp = help;
        ext_help = ext_help;
        ovisible = visible;
        osafe = safe;
        setting = setting }
    in
    add_for_parsing o;
    Plugin.add_option plugin o

  let parse options_list =
    Kernel_log.feedback ~dkey
      "parsing command line options of stage %S."
      S.name;
    let options, nb_used, then_options =
      parse
        (Hashtbl.fold (fun _ o acc -> (o.oname, o.setting) :: acc) options [])
        S.then_expected
        options_list
    in
    let nb_used = nb_used + !nb_actions in
    if S.exclusive && nb_used > 1 then begin
      Kernel_log.abort "at most one %s action must be specified." S.name;
    end;
    H.apply ();
    options, nb_used, then_options

end

module Early_Stage =
  Make_Stage
    (struct
      let exclusive = false
      let name = "early"
      let then_expected = false
    end)

module Extending_Stage =
  Make_Stage
    (struct
      let exclusive = false
      let name = "extending"
      let then_expected = false
    end)

module Extended_Stage =
  Make_Stage
    (struct
      let exclusive = false
      let name = "extended"
      let then_expected = true
    end)

module Exiting_Stage =
  Make_Stage
    (struct
      let exclusive = true
      let name = "exiting"
      let then_expected = false
    end)

module Loading_Stage =
  Make_Stage
    (struct
      let exclusive = true
      let name = "loading"
      let then_expected = false
    end)

let is_going_to_load = Loading_Stage.is_going_to_run

module Configuring_Stage =
  Make_Stage
    (struct
      let exclusive = false
      let name = "configuring"
      let then_expected = false
    end)

let run_after_early_stage = Early_Stage.H.extend
let run_during_extending_stage = Extending_Stage.H.extend
let run_after_extended_stage = Extended_Stage.H.extend
let run_after_exiting_stage = Exiting_Stage.H.extend
let run_after_loading_stage = Loading_Stage.H.extend
let run_after_configuring_stage = Configuring_Stage.H.extend

module After_setting = Hook.Build(struct type t = string list end)
let run_after_setting_files = After_setting.extend

type stage = Early | Extending | Extended | Exiting | Loading | Configuring

let add_option
    name ~plugin ~group stage ?argname ~help ~visible ~safe ~ext_help setting =
  if name <> "" then
    let add = match stage with
      | Early -> Early_Stage.add
      | Extending -> Extending_Stage.add
      | Extended -> Extended_Stage.add
      | Exiting -> Exiting_Stage.add
      | Loading -> Loading_Stage.add
      | Configuring -> Configuring_Stage.add
    in
    add name plugin ~group ?argname help visible safe ext_help setting

let add_option_without_action
    name ~plugin ~group ?(argname="") ~help ~visible ~ext_help () =
  Plugin.add_option
    plugin
    ~group
    { oname = name; argname = argname;
      ohelp = help; ext_help = ext_help; ovisible = visible; osafe = true;
      setting = Unit (fun () -> assert false) }

let add_aliases orig ~plugin ~group ?visible ?deprecated stage aliases =
  let l = Plugin.add_aliases ~orig plugin ~group ?visible ?deprecated aliases in
  let add = match stage with
    | Early -> Early_Stage.add_for_parsing
    | Extending -> Extending_Stage.add_for_parsing
    | Extended -> Extended_Stage.add_for_parsing
    | Exiting -> Exiting_Stage.add_for_parsing
    | Loading -> Loading_Stage.add_for_parsing
    | Configuring -> Configuring_Stage.add_for_parsing
  in
  List.iter add l

let replace_option_setting = Plugin.replace_option_setting
let replace_option_help = Plugin.replace_option_help

module On_Files = Hook.Build(struct type t = Filepath.t list end)
let use_cmdline_files = On_Files.extend

let set_files used_loading l =
  Kernel_log.feedback ~dkey "setting files from command lines.";
  let l =
    List.fold_right
      (fun s acc ->
         if s = "" then
           if permissive then (warning "" "has no name"; acc)
           else error "" "has no name. What do you exactly have in mind?"
         else if s.[0] = '-' then
           (* Option prefixes -I/-D/-U, from the GCC preprocessor, are commonly
              used by mistake; the code below detects them and provides a
              helpful error message. *)
           if String.length s > 1 &&
              (s.[1] == 'D' || s.[1] == 'I' || s.[1] == 'U')
           then
             if permissive then
               (warning s ("is invalid in Frama-C, \
                            use instead: -cpp-extra-args=\"" ^ s ^
                           "\"\n(see option -kernel-h for more information)");
                acc)
             else
               Kernel_log.abort
                 "option `%s' is invalid in Frama-C, \
                  use instead: -cpp-extra-args=\"%s\"@\n\
                  see option -kernel-h for more information."
                 s s
           else
           if permissive then (warning s "is unknown"; acc)
           else error s "is unknown"
         else
           s :: acc
      ) l []
  in
  assert
    (Kernel_log.verify
       (not (On_Files.is_empty ()))
       "no function uses the files provided on the command line");
  if List.length l > 0 then
    if used_loading then
      Kernel_log.warning
        "ignoring source files specified on the command line \
         while loading a global initial context."
    else begin
      On_Files.apply (List.map Filepath.of_string l);
      After_setting.apply l
    end

let nb_used_ref = ref 0
let nb_used_relevant = ref false
let nb_given_options () =
  assert
    (Kernel_log.verify
       !nb_used_relevant "function `nb_given_options' called too early");
  !nb_used_ref

let load_all_plugins = ref (fun () -> assert false)

(** execute one execution shot between 2 "-then*".
    @return the remaining "-then" and the associated options, if any *)
let play_in_toplevel_one_shot nb_used play options =
  let options, nb_used_extended, then_options_extended =
    Extended_Stage.parse options
  in
  let options, nb_used_exiting, then_options_exiting =
    Exiting_Stage.parse options
  in
  assert (then_options_exiting = None);
  if nb_used_exiting > 0 then
    Kernel_log.fatal "setting an option at the exiting stage must stop Frama-C";
  let options, nb_used_loading, then_options_loading =
    Loading_Stage.parse options
  in
  assert (then_options_loading = None);
  let files, nb_used_config, then_options_configuring =
    Configuring_Stage.parse options
  in
  assert (then_options_configuring = None);
  nb_used_relevant := true;
  nb_used_ref :=
    nb_used
    + nb_used_extended
    + nb_used_exiting
    + nb_used_loading
    + nb_used_config ;
  set_files (nb_used_loading > 0) files;
  Kernel_log.feedback ~dkey "running plug-in mains.";
  play ();
  then_options_extended

type project_functions = {
  current: unit -> int;
  on_from_pid: 'a. int -> (unit -> 'a) -> 'a;
  pid_to_name: int -> string;
  name_to_pid: string -> int;
}

let project_functions =
  let current = Project.get_current_pid in
  let on_from_pid pid f =
    try Project.on_from_pid pid f ()
    with Project.Unknown_project ->
      Kernel_log.abort "no project with id `%d'." pid
  in
  let pid_to_name = Project.pid_to_name in
  let name_to_pid name =
    let none () = Kernel_log.abort "no project named `%s'" name in
    Option.value_or_else ~none (Project.name_to_pid name)
  in
  ref { current; on_from_pid; pid_to_name; name_to_pid }

let play_in_toplevel nb_used play options =
  (* [aux then_opts] handles the following "-then" options *)
  let rec aux current = function
    | None -> ()
    | Some(options, then_argument) ->
      let play_on options p =
        p,
        Project.on_from_pid
          p (fun () -> play_in_toplevel_one_shot nb_used play options) ()
      in
      let last_current, then_opts = match then_argument with
        | Default -> current, play_in_toplevel_one_shot nb_used play options
        | Last ->
          (match Project.last_project_created_by_copy () with
           | None -> Kernel_log.abort "no known last created project."
           | Some p -> play_on options p)
        | Replace ->
          (match Project.last_project_created_by_copy () with
           | None -> Kernel_log.abort "no known last created project."
           | Some p ->
             let current = Project.pid_to_name current in
             play_on (("-remove-projects=-@all,+" ^ current) :: options) p)
        | Name p_name ->
          let none () = Kernel_log.abort "no project named `%s'" p_name in
          let pid = Option.value_or_else ~none (Project.name_to_pid p_name) in
          play_on options pid
      in
      aux last_current then_opts
  in
  (* play the first shot before the first "-then" *)
  let then_opts = play_in_toplevel_one_shot nb_used play options in
  (* play the "-then" options *)
  let current = Project.get_current_pid () in
  aux current then_opts

let parse_and_boot ~get_toplevel ~play_analysis =
  let options, nb_used_early, then_options_early =
    Early_Stage.parse !non_initial_options_ref
  in
  assert (then_options_early = None);
  let options, nb_used_extending, then_options_extending =
    Extending_Stage.parse options
  in
  !load_all_plugins ();
  assert (then_options_extending = None);
  get_toplevel
    ()
    (* the extending stage may change the toplevel: applying [get_toplevel]
       provides the good one. *)
    (fun () ->
       play_in_toplevel
         (nb_used_early + nb_used_extending)
         play_analysis
         options)

(* ************************************************************************* *)
(** {2 Help}

    Implement a not very efficient algorithm but it is enough for displaying
    help and exiting. *)
(* ************************************************************************* *)

let print_helpline fmt head help ext_help =
  let n = max 1 (19 - String.length head) in
  Format.fprintf fmt "@[<hov 20>%s%s %t%t@]@\n"
    head
    (* let enough spaces *)
    (String.make n ' ')
    (* the description *)
    (fun fmt -> Format.pp_print_text fmt help)
    (* the extended description *)
    (fun fmt -> Format.fprintf fmt ext_help)

(* Prints option [o], its arguments, and its aliases.
   If [o] is an alias itself, print nothing.
   [print_invisible = true] forces printing invisible options.
   Returns [true] iff something was printed. *)
let low_print_option_help fmt print_invisible o =
  if Plugin.is_option_alias o then begin
    false
  end else
    let ty =
      let s = o.argname in
      if s = "" then
        match o.setting with
        | Unit _ -> ""
        | Int _ -> " <n>"
        | Float _ -> " <x>"
        | String _ -> " <s>"
      else
        " <" ^ s ^ ">"
    in
    let name = o.oname in
    if print_invisible || o.ovisible then begin
      let help =
        if o.osafe then o.ohelp else
          o.ohelp ^ " [unsafe in sandbox mode]"
      in
      print_helpline fmt (name ^ ty) help o.ext_help;
      List.iter
        (fun o ->
           if print_invisible || o.ovisible then
             print_helpline fmt (o.oname ^ ty) ("alias for option " ^ name) "")
        (Plugin.find_option_aliases o)
    end;
    true

let print_option_help fmt ~plugin ~group name =
  let p = Plugin.find plugin in
  let options =
    try Hashtbl.find p.Plugin.groups group
    with Not_found ->
      Kernel_log.fatal "[Cmdline.print_option_help] no group %s" group
  in
  (* linear search... *)
  let rec find_then_print = function
    | [] -> Kernel_log.fatal "[Cmdline.print_option_help] no option %s" name
    | o :: tl ->
      if o.oname = name then ignore (low_print_option_help fmt true o)
      else find_then_print tl
  in
  find_then_print !options

let option_intro short =
  let first =
    if short <> "" then begin
      let short = "-" ^ short in
      Format.sprintf
        "Most options of the form '%s-option-name'@ and without any \
         parameter@ have an opposite with the name '%s-no-option-name'.@\n@\n"
        short short
    end else
      ""
  in
  Format.sprintf
    "%sMost options of the form '-option-name' and without any parameter@ \
     have an opposite with the name '-no-option-name'.@\n@\n\
     Options taking a string as argument should preferably be written@ \
     -option-name=\"argument\"."
    first

(* Sorts command-line options inside a group *)
let sort_cmdline_options =
  List.sort (fun o1 o2 -> String.compare o1.oname o2.oname)

(* Sorts command-line groups inside a plugin *)
let sort_groups groups =
  List.sort
    (fun (s1, _) (s2, _) -> String.compare s1 s2)
    (Hashtbl.fold
       (fun s l acc -> (s, l) :: acc)
       groups [])

let plugin_help shortname =
  let p = Plugin.find shortname in
  if p.Plugin.name <> "" then begin
    assert (p.Plugin.short <> "");
    Log.print_on_output
      (fun fmt ->
         Format.fprintf fmt "@[%s:@ %s@]@\n@[%s:@ %s@]@\n"
           "Plug-in name" p.Plugin.name
           "Plug-in shortname" shortname)
  end;
  Log.print_on_output
    (fun fmt ->
       Format.fprintf fmt
         "@[@[%s:@ %s@]@\n@\n%s@\n@\n%s:@\n@\n@[%t@]@]@?"
         "Description" p.Plugin.help
         (option_intro shortname)
         "***** LIST OF AVAILABLE OPTIONS"
         (fun fmt ->
            let print_options l =
              List.fold_left
                (fun b o ->
                   let b' = low_print_option_help fmt false o in
                   b || b')
                false
                (sort_cmdline_options l)
            in
            match sort_groups p.Plugin.groups with
            | [] -> ()
            | g :: l ->
              let print_group newline (s, o) =
                if newline then Format.pp_print_newline fmt ();
                if s <> "" then
                  Format.fprintf fmt "@[*** %s@]@\n@\n"
                    (String.uppercase_ascii s);
                ignore (print_options !o)
              in
              print_group false g;
              List.iter (print_group true) l));
  raise Exit

let help () =
  Log.print_on_output
    begin fun fmt ->
      Format.fprintf fmt "\nThis is Frama-C %s\n" System_config.Version.id_and_codename ;
      Format.fprintf fmt "\nUsage:\n    %s [options files ...]\n" Sys.argv.(0) ;
      let print_line fmt s =
        Format.(pp_print_string fmt s ; pp_print_newline fmt ()) in
      List.iter (print_line fmt) [
        "" ;
        "Main Options:" ;
        "    -help        This message." ;
        "    -version     Version number only." ;
        "    -plugins     List of installed plugins." ;
        "    -kernel-h    Additional help and options." ;
        "" ;
        "Plug-in Options:" ;
        "    -<plugin>    Plug-in activation." ;
        "    -<plugin>-h  Additional help and options." ;
        "" ;
      ] ;
    end ;
  raise Exit

(** reverse dependency to dynamic.ml *)
let loading_failures = Queue.create ()
let add_loading_failures s = Queue.add s loading_failures
let list_plugins () =
  Log.print_on_output
    begin fun fmt ->
      List.iter
        (fun p ->
           if p.Plugin.name <> "" then
             print_helpline fmt
               (String.capitalize_ascii p.Plugin.name)
               (Printf.sprintf "%s (-%s-h)" p.Plugin.help p.Plugin.short)
               "")
        (Plugin.all_plugins ()) ;
      if not (Queue.is_empty loading_failures) then begin
        Kernel_log.abort
          "@[The following packages failed to load:@ %a@]"
          (Pretty_utils.pp_iter Queue.iter ~sep:",@ " Format.pp_print_string)
          loading_failures;
      end;
    end ;
  raise Exit

(* ************************************************************************* *)
(** {3 Explain}

    Special processing for option "-explain" *)
(* ************************************************************************* *)

let pp_option_help name =
  try
    let option = Hashtbl.find Plugin.all_options name in
    let help = option.ohelp in
    let help = (* Add alias information *)
      if option.oname = name then help else
        "alias for " ^ option.oname ^ "\n" ^ help
    in
    let help = (* Add sandbox mode information *)
      if option.osafe then help else
        help ^ "\nThis function is unsafe in sandbox mode."
    in
    let argname = option.argname in
    let name = if argname = "" then name else name ^ " <" ^ argname ^ ">" in
    let print fmt = print_helpline fmt name help (option.ext_help) in
    Log.print_on_output print
  with Not_found ->
    let print fmt = Format.fprintf fmt "Invalid option %s@." name in
    Log.print_on_output print

(* [option_re] allows matching an option and extracting its name,
   even when there is a '=', e.g. "-kernel-msg-key=-typing".
   It also prevents matching negative numbers, as in "-ulevel -1". *)
let option_re = Str.regexp "-\\([a-zA-Z-][a-zA-Z0-9-]*\\)"
let explain_cmdline () =
  let option_names =
    List.fold_left
      (fun acc option ->
         if Str.string_match option_re option 0 && option <> "-explain"
         then Str.matched_string option :: acc
         else acc)
      [] all_options
  in
  Log.print_on_output
    (fun fmt ->
       Format.fprintf fmt "[kernel] Explaining command-line options:@.");
  List.iter pp_option_help (List.rev option_names);
  raise Exit

(* deprecated *)

module type Level = Log.Level

module Kernel_debug_level = Kernel_log.Debug_level

module Kernel_verbose_level = Kernel_log.Verbose_level

module Kernel_log = Kernel_log
let kernel_debug_atleast_ref =
  Kernel_log.kernel_debug_atleast_ref
[@@alert "-kernel_log"]

let kernel_verbose_atleast_ref =
  Kernel_log.kernel_verbose_atleast_ref
[@@alert "-kernel_log"]

let () = Log.cmdline_at_error_exit := at_error_exit [@@alert "-deprecated"]
let () = Log.cmdline_error_occurred := error_occurred  [@@alert "-deprecated"]

let compress_saved_session = !Project.compress_saved_session

let last_project_created_by_copy = ref Project.last_project_created_by_copy
