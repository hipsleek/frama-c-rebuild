(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module CamlString = String

let empty_string = ""

let session_is_set_ref = Extlib.mk_fun "session_is_set_ref"
let session_ref = Extlib.mk_fun "session_ref"
let cache_is_set_ref = Extlib.mk_fun "cache_is_set_ref"
let cache_ref = Extlib.mk_fun "cache_ref"
let config_is_set_ref = Extlib.mk_fun "config_is_set_ref"
let config_ref = Extlib.mk_fun "config_ref"
let state_is_set_ref = Extlib.mk_fun "state_is_set_ref"
let state_ref = Extlib.mk_fun "state_ref"

(* ************************************************************************* *)
(** {2 Signatures} *)
(* ************************************************************************* *)

module type S_no_log = sig
  val add_group: ?memo:bool -> string -> Cmdline.Group.t
  module Verbose: Parameter_sig.Int
  module Debug: Parameter_sig.Int
  module Message_category: Parameter_sig.String
  module Warn_category: Parameter_sig.String
  module Lib: Parameter_sig.Site_root
  module Share: Parameter_sig.Site_root
  module Session: Parameter_sig.User_dir_opt
  module Cache_dir () : Parameter_sig.User_dir_opt
  module Config_dir () : Parameter_sig.User_dir_opt
  module State_dir () : Parameter_sig.User_dir_opt
  val help: Cmdline.Group.t
  val messages: Cmdline.Group.t
  val grp_debug: Cmdline.Group.t
  val add_plugin_output_aliases:
    ?visible:bool -> ?deprecated:bool -> string list -> unit
end

module type S = sig
  include Log.Messages
  include S_no_log
end

module type General_services = sig
  include S
  include Parameter_sig.Builder
end

(* ************************************************************************* *)
(** {2 Optional parameters of functors} *)
(* ************************************************************************* *)

let kernel = ref false
let kernel_ongoing = ref false

let register_kernel =
  let used = ref false in
  fun () ->
    if !used then
      invalid_arg "The Frama-C kernel should be registered only once."
    else begin
      kernel := true;
      used := true
    end

let is_kernel () = !kernel

let share_visible_ref = ref false
let is_share_visible () = share_visible_ref := true

let session_visible_ref = ref false
let is_session_visible () = session_visible_ref := true

let plugin_subpath_ref = ref None
let plugin_subpath s = plugin_subpath_ref := Some s

let default_verbose_level = ref 1
let set_default_verbose_level n = default_verbose_level := n

let default_msg_keys_ref = ref []

let reset_plugin () =
  kernel := false;
  share_visible_ref := false;
  session_visible_ref := false;
  plugin_subpath_ref := None;
  default_verbose_level := 1;
  default_msg_keys_ref := [];
;;

(* ************************************************************************* *)
(** {2 Generic functors} *)
(* ************************************************************************* *)

let kernel_name = "kernel"

type plugin =
  { p_name: string;
    p_shortname: string;
    p_help: string;
    p_parameters: (string, Typed_parameter.t list) Hashtbl.t }

let plugins: plugin list ref = ref []
let cmp_plugins p1 p2 =
  (* the kernel is the smallest plug-in *)
  match p1.p_name, p2.p_name with
  | s1, s2 when s1 = kernel_name && s2 = kernel_name -> 0
  | s1, _ when s1 = kernel_name -> -1
  | _, s2 when s2 = kernel_name -> 1
  | s1, s2 -> String.compare s1 s2
let iter_on_plugins f =
  List.iter f (List.sort cmp_plugins !plugins)

let fold_on_plugins (f : (plugin -> 'a -> 'a)) (acc : 'a) : 'a =
  List.fold_left (fun acc e -> f e acc) acc (List.sort cmp_plugins !plugins)

let is_present s = List.exists (fun p -> p.p_shortname = s) !plugins
let get_from_name s = List.find (fun p -> p.p_name = s) !plugins
let get_from_shortname s = List.find (fun p -> p.p_shortname = s) !plugins

let check_name s =
  let name_reg = Str.regexp {|^[A-Z]|} in
  if is_kernel () || Str.string_match name_reg s 0 then ()
  else
    let msg = "name '" ^ s ^ "' must start with an uppercase letter" in
    raise (Invalid_argument msg)

let check_shortname s =
  let shortname_reg = Str.regexp {|^[a-z][a-z0-9]*\([-_][a-z0-9]+\)*$|} in
  if s = "kernel" then
    let msg = "shortname \"kernel\" is reserved by Frama-C" in
    raise (Invalid_argument msg)
    (* Kernel's name is the empty string, not "kernel", even if the latter is
       reserved by Frama-C, so we do not want to check its name. *)
  else if is_kernel () || Str.string_match shortname_reg s 0 then ()
  else
    let msg =
      "shortname '" ^ s
      ^ "' must start with a lowercase letter and contain only lowercase"
      ^ " letters and numbers, possibly separated by '-' or '_'"
    in
    raise (Invalid_argument msg)

(* ************************************************************************* *)
(** {2 Global data structures} *)
(* ************************************************************************* *)

(* File formatters used by options [-<plugin>-log]. *)
module File_formatters : sig
  val get : string -> Format.formatter
end =
struct
  (* File formatters must be globally defined so that if a new plugin
     wants to redirect output to an existing file, the same formatter
     must be used to avoid re-opening file descriptors and erasing data.
     E.g. in `frama-c -plugin1-log file.txt -then -plugin2-log file.txt`,
     the formatter avoids Frama-C from opening file.txt a second time, which
     would truncate its contents. *)
  let file_formatters : (Filepath.t, Format.formatter) Hashtbl.t =
    Hashtbl.create 0

  (* Opens and returns a new file formatter if the file has not been opened
     yet, otherwise returns the existing formatter for the file. *)
  let get filename =
    (* Note: normalized paths are not necessarily canonical, so if the
       command-line arguments are unusual, this may fail to detect two
       filenames as referring to the same file. *)
    let normalized_filename = Filepath.of_string filename in
    try
      Hashtbl.find file_formatters normalized_filename
    with
    | Not_found ->
      let oc = open_out (Filepath.to_string_abs normalized_filename) in
      let fmt = Format.formatter_of_out_channel oc in
      Hashtbl.add file_formatters normalized_filename fmt;
      Extlib.safe_at_exit (fun () -> close_out oc);
      fmt
end

(* ************************************************************************* *)
(** {2 The functor [Register]} *)
(* ************************************************************************* *)

module Register
    (P: sig
       val name: string (* the name is "" for the kernel *)
       val shortname: string
       val help: string
     end) =
struct

  let verbose_level = Extlib.mk_fun "verbose_level"
  let debug_level = Extlib.mk_fun "debug_level"

  (* unused by the kernel: it uses Kernel_log instead;
     see module [L] below *)
  module Plugin_log = Log.Register
      (struct
        let channel = P.shortname
        let label = P.shortname
        let debug_atleast level = !debug_level () >= level
        let verbose_atleast level = !verbose_level () >= level
      end)

  (* we can't directly make L a Log, since this would require making
     Plugin.Register a generative functor. Instead, we provide a minimal
     signature for internal usage. It can be extended as needed, provided
     L.category is not exported. *)
  module type Log_skeleton = sig
    val warning: 'a Log.pretty_printer
    val abort: ('a, 'b) Log.pretty_aborter
    val register_and_add: string -> unit
    val add_or_warn: string -> unit
    val del_or_warn: string -> unit
    val set_warn_status: string -> Log.warn_status -> unit
    val is_registered_category: string -> bool
    val pp_all_categories: unit -> unit
    val pp_all_warn_categories_status: unit -> unit
  end

  module Auto_log(L: Log.Messages): Log_skeleton =
  struct
    include L
    let register_and_add s = add_debug_keys (register_category s)

    let warning ?current = let wkey = None in warning ?wkey ?current

    let add_or_warn s =
      match get_category s with
      | Some c -> add_debug_keys c
      | None -> warning "Unknown message key %s" s
    let del_or_warn s =
      match get_category s with
      | Some c -> del_debug_keys c
      | None -> warning "Unknown message key %s" s

    let set_warn_status s status =
      match get_warn_category s with
      | Some c -> set_warn_status c status
      | None -> warning "Unknown warning key %s" s
  end

  module L =
    (val if is_kernel ()
      then (module Auto_log(Kernel_log))
      else (module Auto_log(Plugin_log))
      : Log_skeleton)

  (* Add default message keys to the instance of Log.Messages *)
  let () = List.iter L.register_and_add !default_msg_keys_ref

  let plugin =
    let name = if is_kernel () then kernel_name else P.name in
    let tbl = Hashtbl.create 17 in
    Hashtbl.add tbl empty_string [];
    { p_name = name; p_shortname = P.shortname; p_help = P.help; p_parameters = tbl }

  let add_group ?memo name =
    let parameter_groups = plugin.p_parameters in
    let g, new_g = Cmdline.Group.add ?memo ~plugin:P.shortname name in
    if new_g then Hashtbl.add parameter_groups name [];
    g

  let () =
    (try
       check_name P.name;
       check_shortname P.shortname;
       Cmdline.add_plugin P.name ~short:P.shortname ~help:P.help
     with Invalid_argument s ->
       L.abort "cannot register plug-in `%s': %s" P.name s);
    kernel_ongoing := is_kernel ();
    plugins := plugin :: !plugins

  (* ************************************************************************ *)
  (** {3 Generic options for each plug-in} *)
  (* ************************************************************************ *)

  let messages = add_group "Output Messages"
  let grp_debug = add_group "Debug"

  include Parameter_builder.Make
      (struct
        let shortname = P.shortname
        module L = L
        let parameters = plugin.p_parameters
      end)

  let prefix =
    if P.shortname = empty_string then "-kernel-" else "-" ^ P.shortname ^ "-"

  let plugin_subpath = match !plugin_subpath_ref with
    | None -> P.shortname
    | Some s -> s

  (* ************************************************************************ *)
  (** {3 Specific directories} *)
  (* ************************************************************************ *)

  module Make_site_root
      (D: sig
         val name : string
         val dirs : Fclib.Filepath.t list
         val is_visible : bool
       end)
  =
  struct
    let is_kernel = is_kernel () (* the side effect must be applied right now *)

    let () =
      Parameter_customize.set_cmdline_stage Cmdline.Extended;
      if D.is_visible then Parameter_customize.is_reconfigurable ()
      else Parameter_customize.is_invisible ()

    module Dir_name =
      Filepath
        (struct
          let option_name = prefix ^ D.name
          let arg_name = "dir"
          let help =
            if D.is_visible then
              Format.asprintf
                "set the plug-in %s directory to <dir> (may be used if the \
                 plug-in is not installed at the same place as Frama-C)"
                D.name
            else empty_string
          let existence = Fclib.Filepath.Must_exist
          let file_kind = ""
        end)

    include Dir_name

    let add_plugin path =
      if is_kernel then path
      else Fclib.Filepath.(path / plugin_subpath)

    let dirs () =
      if D.is_visible && is_set () then [ get () ]
      else List.map add_plugin D.dirs

    let find ~is_dir relative =
      let exception Found of Fclib.Filepath.t * Filesystem.file_kind in
      let check_presence dir =
        let path = Fclib.Filepath.(dir / relative) in
        match Filesystem.file_kind path with
        | Error _ -> ()
        | Ok file_kind -> raise (Found (path, file_kind))
      in
      try
        List.iter check_presence (dirs ()) ;
        L.abort
          "Could not find %s %s in Frama-C%s %s"
          (if is_dir then "directory" else "file")
          relative
          (if is_kernel then "" else "/" ^ P.name)
          D.name
      with
      | Found (path, file_kind) when is_dir <> (file_kind = Directory) ->
        L.abort "%a is expected to be a %s"
          Fclib.Filepath.pretty path
          (if is_dir then "directory" else "file")
      | Found (path, _) -> path

    let get_dir = find ~is_dir:true
    let get_file = find ~is_dir:false
  end

  module Share =
    Make_site_root
      (struct
        let name = "share"
        let dirs = System_config.Share.dirs
        let is_visible = !share_visible_ref
      end)

  module Lib =
    Make_site_root
      (struct
        let name = "lib"
        let dirs = System_config.Lib.dirs
        let is_visible = false (* we do not allow lib override *)
      end)

  module Make_user_dir_root
      (D: sig
         val name : string
         val default_root : unit -> Fclib.Filepath.t
         val kernel_get : unit -> Fclib.Filepath.t
         val is_visible : bool
       end)
  =
  struct
    let is_visible = D.is_visible
    let is_kernel = P.name = ""

    let () =
      Parameter_customize.set_cmdline_stage Cmdline.Extended;
      if is_visible then Parameter_customize.is_reconfigurable ()
      else Parameter_customize.is_invisible ()

    let prefix = if is_kernel then "-" else prefix
    let var_name =
      Stdlib.String.uppercase_ascii
        ("FRAMAC_" ^ (if is_kernel then "" else P.shortname ^ "_") ^ D.name)

    module Dir_name =
      Filepath
        (struct
          let option_name = prefix ^ D.name
          let arg_name = "dir"
          let help =
            if is_visible && is_kernel
            then Format.asprintf "set the Frama-C %s directory to <dir>" D.name
            else
            if is_visible
            then Format.asprintf "set the plug-in %s directory to <dir>" D.name
            else empty_string

          let existence = Fclib.Filepath.Indifferent
          let file_kind = ""
        end)

    include Dir_name

    let get () =
      if Dir_name.is_set () then Dir_name.get ()
      else match Sys.getenv_opt var_name with
        | Some s when s <> "" -> Fclib.Filepath.of_string s
        | _ when is_kernel -> D.default_root ()
        | _ -> Fclib.Filepath.(D.kernel_get () / P.shortname)

    let expected ~dir path =
      if dir <> Filesystem.dir_exists path then
        L.abort "%a is expected to be a %s"
          Fclib.Filepath.pretty path (if dir then "directory" else "file")

    let mk_dir d =
      try Filesystem.make_dir d
      with Sys_error _ ->
        L.abort "cannot create %s directory `%a'" D.name Fclib.Filepath.pretty d

    let get_dir ?(create_path=false) s =
      let dir = Fclib.Filepath.(get () / s) in
      if Filesystem.exists dir
      then (expected ~dir:true dir ; dir)
      else if create_path
      then (mk_dir dir ; dir)
      else dir

    let get_file ?create_path s =
      let base_dir = get_dir ?create_path @@ Filename.dirname s in
      (* No need to create anything here, as the path of sub-directories has
         been already created by [get_dir] for computing [base_dir]. *)
      let path = Fclib.Filepath.(base_dir / Filename.basename s) in
      if Filesystem.exists path then expected ~dir:false path ;
      path
  end

  module Session = Make_user_dir_root
      (struct
        let name = "session"
        let default_root () = Fclib.Filepath.of_string "./.frama-c"
        let kernel_get () = !session_ref ()
        let is_visible = !session_visible_ref
      end)

  module Cache_dir () = Make_user_dir_root
      (struct
        let name = "cache"
        let default_root = System_config.User_dirs.cache
        let kernel_get () = !cache_ref ()
        let is_visible = !Parameter_customize.is_visible_ref
      end)

  module Config_dir () = Make_user_dir_root
      (struct
        let name = "config"
        let default_root = System_config.User_dirs.config
        let kernel_get () = !config_ref ()
        let is_visible = !Parameter_customize.is_visible_ref
      end)

  module State_dir () = Make_user_dir_root
      (struct
        let name = "state"
        let default_root = System_config.User_dirs.state
        let kernel_get () = !state_ref ()
        let is_visible = !Parameter_customize.is_visible_ref
      end)

  let help = add_group "Getting Information"

  let () = Parameter_customize.set_group help
  let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
  let () = if is_kernel () then Parameter_customize.set_module_name "Help"
  module Help =
    False
      (struct
        let option_name = prefix ^ "help"
        let help =
          if is_kernel () then "help of the Frama-C kernel"
          else "help of plug-in " ^ P.name
      end)
  let () =
    Cmdline.run_after_exiting_stage
      (fun () ->
         if Help.get () then Cmdline.plugin_help P.shortname else Cmdline.nop);
    Help.add_aliases [ prefix ^ "h" ]

  let output_mode ?(group=messages) modname optname =
    Parameter_customize.set_group group;
    Parameter_customize.do_not_projectify ();
    Parameter_customize.is_reconfigurable ();
    if is_kernel () then begin
      Parameter_customize.set_cmdline_stage Cmdline.Early;
      Parameter_customize.set_module_name modname;
      "-" ^ kernel_name ^ "-" ^ optname
    end else begin
      Parameter_customize.set_cmdline_stage Cmdline.Extended;
      prefix ^ optname
    end

  let logfile_optname = output_mode "LogToFile" "log"
  module LogToFile = struct
    include String_map
        (struct
          include Datatype.String
          let of_string s =
            if s = ""
            then raise (Cannot_build "missing filename")
            else s
          let to_string b = b
        end)
        (struct
          let option_name = logfile_optname
          let arg_name = "K_1:file_1,..."
          let help = "copy log messages from " ^
                     (if is_kernel () then "the Frama-C kernel" else P.name) ^
                     " to a file. <K> is a combination of these characters:\n\
                      a: ALL messages (equivalent to 'dfiruw')\n\
                      d: debug       e: user or internal error (same as 'iu')\n\
                      f: feedback    i: internal error\n\
                      r: result      u: user error    w: warning\n\
                      An empty <K> (e.g. \":file.txt\") defaults to 'iruw'. \
                      One plug-in can output to several files and vice-versa."
          let default = Datatype.String.Map.empty
        end)

    type parse_result = | Parse_OK of Log.kind list
                        | Parse_Error of string (*msg*)

    (* default kinds when none are specified *)
    let default_kinds_str = "erw"

    (* all valid characters for specifying kinds *)
    let valid_kinds_str = "adefiruw"

    (* [parse_kinds str] parses [str] to return a list of [kind]s. *)
    let parse_kinds str =
      if Str.string_match (Str.regexp ("[^" ^ valid_kinds_str ^ "]")) str 0
      then
        Parse_Error
          ("invalid log kind character, must be one of: " ^ valid_kinds_str)
      else
        let str = if str = "" then default_kinds_str else str in
        let has_ch c =
          CamlString.contains str (Char.lowercase_ascii c)
        in
        let list_of_bool b e = if b then [e] else [] in
        let kinds =
          list_of_bool (has_ch 'd' || has_ch 'a') Log.Debug @
          list_of_bool (has_ch 'f' || has_ch 'a') Log.Feedback @
          list_of_bool (has_ch 'i' || has_ch 'a' || has_ch 'e') Log.Failure @
          list_of_bool (has_ch 'r' || has_ch 'a') Log.Result @
          list_of_bool (has_ch 'u' || has_ch 'a' || has_ch 'e') Log.Error @
          list_of_bool (has_ch 'w' || has_ch 'a') Log.Warning
        in
        Parse_OK kinds
  end

  (* Note: because of the imperative nature of Log listeners, and the
     fact that they cannot be removed, whenever the -log option is
     processed again (e.g. after a -then), we must only add new entries
     to the list of listeners, otherwise we will duplicate the output. *)
  (* Also note that this code CANNOT be put inside LogToFile, because it
     uses Datatype. *)
  let add_new_listeners plugin_name old_value new_value =
    let new_entries =
      Datatype.String.Map.filter
        (fun k _ -> not (Datatype.String.Map.mem k old_value)) new_value
    in
    Datatype.String.Map.iter (fun kinds_str filename ->
        match LogToFile.parse_kinds kinds_str with
        | LogToFile.Parse_Error msg -> L.abort "%s" msg
        | LogToFile.Parse_OK kinds ->
          let fmt = File_formatters.get filename in
          Log.add_listener ~plugin:plugin_name ~kind:kinds
            (Log.Event.pretty fmt)
      ) new_entries

  let () =
    LogToFile.add_set_hook
      (add_new_listeners
         (if is_kernel () then kernel_name else P.shortname)
      )

  let verbose_optname = output_mode "Verbose" "verbose"
  module Verbose = struct
    include
      Int(struct
        let default = !default_verbose_level
        let option_name = verbose_optname
        let arg_name = "n"
        let help =
          (if is_kernel () then "level of verbosity for the Frama-C kernel"
           else "level of verbosity for plug-in " ^ P.name)
          ^ " (default to " ^ string_of_int default ^ ")"
      end)

    let get () =
      if is_set () || Option.is_none !Cmdline.Verbose_level.value_if_set
      then get ()
      else Cmdline.Verbose_level.get ()

    let () =
      verbose_level := get;
      (* line order below matters *)
      set_range ~min:0 ~max:max_int;
      if is_kernel () then begin
        Kernel_log.kernel_verbose_atleast_ref := (fun n -> get () >= n);
        begin match !Kernel_log.Verbose_level.value_if_set with
          | None -> ()
          | Some n -> set n
        end;
        add_set_hook (fun _ n -> Kernel_log.Verbose_level.set n);
      end
    [@@alert "-kernel_log"]
  end


  let debug_optname = output_mode ~group:grp_debug "Debug" "debug"
  module Debug = struct
    include
      Int(struct
        let default = 0
        let option_name = debug_optname
        let arg_name = "n"
        let help =
          (if is_kernel () then "level of debug for the Frama-C kernel"
           else "level of debug for plug-in " ^ P.name)
          ^ " (default to " ^ string_of_int default ^ ")"
      end)

    let get () =
      if is_set () || Option.is_none !Cmdline.Debug_level.value_if_set
      then get ()
      else Cmdline.Debug_level.get ()

    let () =
      debug_level := get;
      (* line order below matters *)
      set_range ~min:0 ~max:max_int;
      if is_kernel () then begin
        Kernel_log.kernel_debug_atleast_ref := (fun n -> get () >= n);
        begin match !Kernel_log.Debug_level.value_if_set with
          | None -> ()
          | Some n -> set n
        end;
        add_set_hook (fun _ n -> Kernel_log.Debug_level.set n)
      end
    [@@alert "-kernel_log"]
  end

  let debug_category_optname = output_mode "Msg_key" "msg-key"
  module Message_category =
    Empty_string(struct
      let option_name = debug_category_optname
      let arg_name="k1[,...,kn]"
      let help =
        "enables message display for categories <k1>,...,<kn>. Use "
        ^ debug_category_optname
        ^ " help to get a list of available categories, and * to enable \
           all categories"
    end)

  let parse_category is_kernel _old_s s =
    match Log.parse_category s with
    | Category_help ->
      Cmdline.run_after_exiting_stage
        (fun () -> L.pp_all_categories (); raise Cmdline.Exit)
    | Change_category l ->
      let add_or_del flag c () =
        if flag then L.add_or_warn c else L.del_or_warn c
      in
      let action (to_add, c) =
        (* Allow loaded modules to add categories to the kernel:
           Only categories that exist will be considered in the early
           stage where -kernel-msg-key is running. Of course, if
           none of the loaded modules register the given category,
           a warning will still be emitted. *)
        if is_kernel && not (L.is_registered_category c) then begin
          Cmdline.run_after_extended_stage (add_or_del to_add c)
        end else add_or_del to_add c ()
      in
      List.iter action l

  let () =
    let is_kernel = is_kernel () in
    Message_category.add_set_hook (parse_category is_kernel)

  let warn_category_optname = output_mode "Warn_key" "warn-key"
  module Warn_category =
    Empty_string(struct
      let option_name = warn_category_optname
      let arg_name="k1[=s1][,...,kn[=sn]]"
      let help =
        "set warning status for category <k1> to <s1>,...,<kn> to <sn>. Use "
        ^ warn_category_optname
        ^ " help to get a list of available categories, and * to enable \
           all categories. Possible statuses are inactive, feedback, warning, \
           error, abort, feedback-once, warning-once, error-once. \
           Defaults to warning."
    end)

  let parse_warn_directives is_kernel _old_s s =
    let set_status (warning, status) =
      if is_kernel && not (L.is_registered_category warning) then
        Cmdline.run_after_extended_stage
          (fun () -> L.set_warn_status warning status)
      else
        L.set_warn_status warning status
    in
    match Log.parse_warning s with
    | Parsing_error msg -> L.abort "%s" msg
    | Warning_help ->
      Cmdline.run_after_exiting_stage
        (fun () -> L.pp_all_warn_categories_status (); raise Cmdline.Exit)
    | Set_status l -> List.iter set_status l

  let () =
    let is_kernel = is_kernel () in
    Warn_category.add_set_hook (parse_warn_directives is_kernel)

  let add_plugin_output_aliases ?visible ?deprecated aliases =
    let aliases = List.filter (fun alias -> alias <> "") aliases in
    let optname suffix = List.map (fun alias -> "-" ^ alias ^ suffix) aliases in
    Help.add_aliases ?visible ?deprecated (optname "-help");
    Verbose.add_aliases ?visible ?deprecated (optname "-verbose");
    Message_category.add_aliases ?visible ?deprecated (optname "-msg-key");
    Warn_category.add_aliases ?visible ?deprecated (optname "-warn-key");
    LogToFile.add_aliases ?visible ?deprecated (optname "-log")

  let () = reset_plugin ()

  include Plugin_log

end (* Register *)

(* -------------------------------------------------------------------------- *)
(* --- Tests                                                              --- *)
(* -------------------------------------------------------------------------- *)

let _test_valid_name f s =
  try f s; true
  with Invalid_argument _ -> false

let _test_wrong_name f s =
  try f s; false
  with Invalid_argument _ -> true

let%test _ = _test_valid_name check_name "A"
let%test _ = _test_valid_name check_name "AbC"
let%test _ = _test_valid_name check_name "E-ACSL"
let%test _ = _test_valid_name check_name "A long plug_in Name"
let%test _ = _test_valid_name check_name "Jessie3"

let%test _ = _test_valid_name check_shortname "a"
let%test _ = _test_valid_name check_shortname "abc"
let%test _ = _test_valid_name check_shortname "e-acsl"
let%test _ = _test_valid_name check_shortname "a_long_plug-in_shortname"
let%test _ = _test_valid_name check_shortname "jessie3"

let _test_kernel_name f =
  kernel := true;
  let success = _test_valid_name f "" in
  kernel := false;
  success

let%test _ = _test_kernel_name check_name
let%test _ = _test_kernel_name check_shortname

let%test _ = _test_wrong_name check_name ""
let%test _ = _test_wrong_name check_name "-"
let%test _ = _test_wrong_name check_name "_"
let%test _ = _test_wrong_name check_name "-Abc"
let%test _ = _test_wrong_name check_name "3Jessie"
let%test _ = _test_wrong_name check_name "minuscule"

let%test _ = _test_wrong_name check_shortname ""
let%test _ = _test_wrong_name check_shortname "-"
let%test _ = _test_wrong_name check_shortname "_"
let%test _ = _test_wrong_name check_shortname "_abc"
let%test _ = _test_wrong_name check_shortname "abc-"
let%test _ = _test_wrong_name check_shortname "a-_-a"
let%test _ = _test_wrong_name check_shortname "kernel"
let%test _ = _test_wrong_name check_shortname "3jessie"
let%test _ = _test_wrong_name check_shortname "Capital"
