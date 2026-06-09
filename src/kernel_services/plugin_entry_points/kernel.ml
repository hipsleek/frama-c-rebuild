(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* ************************************************************************* *)
(** {2 Kernel as an almost standard plug-in} *)
(* ************************************************************************* *)

let () = Plugin.register_kernel ()

let () = Plugin.is_session_visible ()
module P = Plugin.Register
    (struct
      let name = ""
      let shortname = ""
      let help = "General options provided by the Frama-C kernel"
    end)

include (P: Plugin.S_no_log)
include Kernel_log

(* ************************************************************************* *)
(** {2 Fclib debug options} *)
(* ************************************************************************* *)

(* Link Kernel_log dkeys related to Fclib to the corresponding libraries
   options *)

let is_enabled_debug ?(level=1) key =
  is_debug_key_enabled key && debug_atleast level
let is_enabled_verbose ?(level=1) key =
  is_debug_key_enabled key && verbose_atleast level

let should_warn_error wkey =
  match get_warn_status wkey with
  | Wfeedback | Wfeedback_once when verbose_atleast 1 -> 1
  | Wfeedback | Wfeedback_once | Winactive -> 0
  | Wactive | Wonce -> 2
  | Werror | Werror_once | Wabort -> 3

let set_fclib_debug _ _ =
  Task.set_debug (is_enabled_debug dkey_task);
  Hptmap.set_debug (is_enabled_debug dkey_hptmap);
  Project.set_debug (is_enabled_debug dkey_project);
  Project.set_feedback (is_enabled_verbose dkey_project);
  State_builder.set_debug (is_enabled_debug ~level:4 dkey_project)

let set_fclib_warn _ _ =
  Project.set_warn_level (should_warn_error wkey_project)

let () = Message_category.add_update_hook set_fclib_debug
let () = Warn_category.add_update_hook set_fclib_warn

(* ************************************************************************* *)
(** {2 Specialised functors for building kernel parameters} *)
(* ************************************************************************* *)

module type Input = sig
  include Parameter_sig.Input
  val module_name: string
end

module type Input_with_arg = sig
  include Parameter_sig.Input_with_arg
  val module_name: string
end

module Bool(X:sig include Input val default: bool end) =
  P.Bool
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module False(X: Input) =
  P.False
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module True(X: Input) =
  P.True
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module Int (X: sig val default: int include Input_with_arg end) =
  P.Int
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module Zero(X:Input_with_arg) =
  P.Zero
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module String
    (X: sig include Input_with_arg val default: string end) =
  P.String
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module String_list(X: Input_with_arg) =
  P.String_list
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module String_map (X: Input_with_arg) =
  P.String_map
    (P.Value_string)
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
      let default = Datatype.String.Map.empty
    end)

module String_multiple_map
    (V: Parameter_sig.Value_datatype)
    (X: Input_with_arg) =
  P.String_multiple_map
    (V)
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
      let default = Datatype.String.Map.empty
    end)

module Filepath_list
    (X: sig
       include Input_with_arg
       val existence: Filepath.existence
       val file_kind: string
     end) =
  P.Filepath_list
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module Filepath_map
    (X: sig
       include Input_with_arg
       val existence: Filepath.existence
       val file_kind: string
     end) =
  P.Filepath_map
    (P.Value_string)
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
      let default = Filepath.Map.empty
    end)

module Kernel_function_set(X: Input_with_arg) =
  P.Kernel_function_set
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

module Enum
    (X: sig
       type t
       val default: t
       val values: (t * string) list
       include Input end
    ) =
  P.Enum
    (struct
      let () = Parameter_customize.set_module_name X.module_name
      include X
    end)

(* ************************************************************************* *)
(** {2 Installation Information} *)
(* ************************************************************************* *)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
let () = Parameter_customize.set_negative_option_name ""
module GeneralHelp =
  False
    (struct
      let option_name = "--help"
      let help = "display a general help"
      let module_name = "GeneralHelp"
    end)
let run_help () = if GeneralHelp.get () then Cmdline.help () else Cmdline.nop
let () = Cmdline.run_after_exiting_stage run_help
let () = GeneralHelp.add_aliases [ "-h"; "-help"]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
let () = Parameter_customize.set_negative_option_name ""
module ListPlugins =
  False
    (struct
      let option_name = "--list-plugins"
      let help = "display a general help"
      let module_name = "ListPlugins"
    end)
let run_list_plugins () =
  if ListPlugins.get () then Cmdline.list_plugins () else Cmdline.nop
let () = Cmdline.run_after_exiting_stage run_list_plugins
let () = ListPlugins.add_aliases ["-plugins"; "--plugins"]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintConfig =
  False
    (struct
      let option_name = "-print-config"
      let module_name = "PrintConfig"
      let help = "print full config information"
    end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module Version =
  False(struct
    let option_name = "-version"
    let module_name = "Version"
    let help = "print the Frama-C version"
  end)
let () = Version.add_aliases [ "-v"; "--version" ]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintVersion =
  False(struct
    let option_name = "-print-version"
    let module_name = "PrintVersion"
    let help = "print the Frama-C version"
  end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintShare =
  False(struct
    let option_name = "-print-share-path"
    let module_name = "PrintShare"
    let help = "print the Frama-C share path"
  end)
let () = PrintShare.add_aliases [ "-print-path" ]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintLib =
  False(struct
    let option_name = "-print-lib-path"
    let module_name = "PrintLib"
    let help = "print the path of the Frama-C kernel library"
  end)
let () = PrintLib.add_aliases [ "-print-libpath" ]

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.set_negative_option_name ""
module PrintPluginPath =
  False
    (struct
      let option_name = "-print-plugin-path"
      let module_name = "PrintPluginPath"
      let help =
        "print the path where the Frama-C dynamic plug-ins are searched into"
    end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
let () = Parameter_customize.set_negative_option_name ""
module PrintMachdep =
  False
    (struct
      let module_name = "PrintMachdep"
      let option_name = "-print-machdep"
      let help = "pretty print selected machdep"
    end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
let () = Parameter_customize.set_negative_option_name ""
module PrintMachdepHeader =
  False
    (struct
      let module_name = "PrintMachdepHeader"
      let option_name = "-print-machdep-header"
      let help =
        "print on standard output the content of the generated __fc_machdep.h"
    end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
let () = Parameter_customize.set_negative_option_name ""
module PrintMachdepBuiltinMacros =
  False
    (struct
      let module_name = "PrintMachdepBuiltinMacros"
      let option_name = "-print-machdep-builtin-macros"
      let help =
        "print on standard output the content of the generated __fc_builtin_macros.h"
    end)

let () = Parameter_customize.set_group grp_debug
let () = Parameter_customize.set_negative_option_name ""
module DumpDependencies =
  P.Filepath
    (struct
      let option_name = "-dump-dependencies"
      let help = ""
      let arg_name = ""
      let existence = Filepath.Indifferent
      let file_kind = "Text"
    end)
let () =
  Extlib.safe_at_exit
    (fun () ->
       if not (DumpDependencies.is_default ()) then
         State_dependency_graph.dump (DumpDependencies.get ()))

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
module PrintConfigJson =
  False
    (struct
      let module_name = "PrintConfigJson"
      let option_name = "-print-config-json"
      let help = "prints extensive data about Frama-C's configuration, in \
                  JSON format, and exits (experimental: the output format \
                  is very likely to change in future versions)."
    end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Exiting
let () = Parameter_customize.set_negative_option_name ""
module AutocompleteHelp =
  P.String_set
    (struct
      let option_name = "-autocomplete"
      let arg_name = "p1,p2,..."
      let help = "displays all Frama-C options, used for shell autocompletion. \
                  Prints options for the specified plugin names (or '@all' for \
                  all plugins)."
    end)

let _ =
  AutocompleteHelp.Category.enable_all
    []
    (object
      method fold: 'a. (string -> 'a -> 'a) -> 'a -> 'a =
        fun f acc ->
        Plugin.fold_on_plugins (fun p acc -> f p.Plugin.p_shortname acc) acc
      method mem name =
        try
          if name = "kernel" then raise Exit;
          Plugin.iter_on_plugins
            (fun p -> if name = p.Plugin.p_shortname then raise Exit);
          false
        with Exit -> true
    end)

let () = Parameter_customize.set_group help
let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.set_negative_option_name ""
module Explain =
  False
    (struct
      let option_name = "-explain"
      let help = "prints the help message for each option given in the \
                  command line"
      let module_name = "Explain"
    end)

let () =
  Cmdline.run_after_exiting_stage (fun () ->
      if Explain.get () then Cmdline.explain_cmdline ()
      else Cmdline.nop)
(* This option is processed in a special manner in [Cmdline].
   Nothing to be done here. *)

(* ************************************************************************* *)
(** {2 Output Messages} *)
(* ************************************************************************* *)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.is_reconfigurable ()
module GeneralVerbose =
  Int
    (struct
      let default = 1
      let option_name = "-verbose"
      let arg_name = "n"
      let help = "general level of verbosity"
      let module_name = "GeneralVerbose"
    end)
let () =
  GeneralVerbose.set_range ~min:0 ~max:max_int;
  match !Cmdline.Verbose_level.value_if_set with
  | None -> ()
  | Some n -> GeneralVerbose.set n
let () =
  (* Add the hook after setting it from Cmdline to avoid setting it twice. *)
  GeneralVerbose.add_set_hook (fun _ n -> Cmdline.Verbose_level.set n)

let () = Parameter_customize.set_group grp_debug
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.is_reconfigurable ()
module GeneralDebug =
  Zero
    (struct
      let option_name = "-debug"
      let arg_name = "n"
      let help = "general level of debug"
      let module_name = "GeneralDebug"
    end)
let () =
  GeneralDebug.set_range ~min:0 ~max:max_int;
  match !Cmdline.Debug_level.value_if_set with
  | None -> ()
  | Some n -> GeneralDebug.set n
let () =
  (* Add the hook after setting it from Cmdline to avoid setting it twice. *)
  GeneralDebug.add_set_hook (fun _ n -> Cmdline.Debug_level.set n)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.set_negative_option_name ""
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.is_reconfigurable ()
let () = Parameter_customize.do_not_projectify ()
module Quiet =
  Bool
    (struct
      let default = Cmdline.quiet
      let option_name = "-quiet"
      let module_name = "Quiet"
      let help = "sets -verbose and -debug to 0"
    end)
let () =
  Quiet.add_set_hook
    (fun _ b -> assert b; GeneralVerbose.set 0; GeneralDebug.set 0)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.set_cmdline_stage Cmdline.Extended
let () = Parameter_customize.do_not_projectify ()
module Unicode = struct
  include True
      (struct
        let option_name = "-unicode"
        let module_name = "Unicode"
        let help = "use utf8 in messages"
      end)
  (* This function behaves nicely with the Gui, that detects if command-line
     arguments have been set by the user at some point. *)
  let without_unicode f arg =
    let old, default = get (), not (is_set ()) in
    off ();
    let r = f arg in
    if default then clear () else set old;
    r
end
let () = Unicode.add_update_hook (fun _ curr -> Fclib.Unicode.use_unicode curr)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
module TTY =
  Bool
    (struct
      let option_name = "-tty"
      let module_name = "TTY"
      let default = Cmdline.tty
      let help = "force the use of terminal capabilities for feedback; \
                  use the opposite option for completely disabling the use of \
                  terminal capabilities"
    end)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
let () = Parameter_customize.is_invisible ()
module TTY_debug =
  Bool
    (struct
      let option_name = "-tty-debug"
      let module_name = "TTY_debug"
      let default = false
      let help = "print semantic tags that are not handled by the TTY (for plug-in developers)"
    end)

let () =
  Cmdline.run_after_early_stage
    begin fun () ->
      if TTY.get () then
        let fallback = TTY_debug.get () in
        let _reset = Ansi_escape.enable_on ~fallback Format.std_formatter in ()
    end

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
module Time =
  P.Empty_string
    (struct
      let option_name = "-time"
      let arg_name = "filename"
      let help = "append process time and timestamp to <filename> at exit"
    end)

let () = Parameter_customize.set_group messages
let () = Parameter_customize.do_not_projectify ()
module SymbolicPath =
  Filepath_map
    (struct
      let option_name = "-add-symbolic-path"
      let module_name = "SymbolicPath"
      let arg_name = "path_1:name_1,...,path_n:name_n"
      let existence = Filepath.Indifferent
      let file_kind = "directory"
      let help =
        "When displaying file locations, replace (absolute) path with the \
         corresponding symbolic name"
    end)

let () =
  SymbolicPath.add_update_hook
    (fun prev curr ->
       (* keep module [Filepath] synchronized with [SymbolicPath] *)
       Filepath.Map.iter (fun f _ -> Filepath.remove_symbolic_dir f) prev;
       Filepath.Map.iter (fun f n -> Filepath.add_symbolic_dir n f) curr)

(* [SymbolicPath] is better to be not projectified,
   but must be saved: use a fake state for saving it without projectifying it *)
module SymbolicPathFakeState =
  State_builder.Register
    (Datatype.Unit)
    (struct
      type t = unit
      let create () = ()
      let clear () = ()
      let get () = ()
      let set () = ()
      let clear_some_projects _f () = false
    end)
    (struct
      let name = "SymbolicPathFakeState"
      let unique_name = name
      let dependencies = []
    end)

let () =
  SymbolicPathFakeState.howto_marshal
    (fun () -> SymbolicPath.get ())
    (fun paths -> SymbolicPath.set paths)

(* ************************************************************************* *)
(** {2 Input / Output Source Code} *)
(* ************************************************************************* *)

let inout_source = add_group "Input/Output Source Code"

let () = Parameter_customize.set_group inout_source
module PrintCode =
  False
    (struct
      let module_name = "PrintCode"
      let option_name = "-print"
      let help = "pretty print C code"
    end)

let () = Parameter_customize.set_group grp_debug
let () = Parameter_customize.do_not_projectify ()
module PrintAsIs =
  False
    (struct
      let module_name = "PrintAsIs"
      let option_name = "-print-as-is"
      let help = "when pretty-printing C code, try to print it as close as \
                  possible to the internal (Cil) representation"
    end)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module PrintComments =
  False
    (struct
      let module_name = "PrintComments"
      let option_name = "-keep-comments"
      let help = "try to keep comments in C code"
    end)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module PrintLibc =
  Bool
    (struct
      let module_name = "PrintLibc"
      let option_name = "-print-libc"
      let help = "when pretty-printing C code, keep prototypes coming \
                  from Frama-C standard library"
      let default = false
    end)

let () = Parameter_customize.set_group inout_source
module PrintReturn =
  False
    (struct
      let module_name = "PrintReturn"
      let option_name = "-print-return"
      let help = "inline gotos to return statement"
    end)

module CodeOutput = struct

  let () = Parameter_customize.set_group inout_source
  include P.Filepath
      (struct
        let option_name = "-ocode"
        let arg_name = "filename"
        let existence = Filepath.Indifferent
        let file_kind = "source"
        let help =
          "when printing code, redirects the output to file <filename>"
      end)

  let streams = Hashtbl.create 7

  let output job =
    let file = get () in
    if Filepath.(is_special_stdout file || is_empty file)
    then Log.print_delayed job
    else
      try
        let fmt =
          try fst (Hashtbl.find streams file)
          with Not_found ->
            let out = open_out (Filepath.to_string_abs file) in
            let fmt = Format.formatter_of_out_channel out in
            Hashtbl.add streams file (fmt,out) ; fmt
        in
        job fmt
      with Sys_error s ->
        warning
          "Fail to open file \"%a\" for code output@\nSystem error: %s.@\n\
           Code is output on stdout instead."
          Filepath.pretty file s ;
        Log.print_delayed job

  let close_all () =
    Hashtbl.iter
      (fun file (fmt,cout) ->
         try
           Format.pp_print_flush fmt () ;
           close_out cout ;
         with Sys_error s ->
           failure
             "Fail to close output file \"%a\"@\nSystem error: %s."
             Filepath.pretty file s)
      streams

  let () = Extlib.safe_at_exit close_all

end

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module FloatPrint =
  Enum
    (struct
      type t = Floating_point.float_display
      let module_name = "FloatPrint"
      let option_name = "-float-print"
      let default = Floating_point.Default
      let values = [
        (Floating_point.Default, "default");
        (Floating_point.NormDec, "norm");
        (Floating_point.NormHex, "hex");
      ]
      let help =
        "Control how floats will be printed : 'default' will print the float \
         as is, 'norm' will use an internal routine to normalize it using \
         decimals, and 'hex' is the same than 'norm' but in hexadecimal. \
         Default value is 'default'"
    end)
let () = FloatPrint.add_update_hook (fun _ -> Floating_point.set_float_display)


let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module BigIntsHex =
  Int(struct
    let module_name = "BigIntsHex"
    let option_name = "-big-ints-hex"
    let arg_name = "max"
    let help = "display integers larger than <max> using hexadecimal notation. \
                A negative value disable this option. Disabled by default."
    let default = -1
  end)
let () = BigIntsHex.add_update_hook (fun _ i -> Z.set_big_ints_hex i)


let () = Parameter_customize.set_group inout_source
module EagerLoadSources =
  False(struct
    let module_name = "EagerLoadSources"
    let option_name = "-eager-load-sources"
    let help = "when loading a source, try to load all referenced sources \
                in memory"
  end)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
module AstDiff =
  False
    (struct
      let option_name = "-ast-diff"
      let module_name = "AstDiff"
      let help = "creates a new project and computes a diff of the AST \
                  from the current one"
    end)

let () = Parameter_customize.set_group grp_debug
module DumpInterpretedAutomata =
  False
    (struct
      let option_name = "-dump-interpreted-automata"
      let module_name = "DumpInterpretedAutomata"
      let help = "dumps each interpreted automata built into a dot file"
    end)

(* ************************************************************************* *)
(** {2 Save/Load} *)
(* ************************************************************************* *)

let saveload = add_group "Saving or Loading Data"

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.do_not_projectify ()
module SaveState =
  P.Filepath
    (struct
      let option_name = "-save"
      let arg_name = "filename"
      let existence = Filepath.Indifferent
      let file_kind = "Frama-C state"
      let help = "at exit, save the session into file <filename>"
    end)

let () =
  Cmdline.add_option_without_action
    "-compress-saved-session"
    ~plugin:""
    ~group:saveload
    ~help:"at exit, the session saved by the -save option is compressed. \
           No effect if the -save option is not used. \
           (set by default, opposite option is -no-compress-saved-session)"
    ~visible:true
    ~ext_help:""
    ()

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.set_cmdline_stage Cmdline.Loading
(* must be projectified: when loading, this option will be automatically
   reset *)
(*let () = Parameter_customize.do_not_projectify ()*)
module LoadState =
  P.Filepath
    (struct
      let option_name = "-load"
      let arg_name = "filename"
      let existence = Filepath.Must_exist
      let file_kind = "Frama-C state"
      let help = "load a previously-saved session from file <filename>"
    end)

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.is_unsafe ()
module LoadModule =
  String_list
    (struct
      let option_name = "-load-module"
      let module_name = "LoadModule"
      let arg_name = "SPEC,..."
      let help = "Dynamically load modules. \
                  Each <SPEC> can be an object file, with \
                  or without extension, or a Findlib package. \
                  Loading order is preserved, but after plugins and libraries."
    end)

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.is_unsafe ()
module LoadLibrary =
  String_list
    (struct
      let option_name = "-load-library"
      let module_name = "LoadLibrary"
      let arg_name = "libname,..."
      let help = "Dynamically load libraries. \
                  Loading order is preserved. Libraries are loaded between \
                  plugins and modules."
    end)

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.is_unsafe ()
module LoadPlugin =
  String_list
    (struct
      let option_name = "-load-plugin"
      let module_name = "LoadPlugin"
      let arg_name = "plugin,..."
      let help = "Dynamically load plugins. \
                  Loading order is preserved. Plugins are loaded before \
                  libraries and modules."
    end)

let () = Parameter_customize.set_group saveload
let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.do_not_projectify ()
module AutoLoadPlugins =
  True
    (struct
      let option_name = "-autoload-plugins"
      let module_name = "AutoLoadPlugins"
      let help = "Automatically load all plugins in FRAMAC_PLUGIN."
    end)

let bootstrap_loader () =
  begin
    if AutoLoadPlugins.get () then Dynamic.load_plugin_path () ;
    List.iter Dynamic.load_plugin (LoadPlugin.get()) ;
    Dynamic.load_packages (LoadLibrary.get()) ;
    List.iter Dynamic.load_module (LoadModule.get()) ;
  end

let () = Cmdline.load_all_plugins := bootstrap_loader

let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.set_group saveload
let () = Parameter_customize.do_not_projectify ()
module Session_dir = Session
let () = Plugin.session_is_set_ref := Session_dir.is_set
let () = Plugin.session_ref := Session_dir.get

let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.set_group saveload
let () = Parameter_customize.do_not_projectify ()
module Cache_dir = Cache_dir ()
let () = Plugin.cache_is_set_ref := Cache_dir.is_set
let () = Plugin.cache_ref := Cache_dir.get

let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.set_group saveload
let () = Parameter_customize.do_not_projectify ()
module Config_dir = Config_dir ()
let () = Plugin.config_is_set_ref := Config_dir.is_set
let () = Plugin.config_ref := Config_dir.get

let () = Parameter_customize.set_cmdline_stage Cmdline.Extending
let () = Parameter_customize.set_group saveload
let () = Parameter_customize.do_not_projectify ()
module State_dir = State_dir ()
let () = Plugin.state_is_set_ref := State_dir.is_set
let () = Plugin.state_ref := State_dir.get

(* ************************************************************************* *)
(** {2 Parsing} *)
(* ************************************************************************* *)

let parsing = add_group "Parsing"

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Extended
module Machdep = struct
  include String
      (struct
        let module_name = "Machdep"
        let option_name = "-machdep"
        let default =
          try Sys.getenv "FRAMAC_MACHDEP"
          with Not_found -> "x86_64"
        let arg_name = "machine"
        let help =
          "use <machine> as the current machine dependent configuration. \
           See \"-machdep help\" for a list. The environment variable \
           FRAMAC_MACHDEP can be used to override the default value. The command \
           line parameter still has priority over the default value"
      end)

  let get_dir () = Share.get_dir "machdeps"
  let get_default_file machdep =
    let filename = "machdep_" ^ machdep ^ ".yaml" in
    Filepath.(get_dir () / filename)
  let is_default machdep =
    Filesystem.file_exists (get_default_file machdep)

  let normalize machdep =
    if machdep = "help" || is_default machdep then
      machdep
    else
      Filepath.(of_string machdep |> to_string_abs)

  let () =
    let set_if_necessary old_machdep new_machdep =
      let new_machdep = normalize new_machdep in
      if not (equal old_machdep new_machdep) then unsafe_set new_machdep
    in
    add_set_hook set_if_necessary
end

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module ReadAnnot =
  True(struct
    let module_name = "ReadAnnot"
    let option_name = "-annot"
    let help = "read and parse annotations"
  end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module PreprocessAnnot =
  False(struct
    let module_name = "PreprocessAnnot"
    let option_name = "-pp-annot"
    let help =
      "preprocess annotations (if they are read). Set by default if \
       the preprocessor is GNU-like (see option -cpp-frama-c-compliant)"
  end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
let () = Parameter_customize.is_unsafe ()
module CppCommand =
  P.Empty_string
    (struct
      let option_name = "-cpp-command"
      let arg_name = "cmd"
      let help = "<cmd> is used to build the preprocessing command.\n\
                  Default to $CPP environment variable or else \"gcc -C -E -I.\".\n\
                  If unset, the command is built as follows:\n\
                  CPP -o <preprocessed file> <source file>\n\
                  %1 and %2 can be used into CPP string to mark the position of <source file> \
                  and <preprocessed file> respectively"
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
let () = Parameter_customize.no_category ()
let () = Parameter_customize.is_unsafe ()
module CppExtraArgs =
  String_list
    (struct
      let module_name = "CppExtraArgs"
      let option_name = "-cpp-extra-args"
      let arg_name = "args"
      let help = "additional arguments passed to the preprocessor \
                  (mainly -D and -I) while preprocessing the C code \
                  but not while preprocessing annotations"
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
let () = Parameter_customize.is_unsafe ()
module CppExtraArgsPerFile =
  Filepath_map
    (struct
      let module_name = "CppExtraArgsPerFile"
      let option_name = "-cpp-extra-args-per-file"
      let arg_name = "file:flags"
      let existence = Filepath.Must_exist
      let file_kind = "source"
      let help =
        "when set, adds preprocessing arguments for each specified file. \
         To add arguments for all files, use -cpp-extra-args."
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module CppGnuLike =
  True
    (struct
      let module_name = "CppGnuLike"
      let option_name = "-cpp-frama-c-compliant"
      let help =
        "indicates that a custom preprocessor (see option -cpp-command) \
         accepts the same set of options as GNU cpp. Set it to false if you \
         have preprocessing issues with a custom preprocessor."
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module PrintCppCommands =
  False
    (struct
      let module_name = "PrintCppCommands"
      let option_name = "-print-cpp-commands"
      let help = "prints the preprocessing command(s) used by Frama-C \
                  and exits."
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module AuditPrepare =
  P.Filepath
    (struct
      let option_name = "-audit-prepare"
      let arg_name = "path"
      let existence = Filepath.Indifferent
      let file_kind = "json"
      let help = "produces audit-related information, such as the list of all \
                  source files used during parsing (including those in include \
                  directives) with checksums. Some plug-ins may produce \
                  additional audit-related information. \
                  Prints the information as JSON to the specified file, or \
                  if the file is '-', prints as text to the standard output. \
                  Requires -cpp-frama-c-compliant."
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module AuditCheck =
  P.Filepath
    (struct
      let option_name = "-audit-check"
      let arg_name = "path"
      let existence = Filepath.Must_exist
      let file_kind = "json"
      let help = "reads an audit JSON file (produced by -audit-prepare) and \
                  checks compliance w.r.t. it; e.g., if the source files \
                  were declared and have the expected checksum. \
                  Raises a warning (with warning key 'audit') in case of \
                  failed checks. \
                  Requires -cpp-frama-c-compliant."
    end)

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module FramaCStdLib =
  True
    (struct
      let module_name = "FramaCStdLib"
      let option_name = "-frama-c-stdlib"
      let help =
        "adds -I$FRAMAC_SHARE/libc to the options given to the cpp command. \
         If -cpp-frama-c-compliant is not false, also adds -nostdinc to prevent \
         inconsistent mix of system and Frama-C header files"
    end)

let () = Parameter_customize.set_group grp_debug
module Orig_name =
  False(struct
    let option_name = "-orig-name"
    let module_name = "Orig_name"
    let help = "prints a message each time a variable is renamed"
  end)

type iso_c = C11 | C17 | C23 | C2y

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module CStd =
  P.Enum
    (struct
      type t = iso_c
      let default = C11
      let option_name = "-std"
      let help =
        "Configures the ISO standard to use. Note that your preprocessor must \
         support it, else it will lead to preprocessing failure."
      let values = [ C11, "c11" ; C17, "c17" ; C23, "c23"  ; C2y, "c2y"]
    end)

type attr_info =
    Default | Class of string | Print of bool | Ignore of bool

module AttributeInfo = struct
  include Datatype.Make (struct
      include Datatype.Serializable_undefined
      type t = attr_info
      let name = "Kernel.AttributeInfo"
      let reprs = [ Default; Class "unknown"; Print true; Ignore false ]
      let compare = Stdlib.compare
      let equal = Datatype.from_compare
      let hash = Hashtbl.hash
      let copy = Fun.id
    end)

  let of_string = function
    | "default" -> Default
    | "name" -> Class "name"
    | "type" -> Class "type"
    | "funtype" -> Class "funtype"
    | "stmt" -> Class "stmt"
    | "unknown" -> Class "unknown"
    | "print" -> Print true
    | "noprint" -> Print false
    | "ignore" -> Ignore true
    | "noignore" -> Ignore false
    | s ->
      let msg = Format.asprintf "unknown attribute info %S" s in
      raise (P.Cannot_build msg)

  let to_string = function
    | Default -> "default"
    | Class s -> s
    | Print true -> "print"
    | Print false -> "noprint"
    | Ignore true -> "ignore"
    | Ignore false -> "noignore"
end

let () = Parameter_customize.set_group parsing
let () = Parameter_customize.do_not_reset_on_copy ()
module RegisterAttributes =
  String_multiple_map
    (AttributeInfo)
    (struct
      let module_name = "RegisterAttributes"
      let option_name = "-register-attributes"
      let help =
        "Register an attribute so Frama-C knows how to handle it. \
         Keys are attribute names and values are settings. It takes a list of \
         attributes, separated by commas, each followed by a list of settings \
         separated by colons, for example 'attr1:name:print:ignore,\
         attr2:default'. The possible setting values are:\n\
         - 'default': to register an attribute with default settings. If the \
         attribute is already registered, we use its registered settings \
         instead.\n\
         - 'name', 'type', 'funtype', 'stmt' or 'unknown': class of the \
         attribute that specifies on which AST node the attribute should be \
         attached. Defaults to unknown.\n\
         - 'print' or 'noprint': should the attribute be printed when \
         printing the AST? Debug key 'printer:attrs' ignores this information. \
         Defaults to print.\n\
         - 'ignore' or 'noignore': should the attribute be ignored when \
         comparing types? Defaults to 'ignore' if class is 'unknown' and \
         'noignore' otherwise.\n\
         Note: using the same setting category several times for the same \
         attribute is undefined."
      let arg_name = "k1:v1:v2,k2:v3,..."
    end)

(* ************************************************************************* *)
(** {2 Compilation Database} *)
(* ************************************************************************* *)

let database = add_group "Compilation Database"

let () = Parameter_customize.set_group database
let () = Parameter_customize.do_not_reset_on_copy ()
module CompilationDb =
  P.Filepath
    (struct
      let option_name = "-compilation-db"
      let arg_name = "path"
      let file_kind = "directory or json"
      let existence = Filepath.Must_exist
      let help =
        "when set, preprocessing of each file will include corresponding \
         flags (e.g. -I, -D) from the JSON compilation database \
         specified by <path>. If <path> is a directory, use \
         '<path>/compile_commands.json'. Disabled by default."
    end)

let () = Parameter_customize.set_group database
let () = Parameter_customize.do_not_reset_on_copy ()
module MopsaDb =
  P.Filepath
    (struct
      let option_name = "-mopsa-db"
      let arg_name = "path"
      let file_kind = "directory or json"
      let existence = Filepath.Must_exist
      let help =
        "when set, the specified path (or <path>/mopsa-db.json, if <path> is \
         a directory) is loaded as a build database. \
         If '-mopsa-target' is not set, prints the list of targets in the \
         database and exits. Otherwise, '-mopsa-target' sets the files to \
         be parsed and preprocessing flags."
    end)

let () = Parameter_customize.set_group database
let () = Parameter_customize.do_not_reset_on_copy ()
module MopsaListDeps =
  P.String_list
    (struct
      let option_name = "-mopsa-list-deps"
      let arg_name = "target1,target2,..."
      let help = "prints the sources (and relevant preprocessing flags) \
                  used by target1,target2,..., then exits."
    end)

let () = Parameter_customize.set_group database
let () = Parameter_customize.do_not_reset_on_copy ()
module MopsaTarget =
  P.String_list
    (struct
      let option_name = "-mopsa-target"
      let arg_name = "target1,target2,..."
      let help = "name of the target(s) present in the mopsa-db for \
                  which the list of sources should be parsed; replaces \
                  any existing files in the command-line. Paths are relative \
                  to the directory containing the mopsa database, not PWD. \
                  Note that messages related to mopsa databases are still \
                  emitted relative to Frama-C's PWD, as usual."
    end)

let () = Parameter_customize.set_group database
let () = Parameter_customize.do_not_reset_on_copy ()
module MopsaExcludeSources =
  P.Filepath_list
    (struct
      let option_name = "-mopsa-exclude-sources"
      let arg_name = "file1,file2,..."
      let existence = Filepath.Indifferent
      let file_kind = "source"
      let help = "list of source files to be blacklisted from the set \
                  computed by -mopsa-target, so that Frama-C will not \
                  try to parse them."
    end)


(* ************************************************************************* *)
(** {2 Customizing Normalization} *)
(* ************************************************************************* *)

let normalisation = add_group "Customizing Normalization"

let () = Parameter_customize.set_group normalisation
module UnfoldingLevel =
  Zero
    (struct
      let module_name = "UnfoldingLevel"
      let option_name = "-ulevel"
      let arg_name = "l"
      let help =
        "unfold loops n times (defaults to 0) before analyzes. \
         A negative value hides loop unfold annotations."
    end)

let () = Parameter_customize.set_group normalisation
module UnfoldingForce =
  Bool
    (struct
      let module_name = "UnfoldingForce"
      let default = false
      let option_name = "-ulevel-force"
      let help =
        "ignore loop unfold \"done\" specifications (force unfolding)."
    end)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
let () = Parameter_customize.is_invisible ()
module LogicalOperators =
  Bool
    (struct
      let module_name = "LogicalOperators"
      let option_name = "-keep-logical-operators"
      let default = false
      let help =
        " UNSUPPORTED :  use it only if you really know what you are doing. \
         Use logical operators (&& and ||) instead of conversion into \
         conditional statements when possible."
    end)

type enum = Default | Int | Short

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
module Enums =
  P.Enum
    (struct
      type t = enum
      let default = Default
      let option_name = "-enums"
      let help =
        "decide how enumerated types should be represented:\n\
         - 'default' (default): chose between gcc or msvc depending on the \
         machdep\n\
         - 'int': treat everything as int (including enumerated types \
         with packed attribute)\n\
         - 'gcc-enums': use an unsigned integer type when no tag has a \
         negative value, and choose the smallest rank possible starting \
         from int (default gcc's behavior)\n\
         - 'gcc-short-enums': same behavior than 'gcc-enums' but starting from \
         char instead (gcc's -fshortenums option)\n\
         - 'msvc': same behavior than 'int'"
      let values = [
        Default, "default"; Default, "gcc-enums";
        Int, "int"; Int, "msvc";
        Short, "gcc-short-enums"
      ]
    end)

let () = Parameter_customize.set_group normalisation
module SimplifyCfg =
  False
    (struct
      let module_name = "SimplifyCfg"
      let option_name = "-simplify-cfg"
      let help =
        "remove break, continue and switch statements before analyses"
    end)

let () = Parameter_customize.set_group normalisation
module KeepSwitch =
  False(struct
    let option_name = "-keep-switch"
    let module_name = "KeepSwitch"
    let help = "keep switch statements despite -simplify-cfg"
  end)

let () = Parameter_customize.set_group normalisation
module KeepUnusedFunctions =
  String(struct
    let module_name = "KeepUnusedFunctions"
    let option_name = "-keep-unused-functions"
    let default = "user-specified"
    let arg_name = "none|user-specified|all|all_debug"
    let help = "whether to keep unused function declarations: none, \
                only functions with user specifications (by default; \
                excludes stdlib and generated functions), \
                or keep all unused functions (all_debug also includes \
                compiler builtins)"
  end)
let () =
  KeepUnusedFunctions.set_possible_values
    ["none"; "user-specified"; "all"; "all_debug"]

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.set_negative_option_name "-remove-unused-types"
module Keep_unused_types =
  False(struct
    let option_name = "-keep-unused-types"
    let module_name = "Keep_unused_types"
    let help = "keep unused types (false by default)"
  end)

let () = Parameter_customize.set_group normalisation
module SimplifyTrivialLoops =
  True(struct
    let option_name = "-simplify-trivial-loops"
    let module_name = "SimplifyTrivialLoops"
    let help = "simplify trivial loops, such as do ... while(0) loops"
  end)

let () = Parameter_customize.set_group normalisation
module Constfold =
  False
    (struct
      let option_name = "-constfold"
      let module_name = "Constfold"
      let help = "fold all constant expressions in the code before analysis"
    end)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
module InitializedPaddingLocals =
  True
    (struct
      let option_name = "-initialized-padding-locals"
      let module_name = "InitializedPaddingLocals"
      let help = "Implicit initialization of locals sets padding bits to 0. \
                  If false, padding bits are left uninitialized. \
                  Defaults to true."
    end)

let () = Parameter_customize.set_group normalisation
module AggressiveMerging =
  False
    (struct
      let option_name = "-aggressive-merging"
      let module_name = "AggressiveMerging"
      let help = "merge function definitions modulo renaming \
                  (defaults to false)"
    end)

let () = Parameter_customize.set_group normalisation
module AsmContractsGenerate =
  True
    (struct
      let option_name = "-asm-contracts"
      let module_name = "AsmContractsGenerate"
      let help = "generate contracts for assembly code written according \
                  to gcc's extended syntax"
    end)

let () = Parameter_customize.set_group normalisation
module AsmContractsInitialized =
  False
    (struct
      let option_name = "-asm-contracts-ensure-init"
      let module_name = "AsmContractsInitialized"
      let help = "when contracts for assembly code are generated, add \
                  postconditions stating that the output are initialized."
    end)


let () = Parameter_customize.set_group normalisation
module AsmContractsAutoValidate =
  False
    (struct
      let option_name = "-asm-contracts-auto-validate"
      let module_name = "AsmContractsAutoValidate"
      let help = "automatically mark contracts generated from asm as valid \
                  (defaults to false)"
    end)

let () = Parameter_customize.set_group normalisation
module InlineStmtContracts =
  False
    (struct
      let option_name = "-inline-stmt-contracts"
      let module_name = "InlineStmtContracts"
      let help = "transforms requires/ensures clauses of statement contracts \
                  into plain assertions, enabling their verification \
                  by plug-ins with incomplete support for statement contracts."
    end)

let () = Parameter_customize.set_group normalisation
module RemoveExn =
  False
    (struct
      let option_name = "-remove-exn"
      let module_name = "RemoveExn"
      let help =
        "transforms throw and try/catch statements to normal C functions. \
         Disabled by default, unless input source language has \
         has an exception mechanism."
    end)

module Files = struct

  let () = Parameter_customize.is_invisible ()
  let () = Parameter_customize.no_category ()
  include Filepath_list
      (struct
        let option_name = ""
        let module_name = "Files"
        let arg_name = ""
        let help = ""
        let file_kind = "source"
        let existence = Filepath.Must_exist
      end)
  let () = Cmdline.use_cmdline_files set

end

let () = Parameter_customize.set_group normalisation
module AllowDuplication =
  True(struct
    let option_name = "-allow-duplication"
    let module_name = "AllowDuplication"
    let help =
      "allow duplication of small blocks during normalization"
  end)

let () = Parameter_customize.set_group normalisation
module DoCollapseCallCast =
  True(struct
    let option_name = "-collapse-call-cast"
    let module_name = "DoCollapseCallCast"
    let help =
      "Allow some implicit casts between returned value of a function \
       and the lvalue it is assigned to."
  end)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
module GeneratedSpecMode =
  String
    (struct
      let module_name = "GeneratedSpecMode"
      let option_name = "-generated-spec-mode"
      let default = "frama-c"
      let arg_name = "mode"
      let help =
        "Select which mode will be used to generate missing specifications. \
         Can be one of: frama-c, acsl, safe, or the name of a custom \
         registered mode (defaults to frama-c). See user manual for more \
         information."
    end)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
module GeneratedSpecCustom =
  String_map
    (struct
      let module_name = "GeneratedSpecCustom"
      let option_name = "-generated-spec-custom"
      let arg_name = "c1:m1,c2:m2,..."
      let help =
        "Fine-tune missing specification generation by manually selecting \
         modes for each clause. Can be one of: frama-c, acsl, safe, skip or \
         the name of a custom registered mode. Do not use skip mode for \
         assigns unless you know what you are doing! See user manual for more \
         information."
    end)

let normalization_parameters () =
  let norm = Cmdline.Group.name normalisation in
  let kernel = Plugin.get_from_name "" in
  Hashtbl.find kernel.Plugin.p_parameters norm


(* ************************************************************************* *)
(** {2 Variadic Normalization} *)
(* ************************************************************************* *)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
module VariadicTranslation =
  True (struct
    let option_name = "-variadic-translation"
    let module_name = "VariadicTranslation"
    let help = "translate variadic functions and calls to semantic \
                equivalents with only a fixed list of formal parameters"
  end)

let () = Parameter_customize.set_group normalisation
let () = Parameter_customize.do_not_reset_on_copy ()
module VariadicStrict =
  True (struct
    let option_name = "-variadic-strict"
    let module_name = "VariadicStrict"
    let help = "display warnings about non-portable implicit casts in the \
                calls of standard variadic functions, i.e. casts between \
                distinct integral types which have the same size and \
                signedness"
  end)


(* ************************************************************************* *)
(** {2 Analysis Options} *)
(* ************************************************************************* *)

let analysis_options = add_group "Analysis Options"

let () = Parameter_customize.set_group analysis_options
module MainFunction =
  String
    (struct
      let module_name = "MainFunction"
      let default = "main"
      let option_name = "-main"
      let arg_name = "f"
      let help = "use <f> as entry point for analysis. See \"-lib-entry\" \
                  if this is not for a complete application. Defaults to main"
    end)

let () = Parameter_customize.set_group analysis_options
module LibEntry =
  False
    (struct
      let module_name = "LibEntry"
      let option_name = "-lib-entry"
      let help ="run analysis for an incomplete application e.g. an API call. See the -main option to set the entry point"
    end)

let () = Parameter_customize.set_group analysis_options
module UnspecifiedAccess =
  False(struct
    let module_name = "UnspecifiedAccess"
    let option_name = "-unspecified-access"
    let help = "do not assume that read/write accesses occurring \
                between sequence points are separated"
  end)

let () = Parameter_customize.set_negative_option_name "-unsafe-arrays"
let () = Parameter_customize.set_group analysis_options
module SafeArrays =
  True
    (struct
      let module_name = "SafeArrays"
      let option_name = "-safe-arrays"
      let help = "for multidimensional arrays or arrays that are fields \
                  inside structs, assume that accesses are in bounds"
    end)

let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module AbsoluteValidRange = struct
  module Info = struct
    let option_name = "-absolute-valid-range"
    let arg_name = "min-max"
    let help = "min and max must be integers in decimal, hexadecimal (0x, 0X), octal (0o) or binary (0b) notation and fit in 64 bits. Assume that that all absolute addresses outside of the [min-max] range are invalid. In the absence of this option, all absolute addresses are assumed to be invalid"
    let default = ""
    let module_name = "AbsoluteValidRange"
  end
  include String(Info)
end

(* Signed overflows are undefined behaviors. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module SignedOverflow =
  True
    (struct
      let module_name = "SignedOverflow"
      let option_name = "-warn-signed-overflow"
      let help = "generate alarms for signed operations that overflow."
    end)

(* Unsigned overflows are ok, but might not always be a behavior the programmer
   wants. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module UnsignedOverflow =
  False
    (struct
      let module_name = "UnsignedOverflow"
      let option_name = "-warn-unsigned-overflow"
      let help = "generate alarms for unsigned operations that overflow"
    end)

(* Left shifts on negative integers are undefined behaviors. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module LeftShiftNegative =
  True
    (struct
      let module_name = "LeftShiftNegative"
      let option_name = "-warn-left-shift-negative"
      let help = "generate alarms for signed left shifts on negative values."
    end)

(* Right shift on negative integers are implementation-defined behaviors. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module RightShiftNegative =
  False
    (struct
      let module_name = "RightShiftNegative"
      let option_name = "-warn-right-shift-negative"
      let help = "generate alarms for signed right shifts on negative values."
    end)

(* Signed downcast are implementation-defined behaviors. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module SignedDowncast =
  False
    (struct
      let module_name = "SignedDowncast"
      let option_name = "-warn-signed-downcast"
      let help = "generate alarms when signed downcasts may exceed the \
                  destination range"
    end)

(* Unsigned downcasts are ok, but might not always be a behavior the programmer
   wants. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module UnsignedDowncast =
  False
    (struct
      let module_name = "UnsignedDowncast"
      let option_name = "-warn-unsigned-downcast"
      let help = "generate alarms when unsigned downcasts may exceed the \
                  destination range"
    end)

(* Pointer downcasts are undefined behaviors. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module PointerDowncast =
  True
    (struct
      let module_name = "PointerDowncast"
      let option_name = "-warn-pointer-downcast"
      let help = "generate alarms when a pointer is converted into an integer \
                  but may not be in the range of the destination type."
    end)

(* Not finite floats are ok, but might not always be a behavior the programmer
   wants. *)
let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module SpecialFloat =
  String
    (struct
      let module_name = "SpecialFloat"
      let option_name = "-warn-special-float"
      let default = "non-finite"
      let arg_name = "none|nan|non-finite"
      let help = "generate alarms when special floats are produced: never, \
                  only on NaN, or on infinite floats and NaN (by default)."
    end)
let () = SpecialFloat.set_possible_values ["none"; "nan"; "non-finite"]

let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module InvalidBool =
  True
    (struct
      let module_name = "InvalidBool"
      let option_name = "-warn-invalid-bool"
      let help = "generate alarms when trap representations are read from \
                  _Bool lvalues."
    end)

let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module InvalidPointer =
  False
    (struct
      let module_name = "InvalidPointer"
      let option_name = "-warn-invalid-pointer"
      let help = "generate alarms when invalid pointers are created."
    end)

let () = Parameter_customize.set_group analysis_options
let () = Parameter_customize.do_not_reset_on_copy ()
module UnalignedPointer =
  True
    (struct
      let module_name = "UnalignedPointer"
      let option_name = "-warn-unaligned-pointer"
      let help = "generate alarms when unaligned pointers are created."
    end)

(* ************************************************************************* *)
(** {2 Sequencing options} *)
(* ************************************************************************* *)

let seq = add_group "Sequencing Options"

let () =
  Cmdline.add_option_without_action
    "-then"
    ~plugin:""
    ~group:seq
    ~help:"parse options before `-then' and execute Frama-C \
           accordingly, then parse options after `-then' and re-execute Frama-C"
    ~visible:true
    ~ext_help:""
    ()

let () =
  Cmdline.add_option_without_action
    "-then-last"
    ~plugin:""
    ~group:seq
    ~help:"like `-then', but the second group of actions is executed \
           on the last project created by a program transformer."
    ~visible:true
    ~ext_help:""
    ()

let () =
  Cmdline.add_option_without_action
    "-then-replace"
    ~plugin:""
    ~group:seq
    ~help:"like `-then-last', but also remove the previous current project."
    ~visible:true
    ~ext_help:""
    ()

let () =
  Cmdline.add_option_without_action
    "-then-on"
    ~plugin:""
    ~argname:"p"
    ~group:seq
    ~help:"like `-then', but the second group of actions is executed \
           on project <p>"
    ~visible:true
    ~ext_help:""
    ()

let () =
  Cmdline.add_option_without_action
    "-commands-file"
    ~plugin:""
    ~argname:"filename"
    ~group:seq
    ~help:"read the next command line arguments from the given file. \
           One argument per line. Start a line with # to add a comment."
    ~visible:true
    ~ext_help:""
    ()

(* ************************************************************************* *)
(** {2 Project-related options} *)
(* ************************************************************************* *)

let project = add_group "Project-related Options"

let () = Parameter_customize.set_group project
let () = Parameter_customize.do_not_projectify ()
module Set_project_as_default =
  False(struct
    let module_name = "Set_project_as_default"
    let option_name = "-set-project-as-default"
    let help = "the current project becomes the default one \
                (and so future '-then' sequences are applied on it)"
  end)

let () = Parameter_customize.set_group project
let () = Parameter_customize.do_not_projectify ()
module Remove_projects =
  P.Make_set
    (struct
      include Project.Datatype
      let of_string s =
        let projects = Project.find_all s in
        if projects = [] then
          raise (P.Cannot_build ("no project '" ^ s ^ "'"));
        projects
      let to_string = Project.get_name
    end)
    (struct
      let option_name = "-remove-projects"
      let arg_name = "p1, ..., pn"
      let help = "remove the given projects <p1>, ..., <pn>. \
                  @all_but_current removes all projects but the current one."
      let default = Project.Datatype.Set.empty
      let dependencies = []
    end)

let all_but_current =
  Remove_projects.Category.add
    "all_but_current"
    []
    (object
      method fold: 'a. (Project.t -> 'a -> 'a) -> 'a -> 'a =
        fun f acc ->
        Project.fold_on_projects
          (fun acc p -> if Project.is_current p then acc else f p acc)
          acc
      method mem p = not (Project.is_current p)
    end)

let _ = Remove_projects.Category.enable_all_as all_but_current

let () =
  Cmdline.run_after_configuring_stage
    (fun () ->
       (* clear "-remove-projects" before itering over (a copy of) its contents
          in order to prevent warnings about dangling pointer deletion (since it
          is itself projectified and so contains a pointer to the project being
          removed). *)
       let s = Remove_projects.get () in
       Remove_projects.clear ();
       Project.Datatype.Set.iter (fun project -> Project.remove ~project ()) s)

(* ************************************************************************* *)
(** {2 Checks} *)
(* ************************************************************************* *)

let () = Parameter_customize.set_group grp_debug
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.do_not_reset_on_copy ()
module Check =
  False(struct
    let option_name = "-check"
    let module_name = "Check"
    let help = "performs consistency checks over the Abstract Syntax \
                Tree"
  end)

let () = Parameter_customize.set_group grp_debug
let () = Parameter_customize.do_not_projectify ()
module Copy =
  False(struct
    let option_name = "-copy"
    let module_name = "Copy"
    let help =
      "always perform a copy of the original AST before analysis begin"
  end)

let () = Parameter_customize.set_group inout_source
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_negative_option_name ""
module TypeCheck =
  True(struct
    let module_name = "TypeCheck"
    let option_name = "-typecheck"
    let help = "forces typechecking of the source files"
  end)

(* ************************************************************************* *)
(** {2 Performance options} *)
(* ************************************************************************* *)

let performance = add_group "Performance"

let () = Parameter_customize.set_group performance
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
module MemoryFootprint =
  Int
    (struct
      let module_name = "MemoryFootprint"
      let default = 6
      let option_name = "-memory-footprint"
      let arg_name = "n"
      let help =
        "Control the memory usage of Frama-C. \
         With smaller values, analyses consume much less memory but are \
         also slightly slower. Must be between 1 and 10; default is 6."
    end)
let () = MemoryFootprint.set_range ~min:1 ~max:10

let () = Parameter_customize.set_group performance
let () = Parameter_customize.do_not_reset_on_copy ()
module CacheSize =
  Int
    (struct
      let module_name = "CacheSize"
      let default = 2
      let option_name = "-cache-size"
      let arg_name = "n"
      let help =
        "Control the amount of memory allocated to some internal caches. \
         Must be between 1 and 10; default value is 2. \
         Each increase of 1 doubles the size of these caches. \
         Small values are most suitable for the analysis of small programs \
         (less than 1000 lines). Higher values (around 7-8) can speed up the \
         analysis of large code bases."
    end)
let () = CacheSize.set_range ~min:1 ~max:10
let () = CacheSize.add_update_hook (fun _ i -> Binary_cache.set_cache_size i)

let () = Parameter_customize.set_group performance
let () = Parameter_customize.do_not_projectify ()
module PowLimit =
  Int(struct
    let module_name = "PowLimit"
    let option_name = "-pow-limit"
    let arg_name = "max"
    let default = 99999
    let help =
      Format.sprintf
        "for performance reasons, limit the maximum exponent accepted by pow \
         functions in Z. Must be positive. Default value is %d" default
  end)
let () = PowLimit.set_range ~min:0 ~max:max_int
let () = PowLimit.add_update_hook (fun _ i -> Z.set_pow_exponent_limit i)

(* ************************************************************************* *)
(** {2 Other options} *)
(* ************************************************************************* *)

let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_negative_option_name ""
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
module Permissive =
  False
    (struct
      let module_name = "Permissive"
      let option_name = "-permissive"
      let help =
        "perform less verifications on validity of command-line options"
    end)

(* ************************************************************************* *)
(** {2 Debug options} *)
(* ************************************************************************* *)

let () = Parameter_customize.set_group grp_debug
let () = Parameter_customize.do_not_projectify ()
let () = Parameter_customize.set_cmdline_stage Cmdline.Early
module KeepTempFiles =
  False
    (struct
      let module_name = "KeepTempFiles"
      let option_name = "-keep-temp-files"
      let help = "Keep temporary intermediate files"
    end)
