(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let list_plugin_names () =
  Plugin.fold_on_plugins (fun p acc -> p.Plugin.p_name :: acc) []

let dump_parameter tp =
  let open Typed_parameter in
  let json_value = match tp.accessor with
    | Bool (accessor,_) -> `Bool (accessor.get ())
    | Int (accessor,_) -> `Int (accessor.get ())
    | Float (accessor,_) -> `Float (accessor.get ())
    | String (accessor,_) -> `String (accessor.get ())
  in
  tp.name, json_value

let dump_all_parameters () =
  let add_category _ l acc =
    List.fold_left (fun acc tp -> dump_parameter tp :: acc) acc l
  in
  let add_plugin plugin acc =
    Hashtbl.fold add_category plugin.Plugin.p_parameters acc
  in
  Plugin.fold_on_plugins add_plugin []

let dump_to_json () =
  let string s = `String s in
  let list f l = `List (List.map f l) in
  `Assoc [
    "version", `String System_config.Version.id ;
    "codename", `String System_config.Version.codename ;
    "version_and_codename", `String System_config.Version.id_and_codename ;
    "major_version", `Int System_config.Version.major ;
    "minor_version", `Int System_config.Version.minor ;
    "datadir", `String (Filepath.to_string_abs System_config.Share.main) ;
    "datadirs",
    list string (Filepath.to_string_list System_config.Share.dirs) ;
    "framac_libc", `String (Filepath.to_string_abs System_config.Share.libc) ;
    "plugin_dir",
    list string (Filepath.to_string_list System_config.Plugins.dirs) ;
    "lib_dir", `String (Filepath.to_string_abs System_config.Lib.main) ;
    "lib_dirs",
    list string (Filepath.to_string_list System_config.Lib.dirs) ;
    "preprocessor", `String System_config.Preprocessor.command ;
    "using_default_cpp", `Bool System_config.Preprocessor.is_default ;
    "preprocessor_is_gnu_like", `Bool System_config.Preprocessor.is_gnu_like ;
    "preprocessor_supported_arch_options",
    list string System_config.Preprocessor.supported_arch_options ;
    "preprocessor_keep_comments", `Bool System_config.Preprocessor.keep_comments ;
    "current_machdep", `String (Kernel.Machdep.get ()) ;
    "machdeps", list string (File.list_available_machdeps ()) ;
    "plugins", list string (list_plugin_names ()) ;
    "parameters", `Assoc (dump_all_parameters ()) ;
  ]

let dump_to_stdout () =
  let json = dump_to_json () in
  Yojson.Basic.(pretty_to_channel stdout (sort json))

let () =
  let action () =
    if Kernel.PrintConfigJson.get () then begin
      dump_to_stdout ();
      raise Cmdline.Exit
    end else
      Cmdline.nop
  in
  Cmdline.run_after_exiting_stage action
