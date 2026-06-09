(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Data
module Md = Markdown
module Pkg = Package
module Senv = Server_parameters

(* -------------------------------------------------------------------------- *)
(* --- Frama-C Parameters                                                 --- *)
(* -------------------------------------------------------------------------- *)

let package =
  Package.package ~name:"parameters" ~title:"All Frama-C parameters" ()

(* Ignore any parameter with an invalid name. *)
let is_valid_parameter_name name =
  let is_valid_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-'  -> true
    | _ -> false
  in
  name.[0] = '-' && String.for_all is_valid_char name

(* Should a parameter be exported? *)
let is_exported_parameter (p : Typed_parameter.parameter) =
  p.visible && p.reconfigurable && is_valid_parameter_name p.name

(* Translates a parameter name into a valid camlCase request. *)
let camlCaseParameterName name =
  match String.split_on_char '-' name with
  | "" :: head :: tail ->
    List.fold_left (^) head (List.map String.capitalize_ascii tail)
  | _ -> Senv.fatal "Invalid parameter name %s" name

module ParameterType = struct
  type t = Typed_parameter.parameter

  let descr = Markdown.plain "Type of a command-line parameter"
  let jtype = Data.declare ~package ~name:"parameterType" ~descr Jstring

  let to_json (parameter : t) =
    match parameter.accessor with
    | Bool _ -> `String "Bool"
    | Int _ -> `String "Int"
    | Float _ -> `String "Float"
    | String _ -> `String "String"

  let of_json _ = Data.failure "ParameterType.of_json not implemented"
end


module ParameterRange = struct
  type t = Typed_parameter.parameter

  let jtype =
    let descr = Md.plain "Range of values for an integer or float parameter, \
                          or list of possible values of a string parameter" in
    Data.declare ~package ~name:"parameterRange" ~descr
      (Junion [ Jnull ; Jtuple [Jnumber; Jnumber] ; Jarray Jstring ])

  let to_json (parameter : t) =
    match parameter.accessor with
    | Bool _ -> `Null
    | Int (_accessor, range) ->
      let min, max = range () in `List [`Int min ; `Int max]
    | Float (_accessor, range) ->
      let min, max = range () in `List [`Float min ; `Float max]
    | String (_accessor, values) ->
      match values () with
      | [] -> `Null
      | list -> `List (List.map (fun s -> `String s) list)

  let of_json _ = Data.failure "ParameterRange.of_json not implemented"
end

module ParameterData = struct

  type parameter
  let jparameter : parameter Record.signature = Record.signature ()

  let field name descr =
    Record.field jparameter ~name ~descr:(Md.plain descr) (module Jstring)

  let name = field "name" "parameter name"
  let help = field "help" "parameter help message"
  let state = field "state" "name of the synchronized state for this parameter"

  let typ =
    let descr = Md.plain "Parameter type : bool, int, float or string" in
    Record.field jparameter ~name:"type" ~descr (module ParameterType)

  let range =
    let descr = Md.plain "Range of values for an integer or float parameter, \
                          or list of possible values for a string parameter" in
    Record.field jparameter ~name:"range" ~descr (module ParameterRange)

  let is_set =
    let descr = Md.plain "Has the parameter been set by the user?" in
    Record.field jparameter ~name:"isSet" ~descr (module Jbool)

  let data = Record.publish ~package ~name:"parameter"
      ~descr:(Md.plain "Information about a Frama-C parameter") jparameter

  module R : Record.S with type r = parameter = (val data)

  type t = Typed_parameter.t

  let jtype = R.jtype

  let to_json (parameter: t) =
    R.default |>
    R.set name parameter.name |>
    R.set help parameter.help |>
    R.set typ parameter |>
    R.set state (camlCaseParameterName parameter.name) |>
    R.set range parameter |>
    R.set is_set (parameter.is_set ()) |>
    R.to_json

  let of_json _ = Data.failure "Parameter.of_json not implemented"
end

module PluginData = struct

  type plugin
  let jplugin : plugin Record.signature = Record.signature ()

  let field name =
    let descr = Md.plain ("Plug-in " ^ name) in
    Record.field jplugin ~name ~descr (module Jstring)

  let name = field "name"
  let shortname = field "shortname"
  let help = field "help"

  let data = Record.publish ~package ~name:"plugin"
      ~descr:(Md.plain "Information about a Frama-C plug-in") jplugin

  module R : Record.S with type r = plugin = (val data)

  type t = Plugin.plugin

  let jtype = R.jtype

  let to_json (plugin: t) =
    R.default |>
    R.set name plugin.p_name |>
    R.set shortname plugin.p_shortname |>
    R.set help plugin.p_help |>
    R.to_json

  let of_json _ = Data.failure "Plugin.of_json not implemented"
end

let () = Request.register
    ~package ~kind:`GET ~name:"getPlugins"
    ~descr:(Md.plain "Return the list of available Frama-C plug-ins")
    ~input:(module Junit) ~output:(module Jlist (PluginData))
    (fun () -> Plugin.fold_on_plugins (fun p acc -> p :: acc) [])

let () = Request.register
    ~package ~kind:`GET ~name:"getPluginParameters"
    ~descr:(Md.plain "Return the list of parameters of a Frama-C plug-in")
    ~input:(module Jstring)
    ~output:(module Jlist (Jpair (Jstring) (Jlist (ParameterData))))
    begin fun name ->
      try
        let plugin = Plugin.get_from_name name in
        let add group params acc =
          (group, List.filter is_exported_parameter params) :: acc
        in
        Hashtbl.fold add plugin.p_parameters []
      with Not_found -> Data.failure "No plug-in of name %S" name
    end

let () = Request.register
    ~package ~kind:`GET ~name:"getParameterInfo"
    ~descr:(Md.plain "Return one parameter information")
    ~input:(module Jstring)
    ~output:(module ParameterData)
    Typed_parameter.get

let () = Request.register
    ~package ~kind:`GET ~name:"isSetParameter"
    ~descr:(Md.plain "Has the given parameter been set?")
    ~input:(module Jstring)
    ~output:(module Jbool)
    begin fun name ->
      try (Typed_parameter.get name).is_set ()
      with Not_found -> Data.failure "No parameter of name %S" name
    end


(* Registers a synchronized state for the given parameter. *)
let register_parameter parameter =
  let open Typed_parameter in
  let parameter_name = parameter.name in
  let descr = Md.plain ("State of parameter " ^ parameter_name) in
  let name = camlCaseParameterName parameter_name in
  let register data accessor =
    let add_hook f = accessor.add_update_hook (fun _ x -> f x) in
    ignore
      (States.register_state ~package ~name ~descr
         ~data ~get:accessor.get ~set:accessor.set ~add_hook ())
  in
  match parameter.accessor with
  | Bool (accessor, _) -> register (module Data.Jbool) accessor
  | Int (accessor, _) -> register (module Data.Jint) accessor
  | Float (accessor, _) -> register (module Data.Jfloat) accessor
  | String (accessor, _) -> register (module Data.Jstring) accessor

(* Registers requests for all parameters of the given plugin. *)
let register_plugin_parameters plugin =
  let register_group _group list =
    List.iter register_parameter (List.filter is_exported_parameter list)
  in
  Hashtbl.iter register_group plugin.Plugin.p_parameters

(* Automatically registers requests for all Frama-C parameters. *)
let register_all () = Plugin.iter_on_plugins register_plugin_parameters

let apply_once =
  let once = ref true in
  fun f () -> if !once then (once := false; f())

let () = Cmdline.run_after_extended_stage (apply_once register_all)

(* -------------------------------------------------------------------------- *)
(* --- Frama-C Kernel Services                                            --- *)
(* -------------------------------------------------------------------------- *)

let package = Pkg.package
    ~name:"services"
    ~title:"Kernel Services"
    ~readme:"kernel.md" ()

(* -------------------------------------------------------------------------- *)
(* --- Config                                                             --- *)
(* -------------------------------------------------------------------------- *)

let () =
  let signature = Request.signature ~input:(module Junit) () in
  let result name descr =
    Request.result signature ~name ~descr:(Md.plain descr) (module Jstring) in
  let result_list name descr =
    Request.result signature ~name ~descr:(Md.plain descr) (module Jlist (Jstring)) in
  let set_version = result "version" "Frama-C version" in
  let set_codename = result "codename" "Frama-C codename" in
  let set_version_codename =
    result "version_codename" "Frama-C version and codename"
  in
  let set_datadir = result_list "datadir" "Shared directory (FRAMAC_SHARE)" in
  let set_pluginpath = result_list "pluginpath" "Plugin directories (FRAMAC_PLUGIN)" in
  Request.register_sig
    ~package ~kind:`GET ~name:"getConfig"
    ~descr:(Md.plain "Frama-C Kernel configuration")
    signature
    begin fun rq () ->
      set_version rq System_config.Version.id ;
      set_codename rq System_config.Version.codename ;
      set_version_codename rq System_config.Version.id_and_codename ;
      set_datadir rq (Filepath.to_string_list System_config.Share.dirs);
      set_pluginpath rq
        (Filepath.to_string_list System_config.Plugins.dirs) ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Load saves                                                         --- *)
(* -------------------------------------------------------------------------- *)

let () =
  Request.register ~package ~kind:`SET ~name:"load"
    ~descr:(Md.plain "Load a save file. Returns an error, if not successful.")
    ~input:(module Jfile)
    ~output:(module Joption(Jstring))
    (fun file ->
       try Project.load_all file; None
       with Project.IOError err -> Some err)


let () =
  Request.register ~package ~kind:`SET ~name:"save"
    ~descr:(Md.plain "Save the current session. Returns an error, if not successful.")
    ~input:(module Jfile)
    ~output:(module Joption(Jstring))
    (fun file ->
       try Project.save_all file; None
       with Project.IOError err -> Some err)


(* -------------------------------------------------------------------------- *)
(* --- Log kind                                                           --- *)
(* -------------------------------------------------------------------------- *)

module LogKind =
struct
  let kinds = Enum.dictionary ()

  let t_kind value name descr =
    Enum.tag ~name ~descr:(Md.plain descr) ~value kinds

  let t_error = t_kind Log.Error "ERROR" "User Error"
  let t_warning = t_kind Log.Warning "WARNING" "User Warning"
  let t_feedback = t_kind Log.Feedback "FEEDBACK" "Plugin Feedback"
  let t_result = t_kind Log.Result "RESULT" "Plugin Result"
  let t_failure = t_kind Log.Failure "FAILURE" "Plugin Failure"
  let t_debug = t_kind Log.Debug "DEBUG" "Analyser Debug"

  let () = Enum.set_lookup kinds
      begin function
        | Log.Error -> t_error
        | Log.Warning -> t_warning
        | Log.Feedback -> t_feedback
        | Log.Result -> t_result
        | Log.Failure -> t_failure
        | Log.Debug -> t_debug
      end

  let data = Request.dictionary ~package
      ~name:"logkind"
      ~descr:(Md.plain "Log messages categories.")
      kinds

  include (val data : S with type t = Log.kind)
end

(* -------------------------------------------------------------------------- *)
(* --- Synchronized array of log events                                   --- *)
(* -------------------------------------------------------------------------- *)

let model = States.model ()

let () = States.column model ~name:"kind"
    ~descr:(Md.plain "Message kind")
    ~data:(module LogKind)
    ~get:(fun (evt, _) -> evt.Log.evt_kind)

let () = States.column model ~name:"plugin"
    ~descr:(Md.plain "Emitter plugin")
    ~data:(module Jalpha)
    ~get:(fun (evt, _) -> evt.Log.evt_plugin)

let () = States.column model ~name:"message"
    ~descr:(Md.plain "Message text")
    ~data:(module Jstring)
    ~get:(fun (evt, _) -> Log.Event.message evt)

let () = States.option model ~name:"category"
    ~descr:(Md.plain "Message category (only for debug or warning messages)")
    ~data:(module Jstring)
    ~get:(fun (evt, _) -> evt.Log.evt_category)

let () = States.option model ~name:"source"
    ~descr:(Md.plain "Source file position")
    ~data:(module Kernel_ast.Position)
    ~get:(fun (evt, _) -> evt.Log.evt_source)

let getMarker (evt, _id) =
  Option.bind Printer_tag.pos_to_localizable evt.Log.evt_source

let getDecl t =
  Option.bind Printer_tag.declaration_of_localizable (getMarker t)

let () = States.option model ~name:"marker"
    ~descr:(Md.plain "Marker at the message position (if any)")
    ~data:(module Kernel_ast.Marker)
    ~get:getMarker

let () = States.option model ~name:"decl"
    ~descr:(Md.plain "Declaration containing the message position (if any)")
    ~data:(module Kernel_ast.Decl)
    ~get:getDecl

let iter f = ignore (Messages.fold (fun i evt -> f (evt, i); succ i) 0)

let add_reload_hook f =
  Project.register_after_set_current_hook ~user_only:false (fun _ -> f ())

let _array =
  States.register_array
    ~package
    ~name:"message"
    ~descr:(Md.plain "Log messages")
    ~key:(fun (_evt, i) -> string_of_int i)
    ~iter
    ~add_update_hook:Messages.add_hook
    ~add_reload_hook
    model

(* -------------------------------------------------------------------------- *)
(* --- Log Events                                                         --- *)
(* -------------------------------------------------------------------------- *)

module LogEvent =
struct

  type rlog

  let jlog : rlog Record.signature = Record.signature ()

  let kind = Record.field jlog ~name:"kind"
      ~descr:(Md.plain "Message kind") (module LogKind)
  let plugin = Record.field jlog ~name:"plugin"
      ~descr:(Md.plain "Emitter plugin") (module Jalpha)
  let message = Record.field jlog ~name:"message"
      ~descr:(Md.plain "Message text") (module Jstring)
  let category = Record.option jlog ~name:"category"
      ~descr:(Md.plain "Message category (DEBUG or WARNING)") (module Jstring)
  let source = Record.option jlog ~name:"source"
      ~descr:(Md.plain "Source file position") (module Kernel_ast.Position)

  let data = Record.publish ~package ~name:"log"
      ~descr:(Md.plain "Message event record.") jlog

  module R : Record.S with type r = rlog = (val data)

  type t = Log.event

  let jtype = R.jtype

  let to_json evt =
    R.default |>
    R.set plugin evt.Log.evt_plugin |>
    R.set kind evt.Log.evt_kind |>
    R.set category evt.Log.evt_category |>
    R.set source evt.Log.evt_source |>
    R.set message (Log.Event.message evt) |>
    R.to_json

  let of_json js =
    let r = R.of_json js in
    {
      Log.evt_plugin = R.get plugin r ;
      Log.evt_kind = R.get kind r ;
      Log.evt_category = R.get category r ;
      Log.evt_source = R.get source r ;
      Log.evt_message = Rich_text.of_string (R.get message r) ;
    }

end

(* -------------------------------------------------------------------------- *)
(* --- Log Monitoring                                                     --- *)
(* -------------------------------------------------------------------------- *)

let monitoring = ref false
let monitored = ref false
let events : Log.event Queue.t = Queue.create ()

let set_monitoring flag =
  if flag != !monitoring then
    monitoring := flag ;
  if !monitoring && not !monitored then
    begin
      monitored := true ;
      Log.add_listener (fun evt -> if !monitoring then Queue.add evt events)
    end

let monitor_server activity =
  if not (Senv.AutoLog.get ()) then set_monitoring activity

let monitor_autologs () =
  if Senv.AutoLog.get () then
    begin
      Senv.feedback "Auto-log started." ;
      set_monitoring true ;
    end

let () =
  Main.on monitor_server ;
  Cmdline.run_after_configuring_stage monitor_autologs

(* -------------------------------------------------------------------------- *)
(* --- Log Requests                                                       --- *)
(* -------------------------------------------------------------------------- *)

(* TODO:LC: shall have an array here. *)

let () = Request.register
    ~package ~kind:`SET ~name:"setLogs"
    ~descr:(Md.plain "Turn logs monitoring on/off")
    ~input:(module Jbool) ~output:(module Junit)
    set_monitoring

let () = Request.register
    ~package ~kind:`GET ~name:"getLogs"
    ~descr:(Md.plain "Flush the last emitted logs since last call (max 100)")
    ~input:(module Junit) ~output:(module Jlist(LogEvent))
    begin fun () ->
      let pool = ref [] in
      let count = ref 100 in
      while not (Queue.is_empty events) && !count > 0 do
        decr count ;
        pool := Queue.pop events :: !pool
      done ;
      List.rev !pool
    end

(* -------------------------------------------------------------------------- *)
