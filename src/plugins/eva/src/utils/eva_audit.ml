(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let get_correctness_parameters () =
  let get param =
    let name = param.Typed_parameter.name in
    let value = Typed_parameter.get_value param in
    (name, value)
  in
  List.map get (Parameters.parameters_correctness)

let parameters_of_json json =
  try
    let open Yojson.Basic.Util in
    let params =
      json |> member "eva" |> member "correctness-parameters" |> to_assoc
    in
    List.map (fun (key, value) -> (key, to_string value)) params
  with
  | Yojson.Json_error msg ->
    Kernel.abort "error reading JSON file: %s" msg
  | Yojson.Basic.Util.Type_error (msg, v) ->
    Kernel.abort "error reading JSON file: %s - %s" msg
      (Yojson.Basic.pretty_to_string v)

let print_correctness_parameters path =
  let parameters = get_correctness_parameters () in
  if Filepath.is_special_stdout path then begin
    Self.feedback "Correctness parameters of the analysis:";
    let print (name, value) = Self.printf "  %s: %s" name value in
    List.iter print parameters
  end else begin
    let json = List.map (fun (name, value) -> name, `String value) parameters in
    let params_json = `Assoc [("correctness-parameters", `Assoc json)] in
    let eva_json = `Assoc [("eva", params_json)] in
    Json.merge_object path eva_json
  end

let check_correctness_parameters json =
  let parameters = get_correctness_parameters () in
  let expected_parameters = parameters_of_json json in
  (* Note: we could compare lengths and use a two-list iterator on sorted lists,
     but in case of divergence, the error messages would be less clear. *)
  List.iter (fun (exp_p, exp_v) ->
      try
        let v = List.assoc exp_p parameters in
        if exp_v <> v then
          Kernel.warning ~wkey:Kernel.wkey_audit
            "correctness parameter %s: expected value %s, but got %s" exp_p
            exp_v v
      with Not_found ->
        Kernel.warning ~wkey:Kernel.wkey_audit
          "expected correctness parameter %s, but not found" exp_p
    ) expected_parameters

module type PluginWarnings =
sig
  type warn_category
  val wkey_name: warn_category -> string
  val get_all_warn_categories_status:
    unit -> (warn_category * Log.warn_status) list
end

let compute_warning_status (module Plugin: PluginWarnings) =
  let warning_categories = Plugin.get_all_warn_categories_status () in
  let is_active = function
    | Log.Winactive | Wfeedback_once | Wfeedback -> false
    | Wonce | Wactive | Werror_once | Werror | Wabort -> true
  in
  let is_enabled (_key, status) = is_active status in
  let enabled, disabled = List.partition is_enabled warning_categories in
  let pp_fst = List.map (fun (c, s) -> Plugin.wkey_name c, s) in
  (pp_fst enabled, pp_fst disabled)

let json_of_warning_statuses wkeys key_name =
  let json_of_wkey = List.map (fun (c, _) -> `String c) in
  (key_name, `List (json_of_wkey wkeys))

let enabled_warning_of_json name json =
  try
    let open Yojson.Basic.Util in
    json |> member name |> member "warning-categories" |>
    member "enabled" |> to_list |> filter_string
  with
  | Yojson.Json_error msg ->
    Kernel.abort "error reading JSON file: %s" msg
  | Yojson.Basic.Util.Type_error (msg, v) ->
    Kernel.abort "error reading JSON file: %s - %s" msg
      (Yojson.Basic.pretty_to_string v)

let print_warning_status path name (module Plugin: PluginWarnings) =
  let enabled, disabled = compute_warning_status (module Plugin) in
  if Filepath.is_special_stdout path then
    begin
      let pp_categories =
        Pretty_utils.pp_list ~sep:",@ " Format.pp_print_string
      in
      Self.feedback "Audit: %s warning categories:" name;
      Self.printf "  Enabled: @[%a@]"
        pp_categories (List.map fst enabled);
      Self.printf "  Disabled: @[%a@]"
        pp_categories (List.map fst disabled)
    end
  else begin
    let enabled_json = json_of_warning_statuses enabled "enabled"
    and disabled_json = json_of_warning_statuses disabled "disabled" in
    let warning_json = `Assoc [enabled_json; disabled_json] in
    let name = Stdlib.String.lowercase_ascii name in
    let json = `Assoc [(name, `Assoc [("warning-categories", warning_json)])] in
    Json.merge_object path json
  end

let check_warning_status json name (module Plugin: PluginWarnings) =
  let name = Stdlib.String.lowercase_ascii name in
  let enabled, _disabled = compute_warning_status (module Plugin) in
  let enabled = List.map fst enabled in
  let expected_enabled = enabled_warning_of_json name json in
  let diff l1 l2 = List.filter (fun k -> not (List.mem k l2)) l1 in
  let should_be_enabled = diff expected_enabled enabled in
  if should_be_enabled <> [] then
    Kernel.warning ~wkey:Kernel.wkey_audit
      "the following warning categories were expected to be enabled,@ \
       but are disabled: %a"
      (Pretty_utils.pp_list ~sep:", " Format.pp_print_string) should_be_enabled

let check_configuration path =
  try
    let json = Json.from_file path in
    check_correctness_parameters json;
    check_warning_status json "Kernel" (module Kernel);
    check_warning_status json "Eva" (module Self)
  with Yojson.Json_error msg ->
    Kernel.abort "error reading JSON file %a: %s"
      Filepath.pretty path msg

let print_configuration path =
  try
    print_correctness_parameters path;
    print_warning_status path "Kernel" (module Kernel);
    print_warning_status path "Eva" (module Self);
    if not (Filepath.is_special_stdout path) then
      Self.feedback "Audit: eva configuration written to: %a"
        Filepath.pretty path;
  with Json.CannotMerge _ ->
    Kernel.failure "%s: error when writing json file %a."
      Kernel.AuditPrepare.option_name Filepath.pretty path

let prepare_audit () =
  if not (Kernel.AuditPrepare.is_empty ()) then
    print_configuration (Kernel.AuditPrepare.get ())

let () = Cmdline.at_normal_exit prepare_audit
