(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Sarif

let frama_c_sarif () =
  let name = "frama-c" in
  let version, semanticVersion =
    if Mdr_params.SarifDeterministic.get () then
      "0+omitted-for-deterministic-output", ""
    else
      System_config.Version.id_and_codename, System_config.Version.id
  in
  let fullName = name ^ "-" ^ version in
  let downloadUri = "https://frama-c.com/download.html" in
  let informationUri = "https://frama-c.com" in
  Tool.create
    (Driver.create ~name ~version ~semanticVersion ~fullName ~downloadUri
       ~informationUri ())

let get_remarks () =
  if not (Mdr_params.Remarks.is_empty ()) then
    Parse_remarks.get_remarks (Mdr_params.Remarks.get ())
  else Datatype.String.Map.empty

let get_remark remarks label =
  match Datatype.String.Map.find_opt label remarks with
  | None -> []
  | Some l -> l

(* keep track of command line arguments for all invocations of Frama-C during
   a save/load sequence. Note that the list is in reverse order
   (newest invocation first).
*)
module Analysis_cmdline =
  State_builder.List_ref(Datatype.List(Datatype.String))
    (struct
      let name = "Sarif_gen.Analysis_cmdline"
      let dependencies = []
    end)

let command_line () = Array.to_list Sys.argv

let update_cmdline =
  let already_updated = ref false in
  fun () ->
    if not (!already_updated) then begin
      (* This function must be run after the loading stage, so that
         the Analysis_cmdline state contains the list of previous launches
         if any. However, `-then` restart the boot sequence from the loading
         included, meaning that the hook will be replayed _also_ after each
         `-then`. Using a _non-projectified_ boolean ref ensures that we add
         the command line only once per run. *)
      already_updated := true;
      Analysis_cmdline.add (command_line())
    end

let () = Cmdline.run_after_loading_stage update_cmdline

let gen_invocation () =
  let cls = Analysis_cmdline.get () in
  let gen_one cl =
    (* The first argument is _always_ the binary name, but to avoid printing it
       as an absolute path to binlevel.opt, we replace it with 'frama-c' *)
    let cl = "frama-c" :: List.tl cl in
    let commandLine = String.concat " " cl in
    let arguments = List.tl cl in
    Invocation.create ~commandLine ~arguments ()
  in
  List.rev_map gen_one cls

let gen_remark alarm =
  let open Markdown in
  [ Block
      [ Text
          (plain
             (Printf.sprintf "This alarm represents a potential %s."
                (Alarms.get_description alarm)
             )
          )
      ]
  ]

let kind_of_status =
  let open Property_status.Feedback in
  let open Sarif.Result_kind in
  function
  | Never_tried -> notApplicable
  | Considered_valid | Valid | Valid_under_hyp | Valid_but_dead -> pass
  | Unknown | Unknown_but_dead -> open_
  | Invalid | Invalid_under_hyp | Invalid_but_dead -> fail
  | Inconsistent -> review

let level_of_status =
  let open Property_status.Feedback in
  let open Sarif.Result_level in
  function
  | Never_tried -> none
  | Considered_valid | Valid | Valid_under_hyp | Valid_but_dead -> none
  | Unknown | Unknown_but_dead -> none
  | Invalid | Invalid_under_hyp | Invalid_but_dead -> error
  | Inconsistent -> none

let make_message alarm annot remark =
  let open Markdown in
  let name = Alarms.get_name alarm in
  let text = name ^ "." in
  let kind = plain (name ^ ":") in
  let descr = codeblock ~lang:"acsl" "%a" Printer.pp_code_annotation annot in
  let summary = Block (Text kind :: descr) in
  let markdown =
    match remark with
    | [] -> summary :: gen_remark alarm
    | _ -> summary :: remark
  in
  let markdown =
    String.trim
      (Format.asprintf "@[%a@]" (Markdown.pp_elements ~page:"") markdown)
  in
  Message.create ~text ~markdown ()

let kf_is_in_libc kf =
  Cil.global_is_in_libc (Kernel_function.get_global kf)

let ip_is_in_libc ip =
  match Property.get_kf ip with
  | None ->
    (* possibly an identified lemma; check its attributes *)
    begin
      match ip with
      | IPAxiomatic {iax_attrs=attrs}
      | IPLemma {il_attrs=attrs}
        -> Cil.is_in_libc attrs
      | _ ->
        false
    end
  | Some kf ->
    kf_is_in_libc kf

let opt_physical_location_of_loc loc =
  if loc = Fileloc.unknown then []
  else [ Location.of_loc loc ]
(* Cil_types *)
let gen_results remarks =
  let keep_alarm (_,kf,_,_,_,_) =
    (* skip alarm in libc *)
    Mdr_params.PrintLibc.get () || not (kf_is_in_libc kf)
  in
  let treat_alarm i (_e,kf,s,_rank,alarm,annot) =
    let prop = Property.ip_of_code_annot_single kf s annot in
    let ruleId = Alarms.get_name alarm in
    let label = "Alarm-" ^ string_of_int i in
    let kind = kind_of_status (Property_status.Feedback.get prop) in
    let level = level_of_status (Property_status.Feedback.get prop) in
    let remark = get_remark remarks label in
    let message = make_message alarm annot remark in
    let locations = opt_physical_location_of_loc (Cil_datatype.Stmt.loc s) in
    let res = Sarif_result.create ~kind ~level ~ruleId ~message ~locations () in
    (ruleId, Alarms.get_description alarm), res
  in
  let rules, content =
    Alarms.to_seq () |> Seq.filter keep_alarm |>
    Seq.mapi treat_alarm |> Seq.unzip
  in
  Datatype.String.Map.of_seq rules, List.of_seq content

let is_alarm = function
  | Property.(IPCodeAnnot { ica_ca }) -> Option.is_some (Alarms.find ica_ca)
  | _ -> false

let make_ip_message ip =
  let text = Format.asprintf "@[%a.@]" Property.short_pretty ip in
  Message.plain_text ~text ()

let user_annot_id = "user-spec"

let gen_status ip =
  let status = Property_status.Feedback.get ip in
  let level = level_of_status status in
  let locations = opt_physical_location_of_loc (Property.location ip) in
  let message = make_ip_message ip in
  Sarif_result.create ~ruleId:user_annot_id ~level ~locations ~message ()

let gen_statuses () =
  let cmp = Property.Ordered_by_function.compare in
  let f ip content =
    let exclude =
      is_alarm ip ||
      (not (Mdr_params.PrintLibc.get ()) && ip_is_in_libc ip)
    in
    if exclude then content else (gen_status ip) :: content
  in
  List.rev (Property_status.fold_sorted ~cmp f [])

let gen_artifacts () =
  let add_src_file f =
    let base, uri = Filepath.to_base_uri f in
    let location = ArtifactLocation.create ~uri ~base () in
    let roles = [ Role.analysisTarget ] in
    let mimeType = "text/x-csrc" in
    Artifact.create ~location ~roles ~mimeType ()
  in
  List.map add_src_file (Kernel.Files.get ())

let add_rule id desc l =
  let text = desc ^ "." in
  let shortDescription = MultiformatMessageString.create ~text () in
  let rule = ReportingDescriptor.create ~id ~shortDescription () in
  rule :: l

let make_taxonomies rules = Datatype.String.Map.fold add_rule rules []

let gen_run remarks =
  let tool = frama_c_sarif () in
  let name = "frama-c" in
  let invocations = gen_invocation () in
  let rules, results = gen_results remarks in
  let user_annot_results = gen_statuses () in
  let rules =
    match user_annot_results with
    | [] -> rules
    | _ ->
      Datatype.String.Map.add
        user_annot_id "User-written ACSL specification" rules
  in
  let rules = make_taxonomies rules in
  let taxonomies = [ToolComponent.create ~name ~rules ()] in
  let results = results @ user_annot_results in
  let artifacts = gen_artifacts () in
  let symbolicDirs =
    List.map (fun (key, (dir : Filepath.t)) ->
        (key, (Filepath.to_string_abs dir))
      ) (Filepath.all_symbolic_dirs ())
  in
  let pwd = Filepath.(to_string_abs (pwd ())) in
  let uriBases = ("PWD", pwd) :: symbolicDirs in
  let uriBases =
    if not (Mdr_params.SarifDeterministic.get ())
    then uriBases
    else
      uriBases
      |> List.map (fun (name, _) -> (name, "/omitted-for-deterministic-output"))
      |> List.sort_uniq Extlib.compare_basic
  in
  let uriBasesJson =
    List.map (fun (name, dir) ->
        (name, `Assoc [("uri", `String ("file://" ^ dir ^ "/"))]))
      uriBases
  in
  let originalUriBaseIds =
    match ArtifactLocationDictionary.of_yojson (`Assoc uriBasesJson) with
    | Ok x -> x
    | Error s -> failwith s
  in
  Run.create ~tool ~invocations ~results ~taxonomies ~artifacts
    ~originalUriBaseIds ()

let generate () =
  let remarks = get_remarks () in
  let runs = [ gen_run remarks ] in
  let json = Schema.create ~runs () |> Schema.to_yojson in
  if not (Mdr_params.Output.is_empty ()) then
    let file = Mdr_params.Output.get () in
    try
      let open Filesystem.Operators in
      let$ out = Filesystem.with_open_out_exn file in
      Yojson.Safe.pretty_to_channel ~std:true out json;
      output_char out '\n';
      Mdr_params.result "Report %a generated" Filepath.pretty file
    with Sys_error s ->
      Mdr_params.abort "Unable to generate %a (%s)"
        Filepath.pretty file s
  else
    Log.print_on_output (fun fmt -> Yojson.Safe.pretty_print fmt json)
