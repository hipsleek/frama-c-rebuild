(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type object_entry = {
  source: Filepath.t;
  lang: string;
  args: string list;
}

(* a 'target' is an executable or a library *)
type target_entry = {
  contents: Filepath.t list;
}

let tables_from_json db_dir json =
  let open Yojson.Basic.Util in
  let object_map = Hashtbl.create 10 in
  let target_map = Hashtbl.create 3 in
  let base = db_dir in
  List.iter (fun entry ->
      let filename =
        entry |> member "filename" |> to_string |> Filepath.of_string ~base
      in
      let typ = entry |> member "type" |> to_string in
      if typ = "object" then begin
        let lang = entry |> member "lang" |> to_string in
        let source =
          entry |> member "source" |> to_string |> Filepath.of_string ~base
        in
        let args = entry |> member "args" |> to_list |> List.map to_string in
        let _path = entry |> member "path" |> to_string |> Filepath.of_string ~base in
        let entry = { lang; source; args } in
        Kernel.debug
          ~dkey:Kernel.dkey_mopsa_db
          "object_map: adding '%a'" Filepath.pretty_abs filename;
        Hashtbl.replace object_map filename entry;
        Parse_env.set_workdir source base;
      end else if typ = "executable" || typ = "library" then begin
        let contents =
          entry |> member "contents" |> to_list |>
          List.map (fun d -> to_string d |> Filepath.of_string ~base)
        in
        let entry = { contents } in
        Kernel.debug
          ~dkey:Kernel.dkey_mopsa_db
          "target_map: adding '%a'" Filepath.pretty_abs filename;
        Hashtbl.replace target_map filename entry
      end else
        Kernel.abort "unknown 'type' in Mopsa DB: %s" typ
    ) (json |> member "contents" |> to_list);
  (object_map, target_map)

let pp_tbl_paths fmt tbl =
  let paths =
    Hashtbl.fold (fun (k : Filepath.t) _v acc ->
        let s = Filepath.to_string_abs k in s :: acc) tbl []
  in
  let sorted = List.sort String.compare paths in
  List.iter (fun k -> Format.fprintf fmt "%s@\n" k) sorted

let acc_deps object_map target_map targets =
  let rec aux ((acc_res, acc_seen) as acc) targets =
    match targets with
    | [] -> acc_res
    | t :: r ->
      if Filepath.Set.mem t acc_seen then
        aux acc r
      else begin
        match Hashtbl.find_opt target_map t with
        | None ->
          begin
            if String.ends_with ~suffix:".a" (Filepath.to_string_abs t)
            || String.ends_with ~suffix:".so" (Filepath.to_string_abs t)
            then begin
              Kernel.warning ~wkey:Kernel.wkey_mopsa_db
                "library '%a' not found in mopsa-db, ignoring"
                Filepath.pretty_abs t;
              aux (acc_res, Filepath.Set.add t acc_seen) r
            end else
              match Hashtbl.find_opt object_map t with
              | None ->
                let msg =
                  let wkey = Kernel.wkey_mopsa_db_missing_library in
                  match Kernel.(get_warn_status wkey) with
                  | Log.(Winactive | Wfeedback | Wfeedback_once) ->
                    ", ignoring"
                  | _ ->
                    if Kernel.(is_debug_key_enabled dkey_mopsa_db_verbose) then
                      Format.asprintf
                        "@\nobjects:@\n@[%a@]@\ntargets:@\n@[%a@]"
                        pp_tbl_paths object_map
                        pp_tbl_paths target_map
                    else ""
                in
                Kernel.warning ~wkey:Kernel.wkey_mopsa_db_missing_library
                  "entry '%a' not found in mopsa-db%s"
                  Filepath.pretty_abs t msg;
                aux (acc_res, acc_seen) r
              | Some o ->
                if o.lang <> "C" then begin
                  let suffixes = File.get_suffixes () in
                  let suffix = Filepath.extension o.source in
                  let try_parse =
                    if List.mem suffix suffixes then true
                    else begin
                      Kernel.warning ~wkey:Kernel.wkey_mopsa_db_non_c ~once:true
                        "ignoring non-C (%s) dependency: %a@\n(setting this \
                         warning category to inactive or feedback will try to \
                         parse it nevertheless)"
                        o.lang Filepath.pretty t;
                      let force =
                        match Kernel.(get_warn_status wkey_mopsa_db_non_c) with
                        | Log.(Wfeedback | Wfeedback_once | Winactive) -> true
                        | _ -> false
                      in
                      force
                    end
                  in
                  if try_parse then
                    aux ((o.source, o.args) :: acc_res, acc_seen) r
                  else aux (acc_res, acc_seen) r
                end else
                  aux ((o.source, o.args) :: acc_res, acc_seen) r
          end
        | Some t ->
          aux (acc_res, acc_seen) (r @ t.contents)
      end
  in
  aux ([], Filepath.Set.empty) targets

let calc_deps db_dir db_json (module Targets : Parameter_sig.String_list) =
  let (object_map, target_map) = tables_from_json db_dir db_json in
  let targets =
    Targets.fold (fun f acc ->
        let path = Filepath.of_string ~base:db_dir f in
        match Hashtbl.find_opt target_map path with
        | None ->
          Kernel.abort "executable or library '%a' not found in mopsa-db"
            Filepath.pretty_abs path
        | Some _entry -> path :: acc) []
  in
  let r = acc_deps object_map target_map targets in
  List.sort (fun (fp1, _) (fp2, _) -> Filepath.compare_pretty fp1 fp2) r

let join_filtered_args args =
  let buf = Buffer.create 64 in
  let continuation_of_previous_arg = ref "" in
  List.iter (fun arg ->
      if !continuation_of_previous_arg <> "" then begin
        Buffer.add_string buf !continuation_of_previous_arg;
        Buffer.add_char buf ' ';
        Buffer.add_char buf '\'';
        Buffer.add_string buf arg;
        Buffer.add_char buf '\'';
        continuation_of_previous_arg := ""
      end else if arg = "-I" || arg = "-D" || arg = "-isystem" then
        continuation_of_previous_arg := " " ^ arg
      else if String.starts_with ~prefix:"-I" arg ||
              String.starts_with ~prefix:"-D" arg ||
              String.starts_with ~prefix:"-isystem" arg
      then begin
        Buffer.add_char buf ' ';
        Buffer.add_char buf '\'';
        Buffer.add_string buf arg;
        Buffer.add_char buf '\'';
      end
    ) args;
  Buffer.contents buf

let run () =
  let db_path = Kernel.MopsaDb.get () in
  (* if we entered this function with db_path = empty, then one of the other
     mopsa-related options was set. Assume '.' for db_path. *)
  let db_path =
    if Filepath.is_empty db_path then Filepath.of_string "." else db_path
  in
  (* here, db_path exists (checked by Filepath constructor) *)
  let adjusted_db_path =
    if Filesystem.dir_exists db_path then
      let new_path = Filepath.(db_path / "mopsa-db.json") in
      if Filesystem.exists new_path then new_path
      else
        Kernel.abort
          "mopsa-db: directory '%a' does not contain a mopsa-db.json file"
          Filepath.pretty db_path
    else
      db_path
  in
  let db_dir = Filepath.dirname adjusted_db_path in
  let json =
    try
      Yojson.Basic.from_file (Filepath.to_string_abs adjusted_db_path)
    with
    | Yojson.Json_error s ->
      Kernel.abort "mopsa-db: invalid JSON file '%a': %s"
        Filepath.pretty adjusted_db_path s
  in
  let open Yojson.Basic.Util in
  let targets =
    json |> member "contents" |> to_list |> filter_map
      (fun o ->
         match o |> member "type" |> to_string with
         | "executable" | "library" -> Some o
         | _ -> None)
  in
  if List.length targets = 0 then begin
    Kernel.result "no executables/libraries found in %a"
      Filepath.pretty db_path;
    raise Cmdline.Exit
  end else
  if not (Kernel.MopsaListDeps.is_empty ()) then begin
    (* 'list-dependencies' mode *)
    let deps = calc_deps db_dir json (module Kernel.MopsaListDeps) in
    Kernel.result "dependencies:@\n%a"
      (Pretty_utils.pp_list ~sep:"@\n"
         (Pretty_utils.pp_pair ~sep:":\t"
            Filepath.pretty_abs
            (fun fmt args ->
               Format.pp_print_string fmt (join_filtered_args args)))
      ) deps;
    raise Cmdline.Exit
  end
  else if (Kernel.MopsaTarget.get ()) <> [] then begin
    (* '-mopsa-target' mode *)
    let deps = calc_deps db_dir json (module Kernel.MopsaTarget) in

    (* Add preprocessing flags for files in the DB *)
    List.iter (fun (fname, args) ->
        let args = join_filtered_args args in
        Kernel.CppExtraArgsPerFile.add (fname, args);
      ) deps;

    let deps_files = List.(sort_uniq Filepath.compare (map fst deps)) in
    let count_before_filter = List.length deps_files in
    let deps_files = List.filter (fun s ->
        not (List.mem s (Kernel.MopsaExcludeSources.get ()))
      ) deps_files
    in
    if deps_files = [] then
      Kernel.warning
        ~wkey:Kernel.wkey_mopsa_db
        "No remaining sources in mopsa-db \
         (%d sources before filters)!" count_before_filter
    else
      Kernel.feedback ~dkey:Kernel.dkey_mopsa_db
        "Sources from mopsa-db:@\n%a"
        (Pretty_utils.pp_list ~sep:"@\n" Filepath.pretty) deps_files;
    Kernel.Files.set deps_files;
    Kernel.Files.add_set_hook (fun _ _ ->
        (* The user specified one or more sources;
           we need to pre-register ours *)
        List.iter (fun f -> File.(pre_register (from_filename f))) deps_files
      );
  end else begin (* 'print-targets' mode *)
    let pp_target fmt target =
      let path =
        target |> member "filename" |> to_string |> Filepath.of_string
      in
      let ttype = target |> member "type" |> to_string in
      Format.fprintf fmt "[%-10s] %a" ttype Filepath.pretty path
    in
    Kernel.result "targets:@\n%a"
      (Pretty_utils.pp_list ~sep:"@\n" pp_target) targets;
    raise Cmdline.Exit
  end

let run_once, _ =
  State_builder.apply_once "Mopsa.run"
    [Kernel.MopsaTarget.self; Kernel.MopsaDb.self] run

let main () =
  let enabled =
    not (Kernel.MopsaDb.is_empty ()) ||
    not (Kernel.MopsaTarget.is_empty ()) ||
    not (Kernel.MopsaListDeps.is_empty ())
  in
  if enabled then run_once ()

let () =
  Cmdline.run_after_configuring_stage main
