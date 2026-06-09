(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Wpo

type script =
  | NoScript
  | Script of Filepath.t
  | Deprecated of Filepath.t

let files : (Filepath.t,script) Hashtbl.t = Hashtbl.create 32

let jsonfile (dir:Filepath.t) filename =
  Filepath.(dir / (filename ^ ".json"))

let get_script_dir ~force =
  Wp_parameters.Session.get_dir ~create_path:force "script"

let filename ~force wpo =
  let dscript = get_script_dir ~force in
  jsonfile dscript wpo.po_sid (* no model in name *)

let legacies wpo =
  let mid = WpContext.MODEL.id wpo.po_model in
  let dscript = Wp_parameters.Session.get_dir "script" in
  let dmodel = Wp_parameters.Session.get_dir mid in
  [
    jsonfile dscript wpo.po_gid ;
    jsonfile dmodel wpo.po_gid ;
  ]

let get wpo =
  let f = filename ~force:false wpo in
  try Hashtbl.find files f
  with Not_found ->
    let script =
      if Filesystem.exists f then Script f else
        try
          let f' = List.find Filesystem.exists (legacies wpo) in
          Wp_parameters.warning ~current:false
            "Deprecated script for '%s'" wpo.po_sid ;
          Deprecated f'
        with Not_found -> NoScript
    in Hashtbl.add files f script ; script

let pp_file fmt s = Filepath.pretty fmt s

let pp_script_for fmt wpo =
  match get wpo with
  | Script f -> Format.fprintf fmt "script '%a'" pp_file f
  | Deprecated f -> Format.fprintf fmt "(deprecated) script '%a'" pp_file f
  | _ -> Format.fprintf fmt "script '%a'" pp_file @@ filename ~force:false wpo

let exists wpo =
  match get wpo with NoScript -> false | Script _ | Deprecated _ -> true

let load wpo =
  match get wpo with
  | NoScript -> `Null
  | Script f | Deprecated f ->
    if Filesystem.exists f then Json.load_file f else `Null

let remove wpo =
  match get wpo with
  | NoScript -> ()
  | Script f ->
    begin
      Filesystem.remove_file f ;
      Hashtbl.replace files f NoScript ;
    end
  | Deprecated f0 ->
    begin
      Wp_parameters.feedback
        "Removed deprecated script for '%s'" wpo.po_sid ;
      Filesystem.remove_file f0 ;
      let f = filename ~force:false wpo in
      Hashtbl.replace files f NoScript ;
    end

let save ~stdout wpo js =
  let empty =
    match js with
    | `Null | `List [] | `Assoc [] -> true
    | _ -> false in
  if stdout then
    Wp_parameters.result "Proof script for %s:@.%a"
      wpo.po_gid (Json.save_formatter ~pretty:true) js
  else
  if empty then remove wpo else
    match get wpo with
    | Script f ->
      Json.save_file f js
    | NoScript ->
      begin
        let f = filename ~force:true wpo in
        Json.save_file f js ;
        Hashtbl.replace files f (Script f) ;
      end
    | Deprecated f0 ->
      begin
        Wp_parameters.feedback
          "Upgraded script for '%s'" wpo.po_sid ;
        Filesystem.remove_file f0 ;
        let f = filename ~force:true wpo in
        Json.save_file f js ;
        Hashtbl.replace files f (Script f) ;
      end

let get_marks_dir ~force =
  let scripts = Wp_parameters.Session.get_dir ~create_path:force "script" in
  let path = Filepath.(scripts / ".marks") in
  if force then Wp_parameters.Output.mkdir path ;
  path

let remove_marks ~dry =
  let marks = get_marks_dir ~force:false in
  if Filesystem.dir_exists marks then
    if dry
    then Wp_parameters.feedback "[dry] remove marks"
    else Filesystem.remove_dir marks

let reset_marks () =
  remove_marks ~dry:false ;
  ignore @@ get_marks_dir ~force:true

let mark goal =
  let marks = get_marks_dir ~force:false in
  if Filesystem.dir_exists marks then
    let mark = Filepath.(marks / (goal.po_sid ^ ".json")) in
    if Filesystem.exists mark then ()
    else close_out @@ open_out (Filepath.to_string_abs mark)

module StringSet = Datatype.String.Set

let remove_unmarked_files ~dry =
  let dir = get_script_dir ~force:false in
  if Filesystem.dir_exists dir then
    let marks = get_marks_dir ~force:false in
    if Filesystem.dir_exists marks then
      begin
        let files = Filesystem.fold_dir StringSet.add dir StringSet.empty in
        let marks = Filesystem.fold_dir StringSet.add marks StringSet.empty in
        let orphans = StringSet.diff files marks in
        let orphans = StringSet.remove ".marks" orphans in
        let remove file =
          let path = Filepath.(dir / file) in
          if dry
          then Wp_parameters.feedback "[dry] rm %a" Filepath.pretty path
          else Filesystem.remove_file path
        in
        StringSet.iter remove orphans ;
        remove_marks ~dry
      end
