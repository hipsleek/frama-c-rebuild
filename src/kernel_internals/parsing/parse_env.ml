(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* maps filepaths to bools (true iff the file existed at the moment
   the reference was read *)
let referenced_files = Hashtbl.create 7

module SourceFiles =
  State_builder.Hashtbl(Filepath.Hashtbl)(Datatype.String)
    (struct
      let name = "SourceFiles"
      let dependencies = [ Kernel.Files.self ]
      let size = 1
    end)

(* maps .i/.pp files to their workdir (when a JCDB/Mopsa-DB is used) *)
module PreprocessingWorkdir =
  State_builder.Hashtbl(Filepath.Hashtbl)(Filepath)
    (struct
      let name = "PreprocessingWorkdir"
      let dependencies =
        [ Kernel.CompilationDb.self; Kernel.MopsaDb.self ]
      let size = 2
    end)

let set_workdir file workdir =
  PreprocessingWorkdir.replace file workdir

let get_workdir file =
  try
    Some (PreprocessingWorkdir.find file)
  with Not_found -> None

let store_referenced_source fp =
  if not (Hashtbl.mem referenced_files fp) then begin
    try
      let open Filesystem.Operators in
      let$ inchan = Filesystem.with_open_in_exn ~binary:true fp in
      let contents = really_input_string inchan (in_channel_length inchan) in
      SourceFiles.replace fp contents;
      Hashtbl.add referenced_files fp true
    with Sys_error s ->
      Kernel.warning ~once:true ~wkey:Kernel.wkey_file_not_found
        "Cannot find referenced file %a (%s), ignoring" Filepath.pretty fp s;
      Hashtbl.add referenced_files fp false
  end

let scan_source_for_references ~workdir contents =
  let re_hash =
    Str.regexp "^#[ \\t]*\\(line\\)?[ \\t]*[0-9]+[ \\t]+\"\\([^<>]*\\)\"[ \\t]+[0-9]*[ \\t]*$"
  in
  let lines = String.split_on_char '\n' contents in
  List.iter (fun line ->
      if Str.string_match re_hash line 0 then
        let file = Str.matched_group 2 line in
        let file =
          if String.contains file '"' then
            (* Special case: the filename contains double quotes;
               when this happens, the matched regex contains an extra backslash
               that must be removed. In other words, we must undo the quoting
               introduced by the C preprocessor. *)
            Str.global_replace (Str.regexp "\\\\\"") "\"" file
          else file
        in
        let file = if Filename.is_relative file && workdir <> None then
            Filepath.concat (Option.get workdir) file
          else Filepath.of_string file
        in
        store_referenced_source file
    ) lines

let open_source ~scan_references fp =
  try
    let s = SourceFiles.find fp in
    Ok s
  with Not_found ->
  try
    Kernel.feedback ~dkey:Kernel.dkey_file_source
      "opening source file: %a"
      Filepath.pretty fp;
    let open Filesystem.Operators in
    let$ inchan = Filesystem.with_open_in_exn ~binary:true fp in
    let contents = really_input_string inchan (in_channel_length inchan) in
    SourceFiles.replace fp contents;
    let workdir = PreprocessingWorkdir.find_opt fp in
    if scan_references then
      scan_source_for_references ~workdir contents;
    Ok contents
  with Sys_error s ->
    Error s

