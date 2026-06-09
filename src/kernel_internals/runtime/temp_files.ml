(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Filesystem

let cleanup_at_exit f = Extlib.safe_at_exit (fun () -> remove_file f)

let should_keep_temp_file = function
  | None -> Kernel.KeepTempFiles.get ()
  | Some b -> b

let file ?keep ~prefix ~suffix () =
  let file =
    try
      temp_file ~prefix ~suffix
    with Sys_error s ->
      Kernel.abort "Cannot open temporary file: %s" s
  in
  Extlib.safe_at_exit
    begin fun () ->
      if not (should_keep_temp_file keep) then
        remove_file file
      else if exists file then
        Kernel.debug "Not removing file %a@." Filepath.pretty file
    end;
  file

let dir ?keep ~prefix ~suffix () =
  let dir =
    try
      temp_dir ~prefix ~suffix
    with Sys_error s ->
      Kernel.abort "Cannot create temporary dir: %s" s
  in
  Extlib.safe_at_exit
    begin fun () ->
      if not (should_keep_temp_file keep) then
        remove_dir dir
      else if exists dir then
        Kernel.debug  "Not removing dir %a@." Filepath.pretty dir;
    end;
  dir
