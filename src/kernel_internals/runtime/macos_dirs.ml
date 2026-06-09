(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let home () =
  match Sys.getenv "HOME" with
  | "" -> raise Not_found
  | s -> Filepath.of_string s

let env_or_default env default =
  match Sys.getenv_opt env with
  | Some s when s <> "" -> Filepath.(of_string s / "frama-c")
  | _ -> Filepath.concats (home ()) default

let cache () =
  env_or_default "XDG_CACHE_HOME" [ "Library" ; "Caches" ; "frama-c"]
let config () =
  env_or_default "XDG_CONFIG_HOME" [ "Application Support" ; "frama-c" ; "config" ]
let state () =
  env_or_default "XDG_STATE_HOME" [ "Application Support" ; "frama-c" ; "state" ]
