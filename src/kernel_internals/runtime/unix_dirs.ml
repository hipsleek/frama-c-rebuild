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
  let location =
    match Sys.getenv_opt env with
    | Some env when env <> "" -> Filepath.of_string env
    | _ -> Filepath.concats (home ()) default
  in
  Filepath.(location / "frama-c")

let cache () =
  env_or_default "XDG_CACHE_HOME" [ ".cache" ]
let config () =
  env_or_default "XDG_CONFIG_HOME" [ ".config" ]
let state () =
  env_or_default "XDG_STATE_HOME" [ ".local" ; "state" ]
