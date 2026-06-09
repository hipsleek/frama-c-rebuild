(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_datatype

let tracking_stmt fold mk_stmt env kf vars =
  if Functions.instrument kf then
    fold
      (fun vi env ->
         if Memory_tracking.must_monitor_vi ~kf vi then
           Env.add_stmt env (mk_stmt vi)
         else
           env)
      vars
      env
  else
    env

let store env kf vars =
  tracking_stmt
    List.fold_right (* small list *)
    Smart_stmt.store_stmt
    env
    kf
    vars

let duplicate_store env kf vars =
  tracking_stmt
    Varinfo.Set.fold
    Smart_stmt.duplicate_store_stmt
    env
    kf
    vars

let delete_from_list env kf vars =
  tracking_stmt
    List.fold_right (* small list *)
    Smart_stmt.delete_stmt
    env
    kf
    vars

let delete_from_set env kf vars =
  tracking_stmt
    Varinfo.Set.fold
    Smart_stmt.delete_stmt
    env
    kf
    vars
