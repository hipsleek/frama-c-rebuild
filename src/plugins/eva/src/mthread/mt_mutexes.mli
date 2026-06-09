(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val mutexes_protecting_zones' :
  (Memory_zone.t * Mt_cfg_types.SetNodeIdAccess.t) list ->
  Mt_mutexes_types.MutexesByZone.t
val pretty_with_mutexes :
  Format.formatter -> Mt_shared_vars.Precise.list_accesses -> unit
type protection = Unprotected | Priority | Protected of Mutex.Set.t
val pretty_protection : Format.formatter -> protection -> unit
val pretty_protection_per_thread :
  Format.formatter -> Mt_thread.thread * Mt_thread.thread * protection -> unit
type zone_protection =
  (Memory_zone.t * (Mt_thread.thread * Mt_thread.thread * protection) list)
    list
val pretty_zone_protection :
  Format.formatter ->
  Memory_zone.t * (Mt_thread.thread * Mt_thread.thread * protection) list ->
  unit
val check_protection :
  Mt_thread.analysis_state ->
  Mt_shared_vars.Precise.list_accesses -> zone_protection
val pretty_protections :
  Format.formatter ->
  (Memory_zone.t * (Mt_thread.thread * Mt_thread.thread * protection) list)
    list -> unit
val ill_protected :
  Mt_shared_vars.Precise.list_accesses ->
  zone_protection -> Memory_zone.t Cil_datatype.Stmt.Hashtbl.t
val need_sync :
  'a Cil_datatype.Stmt.Hashtbl.t -> (Cil_datatype.Stmt.Hashtbl.key * 'a) list
