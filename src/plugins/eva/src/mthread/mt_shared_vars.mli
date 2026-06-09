(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type Computer =
sig
  module Access : Datatype.S
  module Set: Lattice_type.Lattice_Set with type O.elt = Access.t
  module ZoneMap: Lmap_bitwise.Location_map_bitwise with type v = Set.t

  type list_accesses = (Memory_zone.t * Set.t) list

  val pretty_concurrent_accesses :
    ?f:Access.t Pretty_utils.formatter ->
    unit -> Format.formatter -> list_accesses -> unit

  val all_zones_accessed : list_accesses -> Memory_zone.t

  val concurrent_accesses_all_threads :
    Mt_thread.ThreadState.t list ->
    (list_accesses * list_accesses) * ZoneMap.map
end

module Global : Computer
  with module Access = Mt_shared_vars_types.StmtIdAccess
   and module Set = Mt_shared_vars_types.SetStmtIdAccess

module Precise :
sig
  include Computer
    with module Access = Mt_cfg_types.NodeIdAccess
     and module Set = Mt_cfg_types.SetNodeIdAccess

  val display_shared_vars_value : ZoneMap.map -> unit
  val enumerate_written_vars_value :
    ZoneMap.map ->
    (Thread.t * Base.t * Cvalue.V_Offsetmap.t) list
  val join_shared_values :
    ('a * Base.t * Cvalue.Model.offsetmap) list -> Cvalue.Model.t
  val remove_non_concur_zones_from_cfg :
    Memory_zone.t -> Mt_cfg_types.CfgNode.t -> unit
  val mark_concur_access_in_cfg :
    ('a * Set.t) list -> unit
end

val read_written_by_thread :
  ?watch_only:Memory_zone.t ->
  (Cil_types.stmt -> bool) ->
  Thread.t ->
  Mt_shared_vars_types.AccessesByZone.map

val register_concurrent_var_accesses :
  Mt_thread.analysis_state ->
  [< `Final of Mt_memory.Types.functions_states
  | `Leaf of Mt_memory.Types.state ] ->
  unit

val stmt_is_multithreaded :
  Mt_thread.analysis_state ->
  Mt_memory.Types.state_accesser -> Cil_types.stmt -> bool
