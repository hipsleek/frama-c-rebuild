(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Summary of an Mthread analysis, stored on disk for the Ivette component. *)

open Mt_types
open Mt_shared_vars_types

type mutex_summary = {
  taken : Mutex.Set.t;
  released : Mutex.Set.t;
}

type queue_summary = {
  created : Mqueue.Set.t;
  receivers : Mqueue.Set.t;
  senders : Mqueue.Set.t;
}

type shared_var_summary = {
  read : Memory_zone.Set.t;
  written : Memory_zone.Set.t;
}

type thread_summary = {
  locks : mutex_summary;
  mqueues : queue_summary;
  shared_vars : shared_var_summary;
}

(* ----- Datatypes for all above types. ----------------------------------- *)

module MutexSummary = struct
  include Datatype.Serializable_undefined

  type t = mutex_summary

  let empty = Mutex.Set.{ taken = empty; released = empty }

  let name = "Mt_summary.MutexSummary"
  let reprs = [ empty ]
  let structural_descr =
    let descr = Mutex.Set.packed_descr in
    Structural_descr.t_record [| descr; descr; |]
end

module QueueSummary = struct
  include Datatype.Serializable_undefined

  type t = queue_summary

  let empty = Mqueue.Set.{ created = empty; receivers = empty; senders = empty }

  let name = "Mt_summary.QueueSummary"
  let reprs = [ empty ]
  let structural_descr =
    let descr = Mqueue.Set.packed_descr in
    Structural_descr.t_record [| descr; descr; descr; |]
end

module SharedVarSummary = struct
  include Datatype.Serializable_undefined

  type t = shared_var_summary

  let empty = Memory_zone.Set.{ read = empty; written = empty }

  let name = "Mt_summary.SharedVarSummary"
  let reprs = [ empty ]
  let structural_descr =
    let descr = Memory_zone.Set.packed_descr in
    Structural_descr.t_record [| descr; descr; |]
end

module MutexSummaryDatatype = Datatype.Make (MutexSummary)
module QueueSummaryDatatype = Datatype.Make (QueueSummary)
module SharedVarSummaryDatatype = Datatype.Make (SharedVarSummary)

module ThreadSummary = struct
  include Datatype.Serializable_undefined

  type t = thread_summary

  let empty =
    { locks = MutexSummary.empty;
      mqueues = QueueSummary.empty;
      shared_vars = SharedVarSummary.empty; }

  let name = "Mt_summary.ThreadSummary"
  let reprs = [ empty ]
  let structural_descr =
    Structural_descr.t_record [| MutexSummaryDatatype.packed_descr;
                                 QueueSummaryDatatype.packed_descr;
                                 SharedVarSummaryDatatype.packed_descr; |]
end

module ThreadSummaryDatatype = Datatype.Make (ThreadSummary)


(* ----- Computation of the summary of one thread --------------------------- *)

let add_lock_taken id th_summary =
  let taken = Mutex.Set.add id th_summary.locks.taken in
  let locks = { th_summary.locks with taken } in
  { th_summary with locks }

let add_lock_released id th_summary =
  let released = Mutex.Set.add id th_summary.locks.released in
  let locks = { th_summary.locks with released } in
  { th_summary with locks }

let add_mqueue_created id th_summary =
  let created = Mqueue.Set.add id th_summary.mqueues.created in
  let mqueues = { th_summary.mqueues with created } in
  { th_summary with mqueues }

let add_mqueue_received_from id th_summary =
  let receivers = Mqueue.Set.add id th_summary.mqueues.receivers in
  let mqueues = { th_summary.mqueues with receivers } in
  { th_summary with mqueues }

let add_mqueue_sent_to id th_summary =
  let senders = Mqueue.Set.add id th_summary.mqueues.senders in
  let mqueues = { th_summary.mqueues with senders } in
  { th_summary with mqueues }

let add_shared_var_read zone th_summary =
  let read = Memory_zone.Set.add zone th_summary.shared_vars.read in
  let shared_vars = { th_summary.shared_vars with read } in
  { th_summary with shared_vars }

let add_shared_var_written zone th_summary =
  let written = Memory_zone.Set.add zone th_summary.shared_vars.written in
  let shared_vars = { th_summary.shared_vars with written } in
  { th_summary with shared_vars }


let compute_thread_summary thread =
  Trace.fold' thread.Mt_thread.th_amap
    (fun action acc ->
       match action with
       | MutexLock id -> add_lock_taken id acc
       | MutexRelease id -> add_lock_released id acc
       | CreateQueue (id, _) -> add_mqueue_created id acc
       | ReceiveMsg (id, _, _) -> add_mqueue_received_from id acc
       | SendMsg (id, _) -> add_mqueue_sent_to id acc
       | VarAccess (Read, zone) -> add_shared_var_read zone acc
       | VarAccess (Write _, zone) -> add_shared_var_written zone acc
       | _ -> acc)
    ThreadSummary.empty


(* ----- Computation of the summary of one access node set ------------------ *)

(* Does [mutex] protect all [rw] accesses according to [mutexes]. *)
let is_protected (mutexes: Mt_mutexes_types.mutexes_by_access) rw mutex =
  let mutexes =
    match rw with
    | Read | ReadPos _ -> mutexes.mutexes_for_read
    | Write _ | WritePos _ -> mutexes.mutexes_for_write
  in
  match mutexes with
  | Unaccessed -> true (* Should not happen *)
  | Mutexes m -> MutexPresence.find m mutex = Present

module StmtSet = Cil_datatype.Stmt.Set

module AccessProperty = Datatype.Pair_with_collections (AccessKind) (Protection)

(* Map binding access property (kind+protection) to a set of locations. *)
module LocationsByAccessProperty = struct
  include AccessProperty.Map
  include Make (StmtSet)

  let join = union (fun _key a b -> Some (StmtSet.union a b))

  let is_included l r =
    let is_included_binding key elt =
      try StmtSet.subset elt (find key r)
      with Not_found -> false
    in
    for_all is_included_binding l

  let get_access_kind = function
    | Read | ReadPos _ -> AccessRead
    | Write _ | WritePos _ -> AccessWrite

  (* Computes the map (property -> locations) corresponding to the cfg node
     of a memory access. *)
  let compute zone_mutexes rw cfg_node : t =
    let access_kind = get_access_kind rw in
    let stmts = Mt_cfg_types.CfgNode.node_stmt cfg_node |> StmtSet.of_list in
    let locked_mutexes = cfg_node.Mt_cfg_types.cfgn_context.locked_mutexes in
    if MutexPresence.is_empty locked_mutexes then
      singleton (access_kind, Unprotected) stmts
    else
      let add_mutex mutex =
        let protection =
          if is_protected zone_mutexes rw mutex
          then Protected mutex
          else MaybeProtected mutex
        in
        add (access_kind, protection) stmts
      in
      let all_mutex = MutexPresence.all_present locked_mutexes in
      MutexPresence.KeySet.fold add_mutex all_mutex empty
end

(** Map zone -> access property (kind+protection) -> set of locations. *)
module AccessPropertyByZone = struct
  module Lattice = struct
    include Lattice_bounds.Top.Bound_Lattice (LocationsByAccessProperty)
    let default = `Value LocationsByAccessProperty.empty
    let default_is_bottom = true
  end

  include Lmap_bitwise.Make_bitwise (Lattice)

  (* Applies [f] on each (zone, access_kind, protection, locations) of [map]. *)
  let iter f map =
    let iter_zone f zone =
      let apply base itvs () = f (Memory_zone.inject base itvs) in
      try Memory_zone.fold_i apply zone ()
      with Abstract_interp.Error_Top -> f Memory_zone.top
    in
    let iter_access f accesses =
      LocationsByAccessProperty.iter
        (fun (kind, protection) locations -> f kind protection locations)
        accesses
    in
    fold
      (fun zones accesses () ->
         let accesses = Eval.Top.non_top accesses in
         iter_zone (fun zone -> iter_access (f zone) accesses) zones)
      map ()

  (* Computes the map corresponding to a set of accesses of a memory zone. *)
  let compute_for_zone mutexes_by_zone (acc : t) (zone, node_access_set) : t =
    (* [mutexes] contains the mutexes for all accesses to [zone], not only
       the current one from [node_access_set]. *)
    let mutexes = Mt_mutexes_types.MutexesByZone.find mutexes_by_zone zone in
    (* By construction, the zone is in the MutexesByZone *)
    let zone_mutexes = Eval.Bottom.non_bottom mutexes in
    Mt_cfg_types.SetNodeIdAccess.fold
      (fun (rw, node, _) acc ->
         let lba = LocationsByAccessProperty.compute zone_mutexes rw node in
         (* Use [exact:false] so that a weak update is performed, as we want
            the join of maps from LocationsByAccessProperty for all accesses. *)
         add_binding ~exact:false acc zone (`Value lba))
      node_access_set
      acc

  (* Computes the map corresponding to all accesses from an analysis. *)
  let compute analysis =
    let accesses = analysis.Mt_thread.concurrent_accesses_by_nodes in
    let mutexes_by_zone = Mt_mutexes.mutexes_protecting_zones' accesses in
    let r1 = List.fold_left (compute_for_zone mutexes_by_zone) empty accesses in
    match r1 with
    | Top | Bottom ->
      Mt_self.fatal "By construction, accesses_by_zone cannot be Top or Bottom"
    | Map m -> m
end


(* ----- Summary for all threads -------------------------------------------- *)

let info name : (module State_builder.Info_with_size) =
  (module struct
    let name = "Eva.Mt_summary." ^ name
    let size = 11
    let dependencies = [ Self.state ]
  end)

module ThreadTable =
  State_builder.Hashtbl
    (Thread.Hashtbl) (ThreadSummaryDatatype) (val info "ThreadTable")

let compute_threads_summary analysis =
  ThreadTable.clear ();
  let threads = Mt_thread.threads analysis in
  List.iter
    (fun thread ->
       let thread_summary = compute_thread_summary thread in
       ThreadTable.replace thread.th_eva_thread thread_summary)
    threads;
  ThreadTable.mark_as_computed ()


(* ----- Summary for all accesses ------------------------------------------- *)

module Access =
  Datatype.Triple_with_collections (Memory_zone) (AccessKind) (Protection)

type access = Access.t

let access_zone (zone, _, _) = zone
let access_kind (_, kind, _) = kind
let access_protection (_, _, protection) = protection
let access_id access = Format.asprintf "%a" Access.pretty access

module AccessTable =
  State_builder.Hashtbl (Access.Hashtbl) (StmtSet) (val info "AccessTable")

let compute_access_summary analysis =
  let accesses_by_zone = AccessPropertyByZone.compute analysis in
  AccessTable.clear ();
  AccessPropertyByZone.iter
    (fun zone kind protection locs ->
       AccessTable.replace (zone, kind, protection) locs)
    accesses_by_zone;
  AccessTable.mark_as_computed ()


(* ----- Summary for everything --------------------------------------------- *)

let compute analysis =
  compute_threads_summary analysis;
  compute_access_summary analysis

let clear () =
  ThreadTable.clear ();
  AccessTable.clear ();
