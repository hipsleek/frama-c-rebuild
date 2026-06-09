(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let concurrent_writes thread shared_bases =
  let module Engine = (val Engine.current ()) in
  let add_pos stmt acc (cs, _state) =
    if cs.Callstack.thread <> Thread.id thread
    then acc
    else
      let pos = Position.local stmt cs in
      (* TODO: Maybe take the memory read/written for all callstacks of the
         given statement? (can be done directly by Inout_access). *)
      let filter = Inout_access.keep_globals_only in
      let accesses = Inout_access.at ~filter pos in
      let written_bases = Memory_zone.get_bases accesses.write in
      if Base.SetLattice.(intersects (inject shared_bases) written_bases)
      then Position.Local.Set.add (stmt, cs) acc
      else acc
  in
  let add_stmt acc stmt =
    let is_write_stmt = match stmt.Cil_types.skind with
      | Cil_types.Instr (Set _ | Call _ | Local_init _) -> true
      | _ -> false
    in
    if is_write_stmt
    then match Engine.get_state_by_callstack (After stmt) with
      | `Top | `Bottom -> acc (* TODO: handle Tops *)
      | `Value list -> List.fold_left (add_pos stmt) acc list
    else acc
  in
  let add_kf kf acc =
    match kf.Cil_types.fundec with
    | Declaration _ -> acc
    | Definition (fundec,_) ->
      List.fold_left add_stmt acc fundec.Cil_types.sallstmts
  in
  Globals.Functions.fold add_kf Position.Local.Set.empty

let shared_bases analysis_state =
  let shared_zones = analysis_state.Mt_thread.concurrent_accesses in
  match Memory_zone.get_bases shared_zones with
  | Top -> assert false
  | Set zones ->  zones

let add_last_analysis (analysis_state : Mt_thread.analysis_state) =
  let thread = analysis_state.curr_thread.th_eva_thread in
  let module Engine = (val Engine.current ()) in
  let bases = shared_bases analysis_state in
  let writes = concurrent_writes thread bases in
  match Engine.Interferences.add_last_analysis thread writes bases with
  | Updated ->
    Mt_thread.iter_threads analysis_state
      (fun th -> Mt_thread.ThreadState.recompute_because th InterferencesChanged)
  | NoChanges ->
    ()
