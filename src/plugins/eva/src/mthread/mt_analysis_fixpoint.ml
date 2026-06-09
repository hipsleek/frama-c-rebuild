(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Mt_types
open Mt_shared_vars_types
open Mt_mutexes_types
open Mt_thread


(* If the thread sends more messages than before, we flag all the threads
   receiving messages on those queues as needed to be recomputed *)
let mark_new_messages_received analysis =
  let th = analysis.curr_thread in
  let is_send = function SendMsg _ -> true | _ -> false in
  let send_before = Trace.find_events is_send th.th_amap
  and send_after = Trace.find_events is_send (curr_events analysis) in
  (* YYY Not monotonic *)
  let diff = EventsSet.diff send_after send_before in
  if not (EventsSet.is_empty diff) then
    let queues = EventsSet.fold
        (fun evt queues -> match evt with
           | SendMsg (q, _) -> Mqueue.Set.add q queues
           | _ -> queues) diff Mqueue.Set.empty
    in
    Mt_self.debug "@[New message(s) sent@ on@ queue(s) %a@]"
      (Pretty_utils.pp_iter Mqueue.Set.iter Mqueue.pretty) queues;
    iter_threads analysis
      (fun th ->
         let should_recompute _stack = function
           | ReceiveMsg (q, _, _) -> Mqueue.Set.mem q queues
           | _ -> false
         in
         if Trace.exists th.th_amap should_recompute
         then (Mt_self.debug "Marking %a as having received new message(s)"
                 ThreadState.pretty th;
               ThreadState.recompute_because th NewMsgReceived)
      );
;;

let post_thread_analysis analysis =
  let th = analysis.curr_thread in

  (* (Temporary) hack to be able to retrieve temporary analysis results *)
  let previous_computation_state = Self.ComputationState.get () in
  Self.ComputationState.set Computed;

  mark_new_messages_received analysis;

  (* We compute the globals variables accessed by the thread *)
  Mt_self.feedback ~level:2 "* Computing shared variables";
  let state_accesser = Mt_memory.Types.Global in
  let read_written =
    Mt_shared_vars.read_written_by_thread
      (Mt_shared_vars.stmt_is_multithreaded analysis state_accesser)
      th.th_eva_thread
  in
  th.th_read_written <- read_written;
  Mt_self.result ~level:3 "@[<v 0>Globals accessed by thread:@ %a@]"
    AccessesByZone.pretty_map read_written;
  Mt_self.feedback ~level:2 "* shared variables computed";

  (* We compute interferences *)
  Mt_interferences.add_last_analysis analysis;

  (* We update the multithread events of the thread for its next iteration *)
  th.th_amap <- curr_events analysis;

  (* Compute the concurrent graph of this thread *)
  Mt_self.feedback ~level:2 "* Computing cfg";
  th.th_cfg <- Mt_cfg.make_cfg th;
  th.th_read_written_cfg <- Mt_cfg.cfg_accesses th.th_eva_thread th.th_cfg;
  Mt_self.feedback ~level:2 "* Cfg computed";

  Mt_self.feedback "*** Thread %a computed" ThreadState.pretty th;

  (* (Temporary) hack to be able to retrieve temporary analysis results *)
  Self.ComputationState.set previous_computation_state

(* We compute a value analysis for the given thread *)
let pre_thread_analysis analysis th =
  Mt_self.feedback
    "@[<hov 2>*** Computing thread %a,@ iteration %d@ (%a)@]"
    ThreadState.pretty th analysis.iteration
    SetRecomputeReason.pretty th.th_to_recompute;

  Mt_self.feedback ~level:2 "* Computing value analysis for thread %a"
    Thread.pretty th.th_eva_thread;
  Mt_self.debug "@[<hov>Arguments@ %a@]"
    (Pretty_utils.pp_list Cvalue.V.pretty) th.th_params;
  Mt_self.debug ~level:2 "Initial state %a"
    Cvalue.Model.pretty th.th_init_state;

  (* We set the values that depend on the thread analysed *)
  analysis.curr_thread <- th;
  analysis.curr_events_stack <- [];
  Datatype.Int.Hashtbl.clear analysis.memexec_cache



let recompute_shared_vars_changed analysis before =
  iter_threads analysis
    (fun th ->
       try AccessesByZone.fold
             (fun z _ () ->
                if not (Memory_zone.is_included z before) then raise Exit)
             th.th_read_written ()
       with Exit -> ThreadState.recompute_because th PotentialSharedVarsChanged
    )

(** Recompute all the threads that are not [th], and that read a value
    that has changed between [before] and [now] *)
let recompute_shared_vars_values_changed analysis th before now =
  let changed_zone b offsm z =
    (* b is present in [now] but not in [before], or has changed: add the
       entire base to the changed_zone *)
    let default () =
      let zb = Memory_zone.inject b Int_Intervals.top in
      Memory_zone.join z zb
    in
    try
      match Cvalue.Model.find_base b before with
      | `Top | `Bottom -> assert false
      | `Value offsm' ->
        if Cvalue.V_Offsetmap.equal offsm offsm' then z
        else default ()
    with Not_found -> default ()
  in
  match now with
  | Cvalue.Model.Top | Cvalue.Model.Bottom -> assert false
  | Cvalue.Model.Map now ->
    (* Over-approximation of the zones changed from [before] to [now] *)
    let z_changed =
      Cvalue.Model.fold changed_zone now Memory_zone.bottom
    in
    iter_threads analysis
      (fun th' ->
         if not (ThreadState.equal th' th) then
           try
             AccessesByZone.fold
               (fun z accesses () ->
                  if Memory_zone.intersects z_changed z &&
                     (* YYY: recompute also threads that only write the variable?*)
                     (SetStmtIdAccess.exists
                        (fun (op, _, _) -> RW.is_read op)
                        accesses)
                  then begin
                    ThreadState.recompute_because th' SharedVarsValuesChanged;
                    raise Exit (* Speed up things, th' will be recomputed *)
                  end)
               th'.th_read_written ()
           with Exit -> ()
      )
;;


let compute_shared_vars analysis =
  let _imprecise =
    Mt_self.feedback "***** Computing shared variables";
    let (ww_accesses, rw_accesses), _ =
      Mt_shared_vars.Global.concurrent_accesses_all_threads
        (threads analysis) in
    let accesses = ww_accesses @ rw_accesses in
    Mt_self.debug ~level:2 "Global concurrent var accesses:@.%a"
      (Mt_shared_vars.Global.pretty_concurrent_accesses ()) accesses;
    let all_zones = Mt_shared_vars.Global.all_zones_accessed accesses in
    Mt_self.result ~level:3 "@[<hov 2>Imprecise locations to watch: %a@]"
      Memory_zone.pretty all_zones;

    (* Detect changes *)
    if not (Memory_zone.equal all_zones analysis.concurrent_accesses)
    then (
      let before = analysis.concurrent_accesses in
      Mt_self.feedback ~level:2 "@[<v>Concurrent imprecise accesses have \
                                 changed: before@ @[<hov 2>  %a@]@ vs.@ @[<hov 2>  %a@]"
        Memory_zone.pretty before Memory_zone.pretty all_zones;
      let after = Memory_zone.join before all_zones in
      analysis.concurrent_accesses <- after;
      recompute_shared_vars_changed analysis before;
    )
  in

  (* Precise computation. Very similar to the above code, we just compute,
     store and print things differently *)
  let precise =
    let (ww_accesses, rw_accesses), zmap =
      Mt_shared_vars.Precise.concurrent_accesses_all_threads
        (threads analysis) in
    if Mt_options.DumpSharedVarsValues.get () > 0 then
      Mt_shared_vars.Precise.display_shared_vars_value zmap;
    let written = Mt_shared_vars.Precise.enumerate_written_vars_value zmap in
    let all_accesses = ww_accesses @ rw_accesses in
    let header fmt = Format.fprintf fmt "Possible read/write data races:" in
    Mt_self.printf ~level:1 ~header "  @[<v 0>%a@]"
      Mt_mutexes.pretty_with_mutexes rw_accesses;
    if Mt_options.WriteWriteRaces.get () then begin
      let header fmt = Format.fprintf fmt "Possible write/write data races:" in
      Mt_self.printf ~level:1 ~header "  @[<v 0>%a@]"
        Mt_mutexes.pretty_with_mutexes ww_accesses;
    end;
    let all_zones = Mt_shared_vars.Precise.all_zones_accessed (ww_accesses @ rw_accesses) in
    Mt_self.result ~level:2 "@[<hov 2>Shared memory:@ %a@]"
      Memory_zone.pretty all_zones;

    (* Detect changes *)
    if not (Memory_zone.equal all_zones analysis.precise_concurrent_accesses)
    then (
      let before = analysis.precise_concurrent_accesses in
      Mt_self.feedback ~level:2
        "@[<v>Concurrent precise var accesses have changed: before@ \
         @[<hov 2>  %a@]@ \
         vs.@ \
         @[<hov 2>  %a@]@]"
        Memory_zone.pretty before Memory_zone.pretty all_zones;
      (* let after = Memory_zone.join before all_zones in *)
      analysis.precise_concurrent_accesses <- all_zones;
      (* No need to recompute for the moment, this field is not used by
         the analysis *)
    );
    all_accesses, written
  in
  precise
;;

(* Update the th_values_written field of all the threads, using the
   list of concurrent accesses that is returned by the shared var analysis.

   This function must be called once the [th_read_written] fields have been
   updated to ensure correct convergence *)
let store_written_value analysis lw =
  let aux th =
    let l = List.filter (fun (id, _, _) -> Thread.equal id th.th_eva_thread) lw in
    let old_written = th.th_values_written in
    let written = Mt_shared_vars.Precise.join_shared_values l in
    (* XXX: temporary *)
    let priority, hint =
      Widen_type.hints_from_keys Cil_datatype.Stmt.dummy (Widen_type.default ())
    in
    let written = Cvalue.Model.widen ~priority ~hint old_written written in
    let changed = not (Cvalue.Model.equal written old_written) in
    if changed then
      recompute_shared_vars_values_changed analysis th old_written written;
    if Mt_options.DumpSharedVarsValues.get () > 0 &&
       not (Cvalue.Model.equal Cvalue.Model.empty_map written)
    then
      Mt_self.result "@[Write summary for %a%t:@ %a@]"
        ThreadState.pretty th
        (fun fmt -> if changed then Format.fprintf fmt " (updated)")
        Cvalue.Model.pretty written;
    th.th_values_written <- written
  in
  iter_threads analysis aux

let save_to_disk analysis =
  if Mt_options.ToDisk.get () then begin
    let filepath =
      let prefix = Mt_options.ToDiskPrefix.get () in
      Filepath.of_format "%siteration_%d.sav" prefix analysis.iteration
    in
    Project.save filepath;
    Mt_self.feedback "* Saved iteration %d to file %S" analysis.iteration
      (Filepath.to_string_rel filepath);
  end

let post_iteration analysis =
  (* We update the locked mutexes and started threads information of the
     cfg. This must obviously be done before shared variables are computed,
     but it supposes the thread creation structure is completely known.
     Hence, it is safer to do this at the end of a full iteration, instead
     of at the end of a thread *)
  Mt_self.feedback ~level:2 "* Computing live threads and locked mutexes";
  iter_threads analysis (Mt_cfg.update_cfg_contexts analysis);
  Mt_self.feedback ~level:2 "* threads and mutexes computed";

  let precise_accesses, written = compute_shared_vars analysis in
  analysis.concurrent_accesses_by_nodes <- precise_accesses;
  store_written_value analysis written;

  let mutexes = Mt_mutexes.mutexes_protecting_zones' precise_accesses in
  Mt_self.result "@[<v 0>Mutexes for concurrent accesses:@ %a@]"
    MutexesByZone.pretty mutexes;
  if Mt_options.CheckProtections.get () then begin
    let protections = Mt_mutexes.check_protection analysis precise_accesses in
    Mt_self.result "Detailed shared zones protections@.%a"
      Mt_mutexes.pretty_protections protections;
    let ill_protected = Mt_mutexes.ill_protected precise_accesses protections in
    let need_sync = Mt_mutexes.need_sync ill_protected in
    if need_sync <> [] then begin
      let pp fmt (stmt, z) =
        Format.fprintf fmt "@[%a (for %a)@]"
          Fileloc.pretty (Cil_datatype.Stmt.loc stmt)
          Memory_zone.pretty z
      in
      Mt_self.result "Statements needing manual synchronisation@.%a"
        (Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@ " ~suf:"@]" pp) need_sync
    end;
  end;
  Mt_self.feedback "***** Shared variables computed";

  save_to_disk analysis

(* Remove "white" nodes in the cfg, ie accesses to variables that
   are not concurrent at all. Done at the very end of the analysis
   because
   - those nodes are needed before to reach the fixpoint
   - the marking of nodes by colors is not used by the analysis
     YYY: this can endanger restarting the analysis from the last point
     (the fixpoint may not be reached immediately, or we might reach a wrong
     one concerning shared variables). This should probably be done in
     a copy of the cfgs, but this means rewriting a fair amount of other
     analysis structures too *)
let mark_shared_nodes_kind analysis =
  let precise_accesses = analysis.concurrent_accesses_by_nodes in
  let shared_vars = Mt_shared_vars.Precise.all_zones_accessed precise_accesses in
  (* Update the information in the cfgs *)
  iter_threads analysis
    (fun th -> Mt_shared_vars.Precise.remove_non_concur_zones_from_cfg
        shared_vars th.th_cfg
    );
  Mt_shared_vars.Precise.mark_concur_access_in_cfg precise_accesses;
  if (not (Mt_options.KeepWhiteNodes.get ()) ||
      not (Mt_options.KeepGreenNodes.get ()))
  && not (Mt_options.FullCfg.get ())
  then
    iter_threads analysis
      (fun th ->
         let keep =
           match Mt_options.KeepWhiteNodes.get (),
                 Mt_options.KeepGreenNodes.get () with
           | false, false -> Mt_cfg_types.ConcurrentAccess
           | false, true  -> Mt_cfg_types.SharedVarNonConcurrentAccess
           | true,  true  -> Mt_cfg_types.NotReallySharedVar
           | true,  false ->
             Mt_self.warning ~once:true
               "Incoherent@ combination@ of@ options@ %s@ \
                and@ %s.@ Only@ non-shared@ variables@ will@ be@ removed."
               Mt_options.KeepWhiteNodes.option_name
               Mt_options.KeepGreenNodes.option_name;
             Mt_cfg_types.SharedVarNonConcurrentAccess
         in
         let cfg = Mt_cfg.remove_superfluous_nodes ~keep th.th_cfg in
         th.th_cfg <- cfg;
      )
