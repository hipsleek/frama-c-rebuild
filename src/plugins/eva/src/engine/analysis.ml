(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type 'state engine = (module Engine_sig.S with type Dom.state = 'state)

(* ----- Pre-analysis checks ------------------------------------------------ *)

(* Clear Eva's various caches. Some operations of Eva depend on parameters,
   such as -ilevel or -plevel, so clearing those caches ensures that those
   options have the expected effect.
   Caches are cleared at the beginning of each analysis, and whenever the
   Frama-C project library changes the local state of Eva. *)
let clear_caches () =
  Cvalue.V_Offsetmap.clear_caches ();
  Cvalue.Model.clear_caches ();
  Addresses.Bytes.clear_caches ();
  Memory_zone.clear_caches ();
  Assigns.Memory.clear_caches ()

let () = State.add_hook_on_update Self.state clear_caches

let floats_ok () =
  let u = min_float /. 2. in
  let u = u /. 2. in
  assert (0. < u && u < min_float)

let need_assigns kf =
  let spec = Annotations.funspec kf in
  match Cil.find_default_behavior spec with
  | None -> true
  | Some bhv -> bhv.b_assigns = WritesAny

(* Check that we can parse the values specified for the options that require
   advanced parsing. Just make a query, as this will force the kernel to
   parse them. *)
let options_ok () =
  let check f = try ignore (f ()) with Not_found -> () in
  check Parameters.SplitReturnFunction.get;
  check Parameters.BuiltinsOverrides.get;
  check Parameters.SlevelFunction.get;
  check Parameters.EqualityCallFunction.get

let plugins_ok () =
  if not (Plugin.is_present "inout") then
    Self.warning
      "The inout plugin is missing: some features are disabled, \
       and the analysis may have degraded precision and performance."

(* Do something tasteless in case the user did not put a spec on functions
   for which he set [-eva-use-spec]:  generate an incorrect one ourselves *)
let generate_specs () =
  let aux kf =
    if need_assigns kf then begin
      Self.warning ~wkey:Self.wkey_missing_assigns
        "@[No assigns specified for function '%a' for which option %s is set. \
         Generating potentially incorrect assigns.@]"
        Kernel_function.pretty kf Parameters.UseSpec.option_name;
      Populate_spec.populate_funspec ~do_body:true kf [`Assigns];
    end
  in
  Parameters.UseSpec.iter aux

let pre_analysis () =
  Self.clear_results ();
  Ast.compute ();
  Self.configure_verbosity ();
  Parameters.configure_precision ();
  Signal.reset ();
  floats_ok ();
  options_ok ();
  plugins_ok ();
  Split_return.pretty_strategies ();
  generate_specs ();
  Widen.precompute_widen_hints ();
  Builtins.prepare_builtins ();
  Statistics.reset_all ();
  clear_caches ();
  Eva_utils.DegenerationPoints.clear ();
  Cvalue_callbacks.apply_at_start_hooks ();
  Origin.clear ();

  (* Engine can now be rebuilt *)
  let module Engine = (val Engine.reset ()) in
  Engine.Interferences.reset ();
  Thread.reset_state ();
  Mutex.reset_state ();
  Mqueue.reset_state ();
  Mt_summary.clear ();

  if not (Kernel.AuditCheck.is_empty ()) then
    Eva_audit.check_configuration (Kernel.AuditCheck.get ());

  (module Engine : Engine_sig.S)


(* ----- Post-analysis cleanup ---------------------------------------------- *)

let post_analysis (type t) (engine: t engine) final_state =
  (* Garbled mix must be dumped here -- at least before the call to
     mark_green_and_red -- because fresh ones are created when re-evaluating
     all the alarms, and we get an unpleasant "ghost effect". *)
  Self.warning ~wkey:Self.wkey_garbled_mix_summary "%t" Origin.pretty_history;
  (* Mark unreachable and RTE statuses. Only do this there, not when the
     analysis was aborted (hence, not in post_cleanup), because the
     propagation is incomplete. Also do not mark unreachable statutes if
     there is an alarm in the initializers (bottom initial state), as we
     would end up marking the alarm as dead. *)
  Eval_annots.mark_unreachable ();
  (* Try to refine the 'Unknown' statuses that have been emitted during
     this analysis. *)
  Eval_annots.mark_green_and_red ();
  Mem_exec.cleanup_results ();
  (* Remove redundant alarms *)
  if Parameters.RmAssert.get () then Eva_dynamic.Scope.rm_asserts ();
  (* The above functions may have changed the status of alarms. *)
  Summary.FunctionStats.recompute_all ();
  Red_statuses.report ();
  (* Print results *)
  let module Engine = (val engine) in
  Engine.Dom.post_analysis final_state;
  Summary.print ();
  Statistics.export_as_csv ()


(* ----- Analysis status ---------------------------------------------------- *)

type computation_state = Self.computation_state =
  | NotComputed | Computing | Computed | Aborted
let current_computation_state = Self.ComputationState.get
let register_computation_hook ?on f =
  let f' = match on with
    | None -> f
    | Some s -> fun s' -> if s = s' then f s
  in
  Self.ComputationState.add_hook_on_change f'

let is_computed = Self.is_computed
let self = Self.state
let emitter = Eva_utils.emitter

type results = Function_calls.results = Complete | Partial | NoResults
type status = Function_calls.analysis_status =
    Unreachable | SpecUsed | Builtin of string | Analyzed of results
let status kf =
  match Function_calls.analysis_status kf with
  | Analyzed Complete as status ->
    if is_computed () then status else Analyzed Partial
  | status -> status

let use_spec_instead_of_definition =
  Function_calls.use_spec_instead_of_definition ?recursion_depth:None

let save_results kf =
  try Function_calls.save_results (Kernel_function.get_definition kf)
  with Kernel_function.No_Definition -> false

(* ----- Running the analysis ------------------------------------------------ *)

exception Error

let compute_from_entry_point  (type t) (engine: t engine)
    ?(thread=Thread.main) ?cvalue_state ?arguments entry_point =
  let module Engine = (val engine) in
  let lib_entry = Kernel.LibEntry.get () in
  Self.feedback "Analyzing a%scomplete application starting at %a"
    (if lib_entry then "n in" else " ")
    Kernel_function.pretty entry_point;
  match Engine.Initialization.initial_state_with_formals
          ?cvalue_state ?arguments ~lib_entry entry_point with
  | `Bottom ->
    Eval_annots.mark_invalid_initializers ();
    Self.error "Eva not started because globals \
                initialization is not computable.";
    raise Error
  | `Value initial_state ->
    Engine.Compute.compute_main_call ~thread entry_point initial_state

(* Mthread entry point *)

let compute_thread (type t) (engine: t engine) ?cvalue_state thread =
  let Thread.{ entry_point; arguments } = Thread.properties thread in
  let arguments =
    if Thread.is_main thread
    then None (* use generated main arguments *)
    else Some (List.map snd arguments)
  in
  (* In multi thread analyses, Memexec cache must be invalidated *)
  Mem_exec.cleanup_results ();
  compute_from_entry_point engine
    ~thread ?cvalue_state ?arguments entry_point

let thread_analysis engine analysis final_states th =
  if Mt_thread.ThreadState.needs_recomputation ~feedback:true th then begin
    Mt_analysis_fixpoint.pre_thread_analysis analysis th;
    let cvalue_state = th.th_init_state in
    let final_state = compute_thread engine ~cvalue_state th.th_eva_thread in
    (* Store the thread analysis final state. *)
    Thread.Hashtbl.replace final_states th.th_eva_thread final_state;
    (* We save all our results *)
    Mt_analysis_fixpoint.post_thread_analysis analysis;
  end;
  th.th_to_recompute <- Mt_thread.SetRecomputeReason.empty

(* Auxiliary function iterating the analysis until the fixpoint is reached *)
let mthread_fixpoint engine analysis =
  (* Store thread analyse final result of each thread in a Hashtbl. For now,
     only the result of the main thread is used. *)
  let final_states = Thread.Hashtbl.create 1 in

  (* We analyse the main thread *)
  Mt_self.feedback "*** Computing value analysis for main thread";
  let final_state = compute_thread engine Thread.main in
  Thread.Hashtbl.replace final_states Thread.main final_state;
  Mt_self.feedback "*** First value analysis for main thread done." ;
  Mt_analysis_fixpoint.post_thread_analysis analysis;

  (* We perform the analysis iterations *)
  Mt_self.feedback "******* Starting to iterate";
  let limit = Mt_options.StopAfter.get () in
  analysis.iteration <- 0;
  while
    analysis.iteration < limit &&
    Mt_thread.needs_recomputation analysis
  do
    analysis.iteration <- analysis.iteration + 1;
    Mt_self.feedback "***** Iteration %d" analysis.iteration;
    Mt_thread.iter_threads analysis
      (thread_analysis engine analysis final_states);
    Mt_self.feedback "***** Threads computed for iteration %d."
      analysis.iteration;
    Mt_analysis_fixpoint.post_iteration analysis
  done;

  (* Return the main thread final state. *)
  Thread.Hashtbl.find final_states Thread.main

(* Perform an entire mthread execution on the current project *)
let compute_from ?cvalue_state ?arguments entry_point =
  (* Setup signals *)
  let restore_signals = Signal.setup () in
  Fun.protect ~finally:restore_signals @@ fun () ->
  (* Mthread pre-analysis: returns an analysis state if requested. *)
  let mt_analysis = Mt_main.pre_analysis () in
  (* Prepare the analysis and build the engine. *)
  let module Engine = (val pre_analysis ()) in
  try
    Self.ComputationState.set Computing;
    (* Run the analysis. *)
    let final_state =
      match mt_analysis with
      | Some analysis -> mthread_fixpoint (module Engine) analysis
      | None ->
        compute_from_entry_point (module Engine)
          ?cvalue_state ?arguments entry_point
    in
    Self.(ComputationState.set Computed);
    post_analysis (module Engine) final_state;
    Option.iter Mt_main.post_analysis mt_analysis
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    Self.(ComputationState.set Aborted);
    match exn with
    | Error | Self.Abort -> () (* do not re-raise  *)
    | exn -> Printexc.raise_with_backtrace exn backtrace

let compute () =
  (* Nothing to recompute when Eva has already been computed. This boolean
      is automatically cleared when an option of Eva changes, because they
      are registered as dependencies on [Self.state] in {!Parameters}.*)
  if not (is_computed ()) then
    let cvalue_state = Eva_results.get_initial_state ()
    and arguments = Eva_results.get_main_args ()
    and entry_point = fst @@ Globals.entry_point () in
    compute_from ?cvalue_state ?arguments entry_point

let compute =
  let name = "Eva.Analysis.compute" in
  fst (State_builder.apply_once name [ Self.state ] compute)

let main () = if Parameters.Eva.get () then compute ()
let () = Boot.Main.extend main

let abort () =
  Signal.abort ()
