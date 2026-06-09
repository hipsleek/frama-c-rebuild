(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Mt_types
open Mt_shared_vars_types
open Mt_cfg_types


(* -------------------------------------------------------------------------- *)
(* --- Thread reasons to recompute                                        --- *)
(* -------------------------------------------------------------------------- *)

type recompute_reason =
  | FirstIteration
  | NewMsgReceived
  | PotentialSharedVarsChanged
  | SharedVarsValuesChanged
  | InitialArgsChanged
  | InitialEnvChanged
  | InterferencesChanged
[@@deriving ord]

module RecomputeReason = struct
  type t = recompute_reason
  [@@deriving ord]

  let to_string = function
    | FirstIteration -> "first iteration"
    | NewMsgReceived -> "new message received"
    | SharedVarsValuesChanged -> "shared vars values changed"
    | PotentialSharedVarsChanged -> "potential shared vars changed"
    | InitialArgsChanged -> "thread initial arguments changed"
    | InitialEnvChanged -> "thread initial memory state changed"
    | InterferencesChanged -> "interferences changed"

  let pretty fmt r =
    Format.pp_print_text fmt (to_string r)
end

module SetRecomputeReason = struct
  include Set.Make (RecomputeReason)
  let pretty fmt set = pretty_text RecomputeReason.pretty fmt set
end


(* -------------------------------------------------------------------------- *)
(* --- Threads                                                            --- *)
(* -------------------------------------------------------------------------- *)

type priority = PDefault | PUnknown | PPriority of int

module Priority = Datatype.Make_with_collections(struct
    type t = priority
    let name = "Mt_thread.priority"
    let reprs = [PPriority 0; PDefault; PUnknown]

    include Datatype.Undefined
    let compare: t -> t -> int = Extlib.compare_basic
    let equal = Datatype.from_compare
    let hash = Hashtbl.hash
  end)

type thread = Thread.t

type thread_state = {
  th_eva_thread : Thread.t;
  th_parent : thread_state option;
  th_fun : kernel_function;
  th_stack : Callstack.t;
  mutable th_init_state : Cvalue.Model.t;
  mutable th_params : Cvalue.V.t list;
  mutable th_amap : Trace.t;
  mutable th_to_recompute: SetRecomputeReason.t;
  mutable th_read_written: AccessesByZone.map;
  mutable th_cfg: CfgNode.t;
  mutable th_read_written_cfg: AccessesByZoneNode.map;
  mutable th_values_written: Mt_memory.Types.state;
  mutable th_priority: priority;
}

module ThreadState = struct
  type t = thread_state

  let label th = Thread.label th.th_eva_thread
  let is_main th = Thread.is_main th.th_eva_thread
  let pretty fmt th = Thread.pretty fmt th.th_eva_thread
  let equal th1 th2 = Thread.equal th1.th_eva_thread th2.th_eva_thread

  let pretty_detailed fmt th =
    let pp_parent fmt = function
      | None -> ()
      | Some p ->
        Format.fprintf fmt ",@ parent %a,@ args %a"
          pretty p
          (Pretty_utils.pp_list ~sep:",@ " Cvalue.V.pretty) th.th_params
    in
    Format.fprintf fmt "%a,@ fun %s%a"
      pretty th
      (Kernel_function.get_name th.th_fun)
      pp_parent th.th_parent

  let one_creates_other th1 th2 =
    let creates thp ths =
      let rec in_parents ths' = match ths'.th_parent with
        | None -> `Unrelated
        | Some th when equal thp th -> `Creates (thp, ths)
        | Some th -> in_parents th
      in
      in_parents ths
    in
    match creates th1 th2 with
    | `Unrelated -> creates th2 th1
    | _ as r -> r

  let recompute_because th r =
    if not (SetRecomputeReason.equal th.th_to_recompute
              (SetRecomputeReason.singleton FirstIteration))
    (* Can happen if the control-flow leading to the thread creation
       is split by the value analysis *)
    then
      th.th_to_recompute <- SetRecomputeReason.add r th.th_to_recompute

  type recompute_status = NoNeed | NotStarted | Recompute

  let get_recompute_status th =
    if SetRecomputeReason.is_empty th.th_to_recompute then NoNeed
    else if not (Cvalue.Model.is_reachable th.th_init_state) then NotStarted
    else Recompute

  let recompute_feedback = function
    | NoNeed -> Mt_self.debug "No need to recompute thread %a"
    | NotStarted ->
      Mt_self.feedback "*** Thread %a has been created but not started. Skipping."
    | Recompute -> fun _ _ -> ()

  let needs_recomputation ?(feedback=false) th =
    let status = get_recompute_status th in
    if feedback then recompute_feedback status pretty th;
    status = Recompute
end



(* -------------------------------------------------------------------------- *)
(* --- Thread analysis                                                    --- *)
(* -------------------------------------------------------------------------- *)


type threads_table = thread_state Thread.Hashtbl.t

type analysis_state = {
  all_threads : threads_table (* List of all threads. Is kept (and can thus
                                 increase) from one iteration to the next *);

  mutable all_mutexes: Mutex.Set.t; (** Set of all mutexes of the analysis *)

  mutable all_queues: Mqueue.Set.t; (** Set of all queues of the analysis *)

  mutable iteration: int (* Current iteration of the analysis *);

  mutable main_thread: thread_state (* Starting thread *);

  mutable curr_thread: thread_state (* Thread currently running. *);

  mutable curr_events_stack: Trace.t list (* Mthread events that have been
                                             found during the current analysis of the current thread. The list
                                             has the same height as [curr_stack]. The top of the list is the trace
                                             containing the events for the function being analyzed by Value, and
                                             so on until the top of the list. When the list is popped, the events
                                             of the callee are merged inside the trace of the caller. *);

  mutable memexec_cache: Trace.t Datatype.Int.Hashtbl.t
(* Cache for the results obtained during the analysis of the current
   thread *);

  mutable curr_stack: Callstack.t
(* stack of a multithread event. Asynchronously set by a callback and used
   by another, because of a slightly too restricted signature in the
   value analysis. *);

  mutable concurrent_accesses: Memory_zone.t
(* Shared variables that have been detected in the analysis so far
   in a global manner *);

  mutable precise_concurrent_accesses: Memory_zone.t
(* Shared variables that have been detected in the analysis so far,
   through the various cfgs. Subset of the previous field *);

  mutable concurrent_accesses_by_nodes:
    (Memory_zone.t * SetNodeIdAccess.t) list
(* List of concurrent accesses that have been found. Used to
   compute the field [precise_concurrent_accesses] *);
}

let is_thread_name_enabled name =
  not (Mt_options.SkipThreads.mem name)
  && Mt_options.OnlyThreads.(is_empty () || mem name)

let is_thread_enabled th =
  Thread.is_main th.th_eva_thread
  || is_thread_name_enabled (ThreadState.label th)

(* Iterators on threads. We presave the current list of threads so that
   the iterators do not accidentally capture new added threads. (This is not
   important for correctness, but is slightly cleaner.). Threads are sorted,
   again for cleanliness reasons. *)
let threads analysis =
  (* the main thread always has the least id and will always be in front of the
     list *)
  Thread.Hashtbl.fold_sorted (fun _ th l -> th :: l) analysis.all_threads []
  |> List.filter is_thread_enabled
  |> List.rev

let thread_state analysis th =
  try Thread.Hashtbl.find analysis.all_threads th
  with Not_found -> Mt_self.fatal "Unknown thread %a" Thread.pretty th

let fold_threads analysis v f =
  List.fold_left (fun acc th -> f th acc) v (threads analysis)
let iter_threads analysis f =
  List.iter (fun th -> f th) (threads analysis)


let calling_ki analysis = Callstack.top_callsite analysis.curr_stack
let current_fun analysis = Callstack.top_kf analysis.curr_stack

let curr_events analysis =
  match analysis.curr_events_stack with
  | [] -> Mt_self.fatal "Invalid analysis stack"
  | h :: _ -> h

let on_current_trace analysis f =
  match analysis.curr_events_stack with
  | [] -> Mt_self.fatal "Invalid analysis stack"
  | h :: q ->
    analysis.curr_events_stack <- f h q :: q

(* Store a mthread event into the state of our analysis. *)
let register_event analysis ?(top=Callstack.top_call analysis.curr_stack) evt =
  on_current_trace analysis
    (fun cur _ -> Trace.add_event cur top evt)
;;

let register_multiple_events analysis evts =
  on_current_trace analysis
    (fun cur _ -> Trace.union evts cur)
;;

(* Store the memory state for the function which we finished analyzing *)
let register_memory_states analysis ~before ~after =
  Mt_self.debug ~level:2 "Recording states for %a"
    Kernel_function.pretty (current_fun analysis);
  on_current_trace analysis (fun cur _ ->  Trace.add_states cur ~before ~after);
;;

let push_function_call analysis =
  analysis.curr_events_stack <- Trace.empty :: analysis.curr_events_stack

let pop_function_call analysis =
  let top = Callstack.top_call analysis.curr_stack in
  match analysis.curr_stack.stack with
  | [] ->
    assert (List.length analysis.curr_events_stack = 1);
    on_current_trace analysis (fun cur _ -> Trace.add_prefix top cur);
  | _ :: _ ->
    match analysis.curr_events_stack with
    | [] | [_] -> Mt_self.fatal "Invalid analysis stack when popping calling"
    | trace_callee :: trace_caller :: q ->
      let trace_callee' = Trace.add_prefix top trace_callee in
      let new_trace = Trace.union trace_caller trace_callee' in
      analysis.curr_events_stack <- new_trace :: q


module OrderedThreads = struct

  let family_tree analysis =
    let th_tbl = analysis.all_threads in
    (* The inheritance table has at most as many entries as the general
       thread table *)
    let tree = Thread.Hashtbl.(create (length th_tbl)) in
    Thread.Hashtbl.iter_sorted
      (fun th state ->
         match state.th_parent with
         | None -> () (* This is the main thread *)
         | Some parent ->
           let children =
             try Thread.Hashtbl.find tree parent.th_eva_thread
             with Not_found -> []
           in
           Thread.Hashtbl.replace tree parent.th_eva_thread (th :: children)
      ) th_tbl;
    tree
  ;;

  let creation_map analysis =
    let tree = family_tree analysis in
    (* Not really optimized, but we don't really care here. Mostly,
       threads are created by one single thread, the main one *)
    let rec all_children acc th =
      let immediate_children = try Thread.Hashtbl.find tree th with Not_found -> []
      and do_child acc th' =
        let acc' = Thread.Set.add th' acc in
        all_children acc' th'
      in
      List.fold_left do_child acc immediate_children
    in
    fold_threads analysis Thread.Map.empty
      (fun th map ->
         let children = all_children Thread.Set.empty th.th_eva_thread in
         Thread.Map.add th.th_eva_thread children map
      )

  (* Iter a function f over program threads following the an order compatible
     with the partial order induced by thread creation *)
  let ordered_iter analysis =
    let tree = family_tree analysis in
    fun f initial ->
      let rec do_thread value th =
        let v = f th value in
        try
          let children = Thread.Hashtbl.find tree th in
          List.iter (do_thread v) children;
        with Not_found -> ()

      in
      do_thread initial Thread.main
  ;;

  let ordered_fold f acc analysis =
    let tree = family_tree analysis in
    let rec do_thread_id_list acc thlist =
      match thlist with
      | [] -> acc
      | _ :: _ ->
        let new_acc, next_level =
          List.fold_left
            (fun (glob_acc, next_acc) th ->
               let children =
                 try Thread.Hashtbl.find tree th
                 with Not_found -> [] in
               (f glob_acc th, children @ next_acc)
            ) (acc, []) thlist in
        do_thread_id_list new_acc next_level
    in do_thread_id_list acc [Thread.main]
  ;;
end

let pretty_recompute_reasons fmt analysis =
  let pretty_thread_reasons fmt th =
    if not (SetRecomputeReason.is_empty th.th_to_recompute) then
      Format.fprintf fmt "@[<hov 2>Thread %a:@ %a@]@ "
        ThreadState.pretty_detailed th
        SetRecomputeReason.pretty th.th_to_recompute
  in
  Format.fprintf fmt "@[<v>Remaining to do:@ %t@]"
    (fun fmt -> iter_threads analysis (pretty_thread_reasons fmt))

let needs_recomputation analysis =
  threads analysis
  |> List.exists ThreadState.needs_recomputation
