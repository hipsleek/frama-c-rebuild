(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast
open Mt_cil
open Mt_memory.Types
open Mt_types
open Mt_shared_vars_types
open Mt_thread

let wrap_res res = Some (Mt_memory.int_to_value res)
let no_res = (None : value option)

type hook_sig = (exp * value) list ->  state * value option

let current_position analysis =
  match Callstack.top_callsite analysis.curr_stack with
  | Kglobal -> assert false (* The current stack must contain the call to the builting creating the thread *)
  | Kstmt stmt ->
    stmt, Option.get (Callstack.pop analysis.curr_stack)

(* -------------------------------------------------------------------------- *)
(* --- Specialized logging functions                                      --- *)
(* -------------------------------------------------------------------------- *)


(* Returns [source] and [append] arguments for log functions used in hooks,
   according to the the current stack. As builtins are called inside stubbed
   function for pthreads library, we use the position of the penultimate call
   site, which should correspond to the call to the pthreads function. *)
let log_arg analysis =
  let stack = analysis.curr_stack in
  let stack = Option.value (Callstack.pop stack) ~default:stack in
  let source = kinstr_to_source (Callstack.top_callsite stack) in
  let append fmt =
    if Mt_options.PrintCallstacks.get () || Mt_self.Debug.get () > 1
    then Format.fprintf fmt "@.%a" Callstack.pretty stack
  in
  source, append

let result analysis =
  let source, append = log_arg analysis in
  Mt_self.result ~once:true ?source ~append

let warning analysis =
  let source, append = log_arg analysis in
  Mt_self.warning ~once:true ?source ~append

let error analysis =
  let source, append = log_arg analysis in
  Mt_self.error ?source ~append

exception Hook_failure of int
let default_err_code = -255
let hook_fail ?(code=default_err_code) () =
  raise (Hook_failure code)

(* Auxiliary function that aborts a hook when a conversion of something
   into a proper value fails *)
let catch_conversion analysis ~prefix v msg =
  match v with
  | Ok v -> v
  | Error error ->
    warning analysis "@[%s: %s. %s. Ignoring.@]" prefix msg error;
    hook_fail ()

(* -------------------------------------------------------------------------- *)
(* --- Specialization of id function                                          *)
(* -------------------------------------------------------------------------- *)

let find_id kind find id =
  match find id with
  | Some elt -> Ok elt
  | None ->
    let error =
      Format.sprintf
        "Id %d for %s does not exists (incrementation inside program?)."
        id kind
    in
    Error error

let find_thread = find_id "thread" Thread.find
let find_mutex = find_id "mutex" Mutex.find
let find_queue = find_id "queue" Mqueue.find

(* -------------------------------------------------------------------------- *)
(* --- Constants written in memory to store states                        --- *)
(* -------------------------------------------------------------------------- *)

(* Auxiliary function which converts a cvalue to a singleton integer from the
   list of expected values [possible_values], or return None otherwise. *)
let project_singleton_int cvalue possible_values =
  let find i =
    List.find_opt (fun (x, _) -> Datatype.Int.equal x i) possible_values
  in
  try
    Cvalue.V.project_ival cvalue
    |> Ival.project_int
    |> Z.to_int
    |> find
  with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int | Z.Overflow ->
    None

(* Information about an operation on a thread, mutex or message queue. *)
type operation = {
  name: string; (* Name of the operation. *)
  before: int list; (* List of possible legal values before the operation. *)
  after: int; (* Value after the operation. *)
}

(* Checks that [value] is a singleton integer as expected before [operation],
   or emit a warning by calling [warn]. [possible_values] is the list of
   possible values for [value] associated with a precise message. If [value]
   is not a singleton integer from [possible_values], an imprecise warning
   is emitted. *)
let check_value warn possible_values operation value =
  match project_singleton_int value possible_values with
  | Some (i, _msg) when List.mem i operation.before -> ()
  | Some (_i, msg) -> warn msg
  | None ->
    let reason =
      Format.asprintf
        "unable to check its precise status (internal value %a should be %a)"
        Cvalue.V.pretty value
        (Pretty_utils.pp_list ~sep:" or " Datatype.Int.pretty) operation.before
    in
    warn reason

(* Operations on threads. *)
module ThreadOp = struct
  let not_created = 0
  let started = 1
  let suspended = 2
  let cancelled = 3

  let possible_values =
    [ not_created, "it might not be created yet" ;
      started, "it might have already been started by the current thread";
      suspended, "it might have already been suspended by the current thread";
      cancelled, "it might have been cancelled by the current thread"; ]

  let start =
    { name = "start"; before = [suspended]; after = started; }
  let suspend =
    { name = "suspend"; before = [started]; after = suspended; }
  let cancel =
    { name = "cancel"; before = [started; suspended]; after = cancelled; }

  let check_and_write analysis state thread operation =
    let id = Mt_ids.of_thread thread in
    let value = Mt_ids.read_id_state state id in
    let warn failure_reason =
      warning analysis "Trying to %s thread %a, but %s."
        operation.name Thread.pretty thread failure_reason
    in
    check_value warn possible_values operation value;
    Mt_ids.write_id_state state id operation.after
end

(* Operations on mutexes. *)
module MutexOp = struct
  let not_init = 0
  let unlocked = 1
  let locked = 2

  let possible_values =
    [ not_init, "it might not be initialized yet";
      unlocked, "it might already be unlocked (and initialized)";
      locked, "it might already be locked (and initialized)"; ]

  let initialize = { name = "initialize"; before = [not_init]; after = unlocked; }
  let lock = { name = "lock"; before = [unlocked]; after = locked; }
  let unlock = { name = "unlock"; before = [locked]; after = unlocked; }

  let check_and_write analysis state mutex operation =
    let id = Mt_ids.of_mutex mutex in
    let value = Mt_ids.read_id_state state id in
    let warn failure_reason =
      warning analysis "Trying to %s mutex %a, but %s."
        operation.name Mutex.pretty mutex failure_reason
    in
    check_value warn possible_values operation value;
    Mt_ids.write_id_state state id operation.after
end

(* Operations on message queues. *)
module QueueOp = struct
  let not_init = 0
  let initialized = 1

  let possible_values =
    [ not_init, "it might not be initialized yet";
      initialized, "it might be already initialized"; ]

  let initialize =
    { name = "initialize"; before = [not_init]; after = initialized; }

  let use name = { name; before = [initialized]; after = initialized; }
  let send = use "send message to"
  let receive = use "receive message from"

  let check_and_write analysis state queue operation =
    let id = Mt_ids.of_queue queue in
    let value = Mt_ids.read_id_state state id in
    let warn failure_reason =
      warning analysis "Trying to %s message queue %a, but %s."
        operation.name Mqueue.pretty queue failure_reason
    in
    check_value warn possible_values operation value;
    Mt_ids.write_id_state state id operation.after
end

(** When a thread is created, it must not inherit from its creator the status
    of mutexes. This function sets all mutexes passed as argument to 1
    (unlocked). *)
let reset_mutexes mutexes state =
  Mutex.Set.fold
    (fun mutex state -> Mt_ids.replace_id_value state (Mt_ids.of_mutex mutex) ~before:2 ~after:1)
    mutexes state


(* -------------------------------------------------------------------------- *)
(* --- External values for shared zones                                   --- *)
(* -------------------------------------------------------------------------- *)

(* XXX: we should sync values only for the threads that may be alive at this
   point *)
let sync_values analysis state =
  let join ~written ~state =
    Cvalue.Model.fold
      (fun b offsm state ->
         let offsm' = Cvalue.Model.find_base_or_default b state in
         match offsm' with
         | `Top -> Mt_self.fatal "Top state"
         | `Bottom -> state
         | `Value offsm' ->
           let offsm'' = Cvalue.V_Offsetmap.join offsm offsm' in
           Cvalue.Model.add_base b offsm'' state)
      written state
  in
  let v = Mt_lib.var_thread_created () in
  let value = Results.(in_cvalue_state state |> eval_var v |> as_cvalue) in
  if Cvalue.V.is_zero value then
    state (* As no thread is running, just skip the synchronization. *)
  else
    fold_threads analysis state
      (fun th state ->
         match th.th_values_written with
         | Cvalue.Model.Bottom -> state
         | Cvalue.Model.Top -> Cvalue.Model.top
         | Cvalue.Model.Map written ->
           if not (ThreadState.equal analysis.curr_thread th) then
             join ~written ~state
           else state
      )


let hook_sync analysis state : hook_sig = function _ ->
  sync_values analysis state, no_res

(* -------------------------------------------------------------------------- *)
(* --- Creation of a thread                                               --- *)
(* -------------------------------------------------------------------------- *)

let basic_thread eva_thread stack func state params parent = {
  th_eva_thread = eva_thread;
  th_stack = stack;
  th_init_state = state;
  th_fun = func;
  th_params = params;
  th_parent = parent;
  th_to_recompute = SetRecomputeReason.empty;
  th_read_written = AccessesByZone.empty_map;
  th_amap = Trace.empty;
  th_cfg = Mt_cfg_types.CfgNode.dead;
  th_read_written_cfg = Mt_cfg_types.AccessesByZoneNode.empty_map;
  th_values_written = Cvalue.Model.empty_map;
  th_priority= PDefault;
}

let spawn_thread analysis eva_thread stack func state params parent =
  try
    let th' = Thread.Hashtbl.find analysis.all_threads eva_thread in

    if Option.equal ThreadState.equal parent th'.th_parent = false
    then (
      let pp_parent = Pretty_utils.pp_opt ~none:"<none>" ThreadState.pretty in
      error analysis "Thread '%a' is launched by two different \
                      threads (%a and %a). Ignoring"
        Thread.pretty eva_thread
        pp_parent parent
        pp_parent th'.th_parent;
      hook_fail ())

    else if Callstack.equal stack th'.th_stack = false then (
      error analysis
        "Thread '%a' is launched in two different contexts:@.\
         Context 1:@.@[<hov 2>  %a@]@.Context 2:@.@[<hov 2>  %a@]@.Ignoring"
        Thread.pretty eva_thread
        Callstack.pretty stack
        Callstack.pretty th'.th_stack;
      hook_fail ())

    else if Kernel_function.get_id func <> Kernel_function.get_id th'.th_fun
    then (
      error analysis
        "Thread '%a' can be two different functions \
         (%s and %s). Imprecise pointer? Ignoring."
        Thread.pretty eva_thread
        (Kernel_function.get_name func)
        (Kernel_function.get_name th'.th_fun);
      hook_fail ())

    else (
      (* Fields that are being joined *)
      let init_state', ris = Mt_memory.join_state th'.th_init_state state
      and args, ra = Mt_memory.join_params th'.th_params params
      in
      th'.th_init_state <- init_state';
      th'.th_params <- args;
      if ris then ThreadState.recompute_because th' InitialEnvChanged;
      if ra  then ThreadState.recompute_because th' InitialArgsChanged;
      let text =
        if ris || ra then "New context for" else "Thread" in
      result analysis "@[<hov 2>%s@ %a@]" text ThreadState.pretty_detailed th';
      th'
    )
  with Not_found ->
    let th = basic_thread eva_thread stack func state params parent in
    th.th_to_recompute <- SetRecomputeReason.singleton FirstIteration;
    Thread.Hashtbl.add analysis.all_threads eva_thread th;
    result analysis "@[<hov>New thread: %a@]" ThreadState.pretty_detailed th;
    th

let check_thread_analysis thread kf =
  match Function_calls.analysis_target kf Kglobal with
  | `Body _ -> ()
  | `Builtin _ | `Spec _ ->
    Mt_self.not_yet_implemented
      "Using an ACSL specification or a builtin to interpret entry point %a \
       of thread %a is not supported."
      Kernel_function.pretty kf Thread.pretty thread

let standalone_thread th kf initial_state =
  check_thread_analysis th kf;
  let formals = Kernel_function.get_formals kf in
  let eval_arg vi =
    Results.(in_cvalue_state initial_state |> eval_var vi |> as_cvalue)
  in
  let args = List.map eval_arg formals in
  let stack = Callstack.init ~thread:(Thread.id th) ~entry_point:kf in
  basic_thread th stack kf initial_state args None

let main_thread k_main initial_state =
  standalone_thread Thread.main k_main initial_state

let interrupt_thread kf initial_state =
  let th = Thread.interrupt_handler kf in
  standalone_thread th kf initial_state

(** Set the global variable that indicates that at least one thread is running
    to one *)
let thread_is_running state =
  let p_thread_running = Mt_lib.var_thread_created (), 0 in
  Mt_memory.write_int_pointer p_thread_running 1 state


(** Hook registered in the value analysis for the creation of thread *)
let hook_thread_creation analysis state : hook_sig = function
  | (_, name) :: (_, f) :: params ->
    let conv = catch_conversion analysis ~prefix:"During thread creation" in
    (* We clean the state that will be used by the created thread *)
    let kf = conv (Mt_memory.extract_fun f) "invalid thread function" in
    let formals = Kernel_function.get_formals kf in
    let rec trunc_params = function
      | [], [] -> []
      | _formal :: qf, param :: qp -> param :: trunc_params (qf, qp)
      | [], (_ :: _ as params) ->
        if Mt_options.ModerateWarnings.get () then
          warning analysis
            "During thread creation, mismatch between function \
             '%s' signature and actual arguments. Ignoring last \
             %d argument(s) and continuing."
            (Kernel_function.get_name kf) (List.length params);
        []
      | _ :: _, [] ->
        error analysis
          "When creating thread from function %s: too few \
           arguments, %d expected but %d given. Ignoring.]"
          (Kernel_function.get_name kf)
          (List.length formals) (List.length params);
        hook_fail ()
    in
    let params = List.map snd (trunc_params (formals, params)) in
    let eva_thread =
      let name = Concurrency.Name.of_cvalue name in
      let pos = current_position analysis in
      Thread.spawn pos name kf params
    in
    ignore (spawn_thread analysis eva_thread analysis.curr_stack kf
              Cvalue.Model.bottom params (Some analysis.curr_thread));
    register_event analysis (CreateThread eva_thread);
    (* Thread is started as suspended *)
    Mt_ids.write_id_state state (Mt_ids.of_thread eva_thread) 2,
    wrap_res (Thread.id eva_thread)

  | _ -> Mt_self.fatal "Incorrect mthread binding for thread creation"
(* By typing, Frama_C_thread_create must receive at least those
   arguments *)


let update_initial_state analysis th state =
  (* From now on, at least two threads are running *)
  let state = thread_is_running state in
  (* Remove references local to the parent thread *)
  let state_started = Mt_memory.clear_non_globals state in
  (* Mutexes should be unlocked in the new threads *)
  let state_started = reset_mutexes analysis.all_mutexes state_started in
  let th =  Thread.Hashtbl.find analysis.all_threads th in
  let initial, changed = Mt_memory.join_state th.th_init_state state_started in
  if changed then (
    ThreadState.recompute_because th Mt_thread.InitialEnvChanged;
    if Cvalue.Model.is_reachable th.th_init_state then
      result analysis "@[<hov 2>New context for@ %a@]"
        ThreadState.pretty_detailed th;
  );
  th.th_init_state <- initial;
  (* Update the state of the creator too: more than one thread is running,
     and the values written by the thread just created become visible. *)
  sync_values analysis state

let hook_thread_start_suspend operation aux_state evt analysis state : hook_sig = function
  | [_, offset]  ->
    let prefix = "During thread " ^ operation.name in
    let conv v = catch_conversion analysis ~prefix v in
    let offset = conv (Mt_memory.extract_int offset) "invalid thread id" in
    if offset <> 0 then
      let th = conv (find_thread offset) "unknown thread" in
      let state = ThreadOp.check_and_write analysis state th operation in
      let evt = evt th in
      result analysis "@[%a@]" Event.pretty evt;
      register_event analysis evt;
      let state = aux_state analysis th (state:state) in
      state, wrap_res 0
    else (
      warning analysis "Trying to %s unknown thread. Ignoring." operation.name;
      hook_fail ~code:(-1) ())

  | _ -> Mt_self.fatal "Incorrect mthread binding for thread %s" operation.name

(** Hook registered in the value analysis when a thread is started *)
let hook_thread_start =
  hook_thread_start_suspend ThreadOp.start
    update_initial_state (fun i -> StartThread i)

let hook_thread_suspend =
  hook_thread_start_suspend ThreadOp.suspend
    (fun _ _ s -> s) (fun i -> SuspendThread i)



let hook_thread_cancellation analysis state : hook_sig = function
  | [_, offset]  ->
    let prefix = "During thread cancellation" in
    let conv v = catch_conversion analysis ~prefix v in
    let offset = conv (Mt_memory.extract_int offset) "invalid thread id" in
    if offset <> 0 then
      let th = conv (find_thread offset) "unknown thread" in
      register_event analysis (CancelThread th);
      let state = ThreadOp.check_and_write analysis state th ThreadOp.cancel in
      state, wrap_res 0
    else (
      warning analysis "Trying to cancel unknown thread. Ignoring.";
      hook_fail ~code:(-1) ())

  | _ -> Mt_self.fatal "Incorrect mthread binding for thread cancellation \
                        (only the thread id is expected)"

let hook_thread_exit analysis (_state: state) : hook_sig = function
  | [_, v]  ->
    if ThreadState.is_main analysis.curr_thread then (
      error analysis
        "Call to thread exit primitive inside main thread. Ignoring";
      hook_fail ())
    else (
      register_event analysis (ThreadExit v);
      result analysis "Thread exiting with value %a" Cvalue.V.pretty v;
      Cvalue.Model.bottom, no_res)

  | _ -> Mt_self.fatal "Incorrect mthread binding for thread exit \
                        (only the return value is expected)"

let hook_thread_id analysis state : hook_sig = fun _ ->
  state, wrap_res (Thread.id analysis.curr_thread.th_eva_thread)


let hook_thread_priority analysis state : hook_sig = function
  |[ _, p] ->
    begin
      let p = catch_conversion analysis
          ~prefix:"During thread priority definition"
          (Mt_memory.extract_int p)
          "invalid thread id"
      in
      begin
        match analysis.curr_thread.th_priority with
        | PPriority p' ->
          if p <> p' then begin
            warning analysis
              "Conflicting priorities (previous: %d, new %d) for thread '%a'."
              p
              p'
              ThreadState.pretty analysis.curr_thread;
            (* TODO: add an event + add a recompute reason *)
            analysis.curr_thread.th_priority <- PUnknown;
          end
        | PUnknown -> ()
        | PDefault ->
          result analysis "Setting priority to %d" p;
          analysis.curr_thread.th_priority <- PPriority p;
      end;
      state, wrap_res 0
    end
  | _ -> Mt_self.fatal "Incorrect mthread binding for thread priority \
                        (only a non negative integer is expected)"

(* -------------------------------------------------------------------------- *)
(** --- Hook registered in the value analysis related to messages         --- *)
(* -------------------------------------------------------------------------- *)

let hook_queue_init analysis state : hook_sig = function
  | [_, name; _, size] ->
    let prefix = "During queue initialization" in
    let conv v = catch_conversion analysis ~prefix v in
    let pos = current_position analysis
    and name = Concurrency.Name.of_cvalue name
    and size = conv (Mt_memory.extract_int size) "invalid size" in
    let q = Mqueue.create pos name in
    analysis.all_queues <- Mqueue.Set.add q analysis.all_queues;
    let state = QueueOp.check_and_write analysis state q QueueOp.initialize in
    let size = if size < 0 then None else Some size in
    register_event analysis (CreateQueue (q, size));
    state, wrap_res (Mqueue.id q)

  | _ -> Mt_self.fatal "Incorrect mthread binding for queue creation"

let hook_send_msg analysis state : hook_sig = function
  | [(_, offset); (_exp_content, content); (_exp_size, size)] ->
    let conv v = catch_conversion analysis ~prefix:"During message sending" v in
    let offset = conv (Mt_memory.extract_int offset) "invalid queue id" in
    if offset <> 0 then
      let sbytes = conv (Mt_memory.extract_int size) "invalid message size" in
      if sbytes <= 0 then
        conv (Error (string_of_int sbytes)) "invalid message length";
      let q = conv (find_queue offset) "unknown queue" in
      let content = Mt_memory.read_slice ~p:content ~sbytes state in
      let state = QueueOp.check_and_write analysis state q QueueOp.send in
      let action = SendMsg (q, (content, sbytes)) in
      result analysis "@[%a@]" Event.pretty action;
      register_event analysis action;
      state, wrap_res 0
    else (
      warning analysis
        "Trying to send message on uninitialized queue. Ignoring.";
      state, wrap_res (-1))

  | _ -> Mt_self.fatal "Incorrect mthread binding for message sending"


let find_msg_content analysis q =
  let extract_action th acc = function
    | SendMsg (q', (v, size)) ->
      if Mqueue.equal q q' then (th, v, size) :: acc else acc
    | _ -> acc
  in
  fold_threads analysis []
    (fun { th_eva_thread = th; th_amap = m } ->
       Trace.fold' m (fun a r -> extract_action th r a))

let hook_receive_msg analysis state : hook_sig = function
  | [(_,offset); (_e_size, size); (e_loc, loc)] ->
    let prefix = "During message reception" in
    let conv v = catch_conversion analysis ~prefix v in
    let offset = conv (Mt_memory.extract_int offset) "invalid queue id" in
    if offset <> 0 then
      let smax = conv (Mt_memory.extract_int size) "invalid size"
      and p = conv (Mt_memory.extract_pointer loc) "invalid destination buffer"
      and q = conv (find_queue offset) "unknown queue" in
      let state = QueueOp.check_and_write analysis state q QueueOp.receive in
      let action = ReceiveMsg (q, p, smax) in
      register_event analysis action;
      let contents = find_msg_content analysis q in
      let state, res, pp =
        if contents <> [] then
          let length, kept_mess, _, state =
            List.fold_left
              (fun (length, kept_mess, exact, state) (_, slice, smess as mess) ->
                 let sbytes = min smess smax in
                 let state' =
                   Mt_memory.write_slice ~p ~sbytes ~slice ~exact state
                 in
                 if Cvalue.Model.is_reachable state' then
                   let sbytes_val =
                     Cvalue.V.inject_ival (Ival.of_int sbytes) in
                   let length' = Cvalue.V.join sbytes_val length in
                   length', mess :: kept_mess, false, state'
                 else (
                   warning analysis
                     "Found message of length %d, which is too long \
                      for buffer '%a'. Execution will continue without \
                      those messages.(Ignore \"This path is assumed to \
                      be dead message if any\".)"
                     smess pp_exp e_loc;
                   length, kept_mess, exact, state)
              )
              (Cvalue.V.bottom, [], true, state) contents
          in
          match kept_mess with
          | [] ->
            Cvalue.Model.bottom,
            no_res,
            (fun fmt -> Format.fprintf fmt "No valid value to receive.")
          | _ :: _ ->
            let pp fmt =
              Format.fprintf fmt "Possible %s values:@.%a"
                (if List.length kept_mess = List.length contents
                 then "" else "valid ")
                (Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@,"
                   (fun fmt (th, v, _) ->
                      Format.fprintf fmt "@[From thread %a:@ %a@]"
                        Thread.pretty th
                        Mt_memory.pretty_slice v
                   )) kept_mess
            in
            state, Some length, pp
        else
          Cvalue.Model.bottom,
          no_res,
          (fun fmt -> Format.fprintf fmt "No value to receive (yet?).")
      in
      result analysis "@[<hov>%a@ %t@]" Event.pretty action pp;
      state, res
    else (
      warning analysis
        "Trying to receive value on non-initialized queue. Ignoring.";
      state, wrap_res (-2))

  | _ -> Mt_self.fatal "Incorrect mthread binding for message reception"


(* Auxiliary functions for the functions that act on mutexes (currently
   lock and release). [check] is the function that checks that the state
   of the information stored in the mutex is consistent with the action
   being performed, and the value with which to update the state.
   [evt] returns the corresponding mthread event. *)
let aux_mutex ~operation:op ~event analysis state : hook_sig = function
  | [_, offset] ->
    let prefix = "During mutex " ^ op.name in
    let conv v = catch_conversion analysis ~prefix v in
    let offset_conversion = Mt_memory.extract_int_possibly_zero offset in
    let offset, exact = conv offset_conversion "invalid mutex id" in
    if exact = `WithZero then
      warning analysis "Trying to %s a possibly uninitialized mutex." op.name;
    if offset <> 0 then
      let m = conv (find_mutex offset) "unknown mutex" in
      let state = MutexOp.check_and_write analysis state m op in
      let evt : event = event m in
      result analysis "%a" Event.pretty evt;
      register_event analysis evt;
      (* XXX: take which mutex is locked into account, and update only
         those values *)
      let with_external = sync_values analysis state in
      with_external, wrap_res  0
    else (
      warning analysis "Trying to %s uninitialized mutex. Ignoring" op.name;
      state, wrap_res (-1))

  | _ -> (* really unlikely unless the code and/or the C binding
            are really strange *)
    Mt_self.fatal "Incorrect mthread binding for mutex function"

let hook_init_mutex analysis state : hook_sig = function
  | [_, name] ->
    let pos = current_position analysis
    and name = Concurrency.Name.of_cvalue name in
    let mutex = Mutex.create pos name in
    analysis.all_mutexes <- Mutex.Set.add mutex analysis.all_mutexes;
    let state = MutexOp.check_and_write analysis state mutex MutexOp.initialize in
    result analysis "Initializing mutex %a" Mutex.pretty mutex;
    state, wrap_res (Mutex.id mutex)

  | _ -> (* really unlikely unless the code and/or the C binding
            are really strange *)
    Mt_self.fatal "Incorrect mthread binding for mutex function"


let hook_lock_mutex =
  aux_mutex ~operation:MutexOp.lock ~event:(fun id -> MutexLock id)

let hook_release_mutex =
  aux_mutex ~operation:MutexOp.unlock ~event:(fun id -> MutexRelease id)


(* -------------------------------------------------------------------------- *)
(** --- Misc                                                              --- *)
(* -------------------------------------------------------------------------- *)

let hook_dummy_message analysis state : hook_sig = function
  | (_, name) :: args ->
    let conv v = catch_conversion analysis ~prefix:"During unknown event" v in
    let name = Mt_memory.extract_constant_string name in
    let name = conv name "invalid event name" in
    let evt = Dummy (name, List.map snd args) in
    register_event analysis evt;
    result analysis "Monitored event: %a" Event.pretty evt;
    state, no_res

  | _ -> Mt_self.fatal "Incorrect mthread binding for unknown event"


(* -------------------------------------------------------------------------- *)
(** --- Main declarations                                                 --- *)
(* -------------------------------------------------------------------------- *)

(* All the Mthread builtin functions, together with their C name.
   The remainder of the conversion to the real type of the callback
   {Builtins.register_builtin} occurs in [Mt_main] *)
let mthread_builtins =
  [
    (* Threads *)
    "Frama_C_thread_create", hook_thread_creation;
    "Frama_C_thread_start", hook_thread_start;
    "Frama_C_thread_suspend", hook_thread_suspend;
    "Frama_C_thread_cancel", hook_thread_cancellation;
    "Frama_C_thread_exit", hook_thread_exit;
    "Frama_C_thread_id", hook_thread_id;
    "Frama_C_thread_priority", hook_thread_priority;
    (* Mutexes *)
    "Frama_C_mutex_init", hook_init_mutex;
    "Frama_C_mutex_lock", hook_lock_mutex;
    "Frama_C_mutex_unlock", hook_release_mutex;
    (* Message queues *)
    "Frama_C_queue_init", hook_queue_init;
    "Frama_C_queue_send", hook_send_msg;
    "Frama_C_queue_receive", hook_receive_msg;
    (* Misc *)
    "Frama_C_mthread_show", hook_dummy_message;
    (* Shared values *)
    "Frama_C_mthread_sync", hook_sync;
  ]
;;

let is_mthread_builtin s =
  List.exists (fun (s', _) -> s = s') mthread_builtins

(* Function to register as a callback of the Eva analysis if Mthread
   is enabled *)
let catch_functions_calls analysis (stack : Callstack.callstack) kf state kind =
  let f = Kernel_function.get_name kf in
  (* If an Mthread builtin has been called as main, we fail immediately.
     In fact, this case should not happen, because we reject calls to __FRAMA_C_*
     functions as main or during thread spawning. We could detect when the stack
     has only one element (i.e. pthread_* has been called as main), but the error
     message arrives too late, and is not really readable *)
  if is_mthread_builtin f && Option.is_none (Callstack.pop stack) then
    Mt_self.abort "Thread function %s called as starting thread function" f;
  (* Warn on concurrency library functions without stubs. *)
  if kind = `Spec then
    Mt_lib.warn_on_unsupported_library_function kf;
  analysis.curr_stack <- stack;
  if Callstack.is_empty stack then
    (* This is the entry point of the analysis, the events stack needs to be
       empty. *)
    analysis.curr_events_stack <- [];
  if Callstack.is_empty analysis.curr_stack &&
     Thread.is_main analysis.curr_thread.th_eva_thread then begin
    (* Beginning of the main thread (kf being the entry point). For the main
       thread, it might have not been registered yet if we are at the
       first iteration. *)
    let th = main_thread kf state in
    (* This call registers the main thread on the first run, and essentially
       does nothing afterwards *)
    let th = spawn_thread analysis th.th_eva_thread
        th.th_stack th.th_fun th.th_init_state th.th_params None in
    if analysis.main_thread != th then begin
      (* On the first run, the record [th] is created. It is not contained
         anywhere else, so we update the fields below. *)
      analysis.main_thread <- th;
      analysis.curr_thread <- th;
      (* We are currently computing this thread (the main one) and we have
         just started, no need to recompute it at the next iteration *)
      th.th_to_recompute <- SetRecomputeReason.empty;
      (* On the first iteration, also register the interrupt handlers *)
      let interrupt_handlers = Mt_options.InterruptHandlers.get () in
      let interrupts, state =
        Kernel_function.Set.fold
          (fun kf_interrupt (interrupts, state) ->
             (* Create and spawn the interrupt thread *)
             let th = interrupt_thread kf_interrupt state in
             let th =
               spawn_thread analysis th.th_eva_thread th.th_stack th.th_fun
                 th.th_init_state th.th_params None
             in
             (* Start the interrupt thread *)
             let state =
               Mt_ids.write_id_state state (Mt_ids.of_thread th.th_eva_thread) 1
             in
             (th :: interrupts, state))
          interrupt_handlers
          ([], state)
      in
      if interrupts != [] then begin
        (* If any interrupt handler has been registered, then their initial
           state and the initial state of the main thread need to be updated
           so that they all are considered running. *)
        List.iter
          (fun th ->
             let _ = update_initial_state analysis th.th_eva_thread state in ())
          (th :: interrupts)
      end
    end
  end;
  push_function_call analysis;
  (* If the function is a leaf one, we register the accesses that occur
     through \assigns ACSL specifications, then pop the stack. If there is a
     definition, the registering will be done by another hook, at the end of
     the execution of the function *)
  match kind with
  | `Spec | `Builtin ->
    Mt_shared_vars.register_concurrent_var_accesses analysis (`Leaf state);
    pop_function_call analysis;
  | `Body | `Reuse -> ()

(* Function registered by [Cvalue_callbacks.register_call_results_hook].
   Given the end states of a function with a definition, records the variable
   accesses it did. *)
let catch_functions_record analysis stack _kf _pre_state = function
  | `Body (Cvalue_callbacks.{before_stmts; after_stmts}, i) ->
    analysis.curr_stack <- stack;
    let hbefore = Lazy.force before_stmts in
    let hafter = Lazy.force after_stmts in
    Mt_shared_vars.register_concurrent_var_accesses analysis (`Final hbefore);
    register_memory_states analysis ~before:hbefore ~after:hafter;
    let cur_events = curr_events analysis in
    Datatype.Int.Hashtbl.add analysis.memexec_cache i cur_events;
    pop_function_call analysis;
  | `Reuse i ->
    let events = Datatype.Int.Hashtbl.find analysis.memexec_cache i in
    (* Merge the memoized results in the current analysis *)
    register_multiple_events analysis events;
    pop_function_call analysis;
  | `Builtin _ | `Spec _ -> ()
