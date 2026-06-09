(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval
open Mt_utils
open Eva_ast
open Concurrency



module BuiltinsResults = struct
  module Info = struct
    let initial_values = [ ]
    let dependencies = [ Ast.self ]
  end
  include Hptmap.Make (Cil_datatype.Varinfo_Id) (Value) (Info)
  let cache_name s = Hptmap_sig.PersistentCache (datatype_name ^ "." ^ s)

  let top = empty

  let is_included =
    let cache = cache_name "is_included" in
    let decide_fst _b _l = true  (* r is top *) in
    let decide_snd _b _r = false (* l is top *) in
    let decide_both _ l r = Value.is_included l r in
    let decide_fast s t = if s == t then PTrue else PUnknown in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let narrow =
    let cache = cache_name "narrow" in
    let decide _ = Value.narrow in
    join ~cache ~symmetric:true ~idempotent:true ~decide

  let narrow l r = `Value (narrow l r)

  let join =
    let cache = cache_name "join" in
    let decide _ l r = Some (Value.join l r) in
    inter ~cache ~symmetric:true ~idempotent:true ~decide

  let enter_scope kind vars returns =
    let open Abstract_domain in
    match kind with
    | Global | Formal _ | Local _ -> returns
    | Result _ ->
      let write returns var = add var Value.top returns in
      List.fold_left write returns vars

  let leave_scope _ vars returns =
    let remove returns var = remove var returns in
    List.fold_left remove returns vars

  let write return value returns =
    match return with
    | None -> returns
    | Some var ->
      if mem var returns then add var value returns else returns

  let read var returns =
    try find var returns
    with Not_found -> Value.top
end



type return = { standard : Value.t }

module State = struct
  type state =
    { threads : Mt_register.Thread.t
    ; mutexes : Mt_register.Mutex.t
    ; return  : return
    ; results : BuiltinsResults.t
    }

  let default =
    { threads = Mt_register.Thread.empty
    ; mutexes = Mt_register.Mutex.empty
    ; return  = Value.{ standard = bottom }
    ; results = BuiltinsResults.empty
    }

  let top =
    { threads = Mt_register.Thread.top
    ; mutexes = Mt_register.Mutex.top
    ; return  = Value.{ standard = top }
    ; results = BuiltinsResults.top
    }

  include Datatype.Make_with_collections (struct
      type t = state
      let name = "Eva.Mt_domain.State"
      let reprs = [ default ; top ]

      let copy state =
        let threads  = Mt_register.Thread.copy state.threads in
        let mutexes  = Mt_register.Mutex.copy state.mutexes in
        let standard = Value.copy state.return.standard in
        let return   = { standard } in
        let results  = BuiltinsResults.copy state.results in
        { threads ; mutexes ; return ; results }

      let structural_descr =
        let open Structural_descr in
        let ths = Mt_register.Thread.packed_descr in
        let mxs = Mt_register.Mutex.packed_descr in
        let ret = pack (t_record Value.[| packed_descr |]) in
        let results = BuiltinsResults.packed_descr in
        t_record [| ths ; mxs ; ret ; results |]

      let pretty fmt state =
        Format.fprintf fmt "Threads :@.  @[<v>%a@]@."
          Mt_register.Thread.pretty state.threads ;
        Format.fprintf fmt "Mutexes :@.  @[<v>%a@]@."
          Mt_register.Mutex.pretty state.mutexes ;
        Format.fprintf fmt "Return  :@.  Standard : %a@."
          Value.pretty state.return.standard

      let compare_return l r =
        let open Value in
        compare l.standard r.standard

      let compare l r =
        Mt_register.Thread.compare l.threads r.threads
        <?> lazy (Mt_register.Mutex.compare l.mutexes r.mutexes)
        <?> lazy (compare_return l.return r.return)
        <?> lazy (BuiltinsResults.compare l.results r.results)

      let equal l r = compare l r = 0

      let hash_return t =
        Value.hash t.standard

      let hash t =
        Hashtbl.hash (
          Mt_register.Thread.hash t.threads,
          Mt_register.Mutex.hash t.mutexes,
          hash_return t.return,
          BuiltinsResults.hash t.results)
      let rehash = Datatype.identity
      let mem_project = Datatype.never_any_project
    end)

  let threads t = t.threads
  let mutexes t = t.mutexes
  let return t = t.return
end



let reset state =
  let open State in
  let return = { standard = Value.bottom } in
  { state with return }



module Datatype_with_Lattice = struct
  include State

  let name = "mthread"

  let is_included l r =
    Mt_register.Thread.is_included l.threads r.threads
    && Mt_register.Mutex.is_included l.mutexes r.mutexes
    && Value.is_included l.return.standard r.return.standard
    && BuiltinsResults.is_included l.results r.results

  let join l r =
    let threads = Mt_register.Thread.join l.threads r.threads in
    let mutexes = Mt_register.Mutex.join l.mutexes r.mutexes in
    let standard = Value.join l.return.standard r.return.standard in
    let return = { standard } in
    let results = BuiltinsResults.join l.results r.results in
    { threads ; mutexes ; return ; results }

  let widen _ _ pre post =
    let threads = Mt_register.Thread.join pre.threads post.threads in
    let mutexes = Mt_register.Mutex.join pre.mutexes post.mutexes in
    let standard = Value.widen pre.return.standard post.return.standard in
    let return = { standard } in
    let results = BuiltinsResults.join pre.results post.results in
    { threads ; mutexes ; return ; results }

  let narrow l r =
    let open Lattice_bounds.Bottom.Operators in
    let threads = Mt_register.Thread.narrow l.threads r.threads in
    let mutexes = Mt_register.Mutex.narrow l.mutexes r.mutexes in
    let standard = Value.narrow l.return.standard r.return.standard in
    let return = { standard } in
    let+ results = BuiltinsResults.narrow l.results r.results in
    { threads ; mutexes ; return ; results }
end



module Queries = struct
  let extract_expr ~oracle:_ _ _ _ = `Value (Value.top, None), Alarmset.all
  let extract_lval ~oracle:_ _ state lval _ =
    let State.{ results } = state in
    let value =
      match lval.node with
      | Var var, NoOffset -> BuiltinsResults.read var results
      | _ -> Value.top
    in
    `Value (value, None), Alarmset.all
end



module Transfer = struct
  open State

  module Builtins = Datatype.String.Hashtbl
  type builtin =
    pos:Position.local -> t -> (Value.t * exp) list -> (t * Value.t) Result.t
  let builtins : builtin Builtins.t = Builtins.create 17
  let mem_builtin name = Builtins.mem builtins name
  let find_builtin name = Builtins.find_opt builtins name
  let add_builtin name f = Builtins.add builtins name f

  let update _ state = `Value state

  let assign_return current_thread lval result return =
    let main = Thread.entry_point current_thread in
    let main_retres = Library_functions.get_retres_vi main in
    let main_return = Option.map Eva_ast.Build.var main_retres in
    if Option.equal Eva_ast.Lval.equal main_return (Some lval) then
      let bottom = Value.{ standard = bottom } in
      let f value = { standard = value } in
      Bottom.(map f result |> value ~bottom)
    else return

  let assign ~pos { lval } _exp assigned _valuation state =
    match Position.is_local pos with
    | false -> `Value state
    | true ->
      let { return } = reset state in
      let current_thread = Thread.from_position pos in
      let value = Eval.value_assigned assigned in
      let return = assign_return current_thread lval value return in
      `Value { state with return }

  let assume ~pos:_ _ _ _ state = `Value state

  let start_call  ~pos:_ _ _ _ state = `Value state

  let map_non_bottom f xs =
    let module E = struct exception Bottom end in
    let f v = match f v with `Value v -> v | `Bottom -> raise E.Bottom in
    try `Value (List.map f xs) with E.Bottom -> `Bottom

  let finalize_call ~pos call _ ~pre:_ ~post =
    let name = Kernel_function.get_name call.kf in
    match find_builtin name with
    | None -> `Value post
    | Some f ->
      let open Lattice_bounds.Bottom.Operators in
      let extract_arg arg = arg.concrete, arg.avalue in
      let arguments = List.map extract_arg call.arguments @ call.rest in
      let extract (exp, v) = Eval.value_assigned v >>-: fun v -> v, exp in
      let* params = map_non_bottom extract arguments in
      let error = (post, Value.top) in
      let (state, ret) = f ~pos post params |> Result.log ~error in
      let results = BuiltinsResults.write call.return ret state.results in
      `Value { state with results }

end

module Domain = struct
  type value = Value.t
  type location = Precise_locs.precise_location
  type origin = unit

  include Datatype_with_Lattice
  include Queries
  include Transfer
  include Domain_builder.Complete (Datatype_with_Lattice)

  let value_dependencies = Main_values.cval
  let location_dependencies = Main_locations.ploc

  let register_and_start_thread thread state =
    let open Result.Operators in
    let threads = state.threads in
    let* (threads, r) = Mt_register.Thread.register [thread] threads in
    let+ (threads, _) = Mt_register.Thread.start r threads in
    { state with threads }

  let create_main_thread state =
    register_and_start_thread Thread.main state

  let create_interrupt_handler_threads state =
    let open Result.Operators in
    List.fold_left
      (fun state interrupt ->
         let* state in
         register_and_start_thread interrupt state)
      state
      (Thread.interrupt_handlers ())

  let empty () =
    default
    |> create_main_thread
    |> create_interrupt_handler_threads
    |> Result.log ~error:default
  let logic_assign _ _ state = state
  let initialize_variable _ _ ~initialized:_ _ state = state
  let initialize_variable_using_type _ _ state  = state
  let relate _ _ = Base.SetLattice.empty

  (* The interferences computation uses the properties inferred by the Mthread
     domain after projection of abstract states, so for now we need to keep
     those properties in the projected state. *)
  let project _bases state = state

  (* This domain only infers information about the current analyzed thread:
     it must not inject interferences from other threads. *)
  let overwrite _bases ~on ~by:_ = on

  let enter_scope kind vars state =
    let state = reset state in
    let results = BuiltinsResults.enter_scope kind vars state.results in
    { state with results }

  let leave_scope kf vars state =
    let state = reset state in
    let results = BuiltinsResults.leave_scope kf vars state.results in
    { state with results }

  let thread_create ~pos state = function
    | (name, _) :: (func, _) :: args ->
      let open Result.Operators in
      let name = Name.of_cvalue name in
      let* func = Value.extract_fun func in
      let args = List.map fst args in
      let spawn f = Thread.spawn pos name f args in
      let th_list = List.map spawn func in
      let+ threads, return = Mt_register.Thread.register th_list state.threads in
      { state with threads }, return
    | _ -> Result.error "Invalid parameters@."

  let thread_update ~pos:_ f state = function
    | (id, _) :: [] ->
      let open Result.Operators in
      let+ (threads, return) = f id state.threads in
      { state with threads }, return
    | _ -> Result.error "Invalid parameters@."

  let thread_start   = thread_update Mt_register.Thread.start
  let thread_suspend = thread_update Mt_register.Thread.suspend
  let thread_cancel  = thread_update Mt_register.Thread.cancel

  let thread_id ~pos state = function
    | [] ->
      let cs = Position.Local.callstack pos in
      Result.ok (state, cs.thread |> Value.of_int)
    | _ :: _ -> Result.error "Invalid parameters@."

  let mutex_init ~pos state = function
    | (name, _) :: [] ->
      let open Result.Operators in
      let name = Name.of_cvalue name in
      let mutex = Mutex.create pos name in
      let+ (mutexes, return) = Mt_register.Mutex.register [mutex] state.mutexes in
      { state with mutexes }, return
    | _ -> Result.error "Invalid parameters@."

  let mutex_lock ~pos:_ state = function
    | (id, _) :: [] ->
      let open Result.Operators in
      let+ (mutexes, return) = Mt_register.Mutex.lock id state.mutexes in
      { state with mutexes }, return
    | _ -> Result.error "Invalid parameters@."

  let mutex_unlock ~pos:_ state = function
    | (id, _) :: [] ->
      let open Result.Operators in
      let+ (mutexes, return) = Mt_register.Mutex.unlock id state.mutexes in
      { state with mutexes }, return
    | _ -> Result.error "Invalid parameters@."

  let () = add_builtin "Frama_C_thread_create" thread_create
  let () = add_builtin "Frama_C_thread_start" thread_start
  let () = add_builtin "Frama_C_thread_suspend" thread_suspend
  let () = add_builtin "Frama_C_thread_cancel" thread_cancel
  let () = add_builtin "Frama_C_thread_id" thread_id
  let () = add_builtin "Frama_C_mutex_init" mutex_init
  let () = add_builtin "Frama_C_mutex_lock" mutex_lock
  let () = add_builtin "Frama_C_mutex_unlock" mutex_unlock
end

let have_builtins_in_globals () =
  let is_builtin kf = Domain.mem_builtin (Kernel_function.get_name kf) in
  Globals.Functions.fold (fun kf acc -> acc || is_builtin kf) false

let have_interrupt_handlers () =
  if Plugin.is_present "mt" then
    (* TODO: when -mt-interrupt-handlers becomes an Eva option, use it directly
       instead of using Dynamic.*)
    let opt_name = "-mt-interrupt-handlers" in
    let p =  Dynamic.Parameter.get_parameter opt_name in
    p.is_set ()
  else
    false

let domain =
  let name = "mthread" in
  let descr =
    "Domain for the analysis of concurrent programs. \
     Automatically enabled by the -mthread parameter."
  in
  let auto_enable () =
    (* TODO: When the options of Mthread are merged with Eva options, reassess
       how the domain should be enabled (only automatic detection, only specific
       option, mix of automatic and option with auto,true,false for instance).
       In any case it should be possible to explicitly deactivate the domain.*)
    let enable = have_builtins_in_globals () || have_interrupt_handlers () in
    if enable then
      Self.feedback ~once:true
        "Found concurrency builtins: enabling mthread domain";
    enable
  in
  Abstractions.Domain.register ~name ~descr ~experimental:true ~priority:2
    ~auto_enable (module Domain)
