(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Server Main Process                                                --- *)
(* -------------------------------------------------------------------------- *)

module Senv = Server_parameters
module Signals = Set.Make(String)

(* -------------------------------------------------------------------------- *)
(* --- Registry                                                           --- *)
(* -------------------------------------------------------------------------- *)

type kind = [ `GET | `SET | `EXEC ]
let string_of_kind = function `GET -> "GET" | `SET -> "SET" | `EXEC -> "EXEC"
let pp_kind fmt kd = Format.pp_print_string fmt (string_of_kind kd)

let registry = Hashtbl.create 32

let register (kind : kind) request handler =
  if Hashtbl.mem registry request then
    Server_parameters.failure "Request '%s' already registered" request
  else
    Hashtbl.add registry request (kind,handler)

let find request =
  try Some (Hashtbl.find registry request)
  with Not_found -> None

let exec request data = (snd (Hashtbl.find registry request)) data

(* -------------------------------------------------------------------------- *)
(* --- Public API                                                         --- *)
(* -------------------------------------------------------------------------- *)

type json = Json.t

type 'a request = [
  | `Poll
  | `Request of 'a * string * json
  | `Kill of 'a
  | `SigOn of string
  | `SigOff of string
  | `Shutdown
]

type 'a response = [
  | `Data of 'a * json
  | `Error of 'a * string
  | `Killed of 'a
  | `Signal of string
  | `Rejected of 'a
  | `CmdLineOn
  | `CmdLineOff
]

type 'a message = {
  requests : 'a request list ;
  callback : 'a response list -> unit ;
}

(* Private API: *)

type 'a process = {
  id : 'a ;
  request : string ;
  data : json ;
  handler : json -> json ;
  yield : bool ;
  mutable killed : bool ;
}

type 'a pending =
  | Process of 'a process
  | Command of (unit -> unit)

type 'a running =
  | Idle (* Server is waiting for requests *)
  | CmdLine (* Frama-C command line is running *)
  | ExecRequest of 'a process (* Running EXEC process *)

(* Server with request identifier (RqId) of type ['a] *)
type 'a server = {
  pretty : Format.formatter -> 'a -> unit ; (* RqId printer *)
  equal : 'a -> 'a -> bool ; (* RqId equality *)
  polling : int ; (* server polling, in milliseconds *)
  fetch : unit -> 'a message option ; (* fetch some client message *)
  q_in : 'a pending Queue.t ; (* queue of pending `EXEC and `GET jobs *)
  q_out : 'a response Queue.t ; (* queue of pending responses *)
  mutable daemon : Async.daemon option ; (* Async.yield daemon *)
  mutable s_listen : Signals.t ; (* signals the client is listening to *)
  mutable s_emitted : Signals.t ; (* emitted signals enqueued *)
  mutable s_pending : Signals.t ; (* emitted signals not enqueued yet *)
  mutable shutdown : bool ; (* server has been asked to shut down *)
  mutable running : 'a running ; (* server running state *)
  mutable cmdline : bool option ; (* cmdline signal management *)
}

exception Killed

(* -------------------------------------------------------------------------- *)
(* --- Debug                                                              --- *)
(* -------------------------------------------------------------------------- *)

let pp_request pp fmt (r : _ request) =
  match r with
  | `Poll -> Format.fprintf fmt "Poll"
  | `Shutdown -> Format.fprintf fmt "Shutdown"
  | `SigOn sg -> Format.fprintf fmt "Signal %S : on" sg
  | `SigOff sg -> Format.fprintf fmt "Signal %S : off" sg
  | `Kill id -> Format.fprintf fmt "Kill [%a]" pp id
  | `Request(id,request,data) ->
    if Senv.debug_atleast 2 then
      Format.fprintf fmt "@[<hov 2>Request [%a] %s@ %a@]"
        pp id request Data.pretty data
    else
      Format.fprintf fmt "Request [%a] %s" pp id request

let pp_process pp fmt (p : _ process) =
  Format.fprintf fmt "Execute %s:%a" p.request pp p.id

let pp_response pp fmt (r : _ response) =
  match r with
  | `Error(id,err) -> Format.fprintf fmt "Error %a: %s" pp id err
  | `Rejected id -> Format.fprintf fmt "Rejected %a" pp id
  | `Killed id -> Format.fprintf fmt "Killed %a" pp id
  | `Signal sg -> Format.fprintf fmt "Signal %S" sg
  | `CmdLineOn -> Format.pp_print_string fmt "CmdLine On"
  | `CmdLineOff -> Format.pp_print_string fmt "CmdLine Off"
  | `Data(id,data) ->
    if Senv.debug_atleast 3 then
      Format.fprintf fmt "@[<hov 2>Response %a:@ %a@]" pp id Data.pretty data
    else
      Format.fprintf fmt "Replied %a" pp id

let pp_running pp fmt = function
  | Idle -> Format.pp_print_string fmt "Idle"
  | CmdLine -> Format.pp_print_string fmt "CmdLine"
  | ExecRequest { id } -> Format.fprintf fmt "ExecRequest [%a]" pp id

(* -------------------------------------------------------------------------- *)
(* --- Request Handling                                                   --- *)
(* -------------------------------------------------------------------------- *)

let run proc : _ response =
  try
    let data = proc.handler proc.data in
    `Data(proc.id,data)
  with
  | Killed -> `Killed proc.id
  | Data.InputError msg -> `Error(proc.id,msg)
  | Sys.Break as exn -> raise exn (* Silently pass the exception *)
  | Globals.No_such_entry_point msg ->
    (* Same as in Boot.play_analysis. *)
    Kernel.error "%s" msg;
    `Error(proc.id, msg)
  | exn when Cmdline.catch_at_toplevel exn ->
    Senv.warning "[%s] Uncaught exception:@\n%s"
      proc.request (Cmdline.protect exn) ;
    `Error(proc.id,Printexc.to_string exn)

let delayed process =
  if Senv.debug_atleast 1 then
    Some (fun d -> Senv.debug "No yield since %dms during %s" d process)
  else None

let execute_process server ?yield proc =
  Senv.debug ~level:2 "%a" (pp_process server.pretty) proc ;
  let resp = match yield with
    | Some yield when proc.yield ->
      Async.with_progress
        ~debounced:server.polling
        ?on_delayed:(delayed proc.request)
        yield run proc
    | _ -> run proc
  in Queue.push resp server.q_out

(* -------------------------------------------------------------------------- *)
(* --- Signals                                                            --- *)
(* -------------------------------------------------------------------------- *)

type signal = string
let signal_name s = s
let signals = Hashtbl.create 32
let signal s =
  if Hashtbl.mem signals s then
    ( Server_parameters.failure "Signal '%s' already registered" s ; "" )
  else
    ( Hashtbl.add signals s [] ; s )

let () = Hashtbl.add signals "" []

let on_signal s callback =
  let ds = Hashtbl.find signals s in
  Hashtbl.replace signals s (callback :: ds)

let notify s a =
  match Hashtbl.find signals s with
  | ds -> List.iter (fun f -> f a) ds
  | exception Not_found -> ()

let nop _s = ()
let emitter = ref nop
let emit s = !emitter s

(* -------------------------------------------------------------------------- *)
(* --- Processing Requests                                                --- *)
(* -------------------------------------------------------------------------- *)

let raise_if_killed = function
  | Idle -> ()
  | CmdLine -> ()
  | ExecRequest { killed } -> if killed then raise Killed

let kill_running ?id s =
  match s.running with
  | Idle -> ()
  | CmdLine -> if id = None then Async.cancel ()
  | ExecRequest p ->
    match id with
    | None -> p.killed <- true
    | Some id -> if s.equal id p.id then p.killed <- true

let kill_request eq id = function
  | Process p -> if eq id p.id then p.killed <- true
  | Command _ -> ()

let process_request (server : 'a server) (request : 'a request) : unit =
  if Senv.debug_atleast 1 && (Senv.debug_atleast 2 || request <> `Poll) then
    Senv.debug "%a" (pp_request server.pretty) request ;
  match request with
  | `Poll -> ()
  | `Shutdown ->
    begin
      kill_running server ;
      Queue.clear server.q_in ;
      Queue.clear server.q_out ;
      server.shutdown <- true ;
    end
  | `SigOn sg ->
    begin
      server.s_listen <- Signals.add sg server.s_listen ;
      if Signals.mem sg server.s_pending then
        begin
          server.s_emitted <- Signals.add sg server.s_emitted ;
          server.s_pending <- Signals.remove sg server.s_pending ;
          Queue.push (`Signal sg) server.q_out ;
        end ;
      notify sg true ;
    end
  | `SigOff sg ->
    begin
      server.s_listen <- Signals.remove sg server.s_listen ;
      notify sg false ;
    end
  | `Kill id ->
    begin
      kill_running ~id server ;
      let set_killed = kill_request server.equal id in
      Queue.iter set_killed server.q_in ;
    end
  | `Request(id,request,data) ->
    begin
      match find request with
      | None ->
        Senv.warning ~once:true "Unknown request %s" request ;
        Senv.debug "Rejected %a" server.pretty id ;
        Queue.push (`Rejected id) server.q_out
      | Some( kind , handler ) ->
        let yield = match kind with `GET | `SET -> false | `EXEC -> true in
        let proc = {
          id ; request ; handler ; data ;
          yield ; killed = false ;
        } in
        match kind with
        | `GET -> execute_process server proc
        | `SET | `EXEC -> Queue.push (Process proc) server.q_in
    end

(* -------------------------------------------------------------------------- *)
(* --- Fetching a Bunck of Messages                                       --- *)
(* -------------------------------------------------------------------------- *)

let communicate server =
  Senv.debug ~level:3 "fetch" ;
  match server.fetch () with
  | None -> false
  | Some message ->
    Senv.debug ~level:2 "message(s) received" ;
    let error =
      try List.iter (process_request server) message.requests ; None
      with exn -> Some exn in (* re-raised after message reply *)
    let pool = ref [] in
    Queue.iter (fun r -> pool := r :: !pool) server.q_out ;
    Option.iter
      (fun cmd ->
         pool := (if cmd then `CmdLineOn else `CmdLineOff) :: !pool ;
      ) server.cmdline ;
    pool := List.rev !pool ;
    Queue.clear server.q_out ;
    server.cmdline <- None ;
    server.s_emitted <- Signals.empty ;
    Senv.debug ~level:2 "response(s) callback" ;
    if Senv.debug_atleast 2 then
      List.iter (Senv.debug "%a" (pp_response server.pretty)) !pool ;
    message.callback !pool ;
    Option.iter raise error ;
    true

(* -------------------------------------------------------------------------- *)
(* --- Yielding & Signaling                                               --- *)
(* -------------------------------------------------------------------------- *)

let do_yield server () =
  raise_if_killed server.running ;
  ignore ( communicate server )

let do_signal server s =
  if Signals.mem s server.s_listen then
    begin
      if not (Signals.mem s server.s_emitted) then
        begin
          server.s_emitted <- Signals.add s server.s_emitted ;
          Queue.push (`Signal s) server.q_out ;
        end
    end
  else
    server.s_pending <- Signals.add s server.s_pending

(* -------------------------------------------------------------------------- *)
(* --- One Step Process                                                   --- *)
(* -------------------------------------------------------------------------- *)

let rec process server =
  if Queue.is_empty server.q_in then
    communicate server
  else
    match Queue.pop server.q_in with
    | Process { killed } when killed -> process server
    | pending ->
      try
        let yield = do_yield server in
        begin
          match pending with
          | Process p ->
            server.running <- ExecRequest p ;
            execute_process server ~yield p
          | Command cmd ->
            server.running <- CmdLine ;
            Async.with_progress
              ~debounced:server.polling
              ?on_delayed:(delayed "<async>")
              yield cmd ()
        end ;
        server.running <- Idle ;
        true
      with exn ->
        server.running <- Idle ;
        raise exn

(* -------------------------------------------------------------------------- *)
(* --- Server Main Loop                                                   --- *)
(* -------------------------------------------------------------------------- *)

let in_range ~min:a ~max:b v = min (max a v) b

let kill () = raise Killed

let rooters = ref []
let daemons = ref []
let once callback = rooters := !rooters @ [ callback ]
let on callback = daemons := !daemons @ [ callback ]

let set_active activity =
  begin
    System_config.set_gui activity [@alert "-system_config_set_gui"] ;
    if activity then
      begin
        List.iter (fun f -> try f () with _ -> ()) !rooters ;
        rooters := [] ;
      end ;
    List.iter (fun f -> try f activity with _ -> ()) !daemons ;
  end

let is_active () = System_config.is_gui ()

let create ~pretty ?(equal=(=)) ~fetch () =
  let polling = in_range ~min:1 ~max:200 (Senv.Polling.get ()) in
  {
    fetch ; polling ; equal ; pretty ;
    q_in = Queue.create () ;
    q_out = Queue.create () ;
    s_listen = Signals.empty ;
    s_emitted = Signals.empty ;
    s_pending = Signals.empty ;
    daemon = None ;
    running = Idle ;
    cmdline = None ;
    shutdown = false ;
  }

(* -------------------------------------------------------------------------- *)
(* --- Async                                                              --- *)
(* -------------------------------------------------------------------------- *)

let schedule server job data =
  let status = ref Task.Yield in
  let command () =
    match !status with
    | Task.Return _ -> ()
    | _ ->
      let st =
        try Task.Result (job data)
        with
        | Async.Cancel -> Task.Canceled
        | exn -> Task.Failed exn
      in status := Task.Return st
  in
  let ping coin =
    match coin, !status with
    | Task.Kill, Task.Yield ->
      let st = Task.Return Task.Canceled in
      status := st ; st
    | _, st -> st
  in
  Queue.push (Command command) server.q_in ;
  Task.async ping

type scheduler = Offline | Server : 'a server -> scheduler

let scheduler = ref Offline

let async job data =
  match !scheduler with
  | Offline -> Task.call job data
  | Server s -> schedule s job data

(* -------------------------------------------------------------------------- *)
(* --- Start / Stop                                                       --- *)
(* -------------------------------------------------------------------------- *)

(* public API ; shall be scheduled at command line main stage *)
let start server =
  begin
    Senv.debug ~level:2 "Server started (was %a)"
      (pp_running server.pretty) server.running ;
    server.running <- CmdLine ;
    server.cmdline <- Some true ;
    emitter := do_signal server ;
    match server.daemon with
    | Some _ -> ()
    | None ->
      begin
        Senv.feedback "Server enabled." ;
        let daemon =
          Async.on_progress
            ~debounced:server.polling
            ?on_delayed:(delayed "command line")
            (do_yield server)
        in
        server.daemon <- Some daemon ;
        set_active true ;
      end
  end

(* public API ; can be invoked to force server shutdown *)
let stop server =
  begin
    Senv.debug ~level:2 "Stopping server (was %a)"
      (pp_running server.pretty) server.running ;
    kill_running server ;
    emitter := nop ;
    match server.daemon with
    | None -> ()
    | Some daemon ->
      begin
        Senv.feedback "Server disabled." ;
        server.daemon <- None ;
        server.running <- Idle ;
        server.cmdline <- None ;
        Async.off_progress daemon ;
        set_active false ;
      end
  end

(* internal only ; invoked by run when command line is finished *)
let foreground server =
  begin
    Senv.debug ~level:2 "Server foreground (was %a)"
      (pp_running server.pretty) server.running ;
    server.running <- Idle ;
    server.cmdline <- Some false ;
    emitter := do_signal server ;
    Task.on_idle := Async.while_progress ~debounced:50 ;
    scheduler := Server server ;
    match server.daemon with
    | None -> ()
    | Some daemon ->
      begin
        server.daemon <- None ;
        Async.off_progress daemon ;
      end
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Loop                                                          --- *)
(* -------------------------------------------------------------------------- *)

(* public API ; shall be invoked at command line normal exit *)
let run server =
  try
    ( (* TODO: catch-break to be removed once Why3 signal handler is fixed *)
      Sys.catch_break true
    ) ;
    foreground server ;
    set_active true ;
    Senv.feedback "Server running." ;
    begin
      try
        while not server.shutdown do
          let activity = process server in
          if not activity then Async.sleep server.polling
        done ;
      with Sys.Break -> () (* Ctr+C, just leave the loop normally *)
    end;
    Senv.feedback "Server shutdown." ;
    emitter := nop ;
    scheduler := Offline ;
    set_active false ;
  with exn ->
    Senv.feedback "Server interrupted (fatal error)." ;
    emitter := nop ;
    scheduler := Offline ;
    set_active false ;
    raise exn

(* -------------------------------------------------------------------------- *)
