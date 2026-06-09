(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let (>>=) = Task.(>>=)
let (>>>) = Task.(>>>)

(* -------------------------------------------------------------------------- *)
(* --- Task Server                                                        --- *)
(* -------------------------------------------------------------------------- *)

let server = ref None
let getprocs = function Some n -> n | None -> Wp_parameters.Procs.get ()
let server ?procs () =
  match !server with
  | Some s ->
    let np = getprocs procs in
    Task.set_procs s np ;
    Why3Provers.set_procs np ;
    s
  | None ->
    let np = getprocs procs in
    let s = Task.server ~procs:np () in
    Why3Provers.set_procs np ;
    server := Some s ; s

(* -------------------------------------------------------------------------- *)
(* --- Task Composition                                                   --- *)
(* -------------------------------------------------------------------------- *)

let dispatch ?(config=VCS.default) mode prover wpo =
  begin
    match prover with
    | Prover.Qed | CFG | Tactical -> Task.return VCS.no_result
    | Why3 prover ->
      let smoke = Wpo.is_smoke_test wpo in
      let kf = match Wpo.get_scope wpo with
        | Global -> None
        | Kf kf -> Some kf
      in
      ProverWhy3.prove
        ~timeout:(VCS.get_timeout ?kf ~smoke config)
        ~steplimit:(VCS.get_stepout config)
        ~memlimit:(VCS.get_memlimit config)
        ~mode ~prover wpo
  end

let silent _ = ()
let spawn_task ?(monitor=silent) ~all ~smoke
    (jobs : ('a * bool Task.task) list) =
  if jobs <> [] then
    begin
      let step = ref 0 in
      let monitored = ref [] in
      let finalized = ref false in
      let callback a r =
        if r then
          begin
            if smoke then
              begin
                finalized := true ;
                monitor (Some a) ;
              end
            else
            if not all && not !finalized then
              begin
                finalized := true ;
                monitor (Some a) ;
                List.iter Task.cancel !monitored ;
              end
          end
        else
          begin
            decr step ;
            if not !finalized && !step = 0 then
              monitor None ;
          end in
      let pack (a,t) = Task.thread (t >>= Task.call (callback a)) in
      step := List.length jobs ;
      monitored := List.map pack jobs ;
      let server = server () in
      List.iter (Task.spawn server) !monitored ;
    end

let started ?start wpo =
  match start with
  | None -> ()
  | Some f -> f wpo

let signal ?progress wpo msg =
  match progress with
  | None -> ()
  | Some f -> f wpo msg

let update ?result wpo prover res =
  Wpo.set_result wpo prover res ;
  match result with
  | None -> ()
  | Some f -> f wpo prover res

let simplify ?start ?result ?(commit=false) wpo =
  Server.Main.async
    (fun wpo ->
       let r = Wpo.get_result wpo Prover.Qed in
       VCS.( r.verdict == Valid ) ||
       begin
         started ?start wpo ;
         let ok = Wpo.reduce wpo in
         if commit || ok then
           let time = Wpo.qed_time wpo in
           let verdict = if ok then VCS.Valid else VCS.Unknown in
           let presult = VCS.result ~time verdict in
           (update ?result wpo Prover.Qed presult ; ok)
         else false
       end)
    wpo

let run_prover wpo ?config ?(mode=Prover.InteractiveMode.Batch) ?progress ?result prover =
  signal ?progress wpo (Prover.ident prover) ;
  dispatch ?config mode prover wpo >>>
  fun status ->
  let res = match status with
    | Task.Result r -> r
    | Task.Canceled -> VCS.no_result
    | Task.Timeout t -> VCS.timeout t
    | Task.Failed exn ->
      let msg = Task.error exn in
      Wp_parameters.warning ~current:false
        "@[<hov 2>Goal %s:@ running prover %s failed (%s)@]"
        (Wpo.get_label wpo) (Prover.ident prover) msg ;
      VCS.failed msg
  in
  let res = { res with solver_time = Wpo.qed_time wpo } in
  update ?result wpo prover res ;
  Task.return (VCS.is_valid res)

let prove wpo ?config ?mode ?start ?progress ?result prover =
  simplify ?start ?result wpo >>= fun succeed ->
  if succeed
  then Task.return true
  else (run_prover wpo ?config ?mode ?progress ?result prover)

let spawn wpo ~delayed
    ?config ?start ?progress ?result ?success provers =
  let provers = List.filter (fun (_,p) -> p <> Prover.Qed) provers in
  if provers<>[] then
    let monitor = match success with
      | None -> None
      | Some on_success ->
        Some
          begin function
            | None -> on_success wpo None
            | Some prover ->
              let r = Wpo.get_result wpo Prover.Qed in
              let prover =
                if VCS.( r.verdict == Valid ) then Prover.Qed else prover in
              on_success wpo (Some prover)
          end in
    let process (mode,prover) =
      prove wpo ?config ~mode ?start ?progress ?result prover in
    let all = Wp_parameters.RunAllProvers.get() in
    let smoke = Wpo.is_smoke_test wpo in
    spawn_task ?monitor ~all ~smoke
      (List.map
         (fun mp ->
            let prover = snd mp in
            let task = if delayed then Task.later process mp else process mp in
            prover , task
         ) provers)
  else
    let process = simplify ?start ?result ~commit:true wpo >>= fun ok ->
      begin
        match success with
        | None -> ()
        | Some on_success ->
          on_success wpo (if ok then Some Prover.Qed else None) ;
      end ;
      Task.return ()
    in
    let thread = Task.thread process in
    let server = server () in
    Task.spawn server thread

(* -------------------------------------------------------------------------- *)
