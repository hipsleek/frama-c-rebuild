(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Cache Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

type mode = NoCache | Update | Replay | Rebuild | Offline | Cleanup

let hits = ref 0
let miss = ref 0
let removed = ref 0
let cleanup = Hashtbl.create 0
(* used entries, never to be reset since cleanup is performed at exit *)

let get_hits () = !hits
let get_miss () = !miss
let get_removed () = !removed

let mark_cache ~mode hash =
  if mode = Cleanup || Wp_parameters.is_interactive () then
    Hashtbl.replace cleanup hash ()

(* -------------------------------------------------------------------------- *)
(* --- Cache Management                                                   --- *)
(* -------------------------------------------------------------------------- *)

let parse_mode ~origin ~fallback = function
  | "none" -> NoCache
  | "update" -> Update
  | "replay" -> Replay
  | "rebuild" -> Rebuild
  | "offline" -> Offline
  | "cleanup" -> Cleanup
  | m ->
    Wp_parameters.warning ~current:false
      "Unknown %s mode %S (use %s instead)" origin m fallback ;
    raise Not_found

module MODE = WpContext.StaticGenerator(Datatype.Unit)
    (struct
      type key = unit
      type data = mode
      let name = "Wp.Cache.mode"
      let compile () =
        let env = "FRAMAC_WP_CACHE" in
        try
          if Wp_parameters.Cache.is_set ()
          then
            let mode = Wp_parameters.Cache.get() in
            parse_mode ~origin:"-wp-cache" ~fallback:env mode
          else raise Not_found
        with Not_found ->
        try
          match Sys.getenv_opt env with
          | None | Some "" -> raise Not_found
          | Some mode -> parse_mode ~origin:env ~fallback:"none" mode
        with Not_found -> Update
    end)

let hooks = ref []
let add_hook_on_mode_update f = hooks := f :: !hooks
let clear_then_hooks () = MODE.clear () ; List.iter (fun h -> h ()) !hooks

let () =
  Wp_parameters.Cache.add_update_hook
    (fun _ _ -> clear_then_hooks ())

let get_mode = MODE.get
let set_mode m = MODE.set () m

let is_active = function
  | NoCache -> false
  | Replay | Offline | Update | Rebuild | Cleanup -> true

let is_updating = function
  | NoCache | Replay | Offline -> false
  | Update | Rebuild | Cleanup -> true

let time_fits time = function
  | None | Some 0.0 -> true
  | Some limit -> time <= limit

let steps_fits steps = function
  | None | Some 0 -> true
  | Some limit -> steps <= limit

let time_seized time = function
  | None | Some 0.0 -> false
  | Some limit -> limit <= time

let steps_seized steps steplimit =
  steps <> 0 &&
  match steplimit with
  | None | Some 0 -> false
  | Some limit -> limit <= steps

let promote ?timeout ?steplimit (res : VCS.result) =
  match res.verdict with
  | VCS.NoResult | VCS.Computing _ | VCS.Invalid | VCS.Failed -> VCS.no_result
  | VCS.Valid | VCS.Unknown ->
    if not (steps_fits res.prover_steps steplimit) then
      { res with verdict = Stepout }
    else
    if not (time_fits res.prover_time timeout) then
      { res with verdict = Timeout }
    else res
  | VCS.Timeout | VCS.Stepout ->
    if steps_seized res.prover_steps steplimit then
      { res with verdict = Stepout }
    else
    if time_seized res.prover_time timeout then
      { res with verdict = Timeout }
    else (* can be run a longer time or widely *)
      VCS.no_result

let file_from_hash ~create file_hash =
  Wp_parameters.CacheDir.get_file ~create_path:create (file_hash ^ ".json")

let get_cache_result ~mode hash =
  match mode with
  | NoCache | Rebuild -> VCS.no_result
  | Update | Cleanup | Replay | Offline ->
    try
      let hash = Lazy.force hash in
      let file = file_from_hash ~create:false hash in

      if not (Filesystem.exists file) then VCS.no_result
      else
        try
          mark_cache ~mode hash ;
          Json.load_file file |> ProofScript.result_of_json
        with err ->
          Wp_parameters.warning ~current:false ~once:true
            "invalid cache entry (%s)" (Printexc.to_string err) ;
          VCS.no_result
    with Not_found -> VCS.no_result

let set_cache_result ~mode hash prover result =
  match mode with
  | NoCache | Replay | Offline -> ()
  | Rebuild | Update | Cleanup ->
    let hash = Lazy.force hash in
    try
      let file = file_from_hash ~create:true hash in
      mark_cache ~mode hash ;
      ProofScript.json_of_result (Prover.Why3 prover) result
      |> Json.save_file file
    with err ->
      Wp_parameters.warning ~current:false ~once:true
        "can not update cache (%s)" (Printexc.to_string err)

let clear_result ~digest prover goal =
  try
    let hash = digest prover goal in
    let file = file_from_hash ~create:false hash in
    Filesystem.remove_file file
  with err ->
    Wp_parameters.warning ~current:false ~once:true
      "can not clean cache entry (%s)" (Printexc.to_string err)

let cleanup_cache () =
  let mode = get_mode () in
  if mode = Cleanup && (!hits > 0 || !miss > 0) then
    try
      if Wp_parameters.CacheDir.is_set () then
        Wp_parameters.warning ~current:false ~once:true
          "Cleanup mode deactivated with global cache."
      else
        let dir = Wp_parameters.CacheDir.get () in
        Filesystem.iter_dir
          (fun f ->
             if Filename.check_suffix f ".json" then
               let hash = Filename.chop_suffix f ".json" in
               if not (Hashtbl.mem cleanup hash) then
                 begin
                   incr removed ;
                   Filesystem.remove_file Filepath.(dir / f) ;
                 end
          ) dir ;
    with
    | Sys_error _ as exn ->
      Wp_parameters.warning ~current:false
        "Can not cleanup cache (%s)" (Printexc.to_string exn)
    | Not_found ->
      Wp_parameters.warning ~current:false
        "Cannot cleanup cache"

type 'a digest =
  Why3Provers.t -> 'a -> string

type 'a runner =
  timeout:float option -> steplimit:int option -> Why3Provers.t -> 'a ->
  VCS.result Task.task

let get_result ~digest ~runner ~timeout ~steplimit prover goal =
  let mode = get_mode () in
  match mode with
  | NoCache -> runner ~timeout ~steplimit prover goal
  | Offline ->
    let hash = lazy (digest prover goal) in
    let result = get_cache_result ~mode hash |> VCS.cached in
    if VCS.is_verdict result then incr hits else incr miss ;
    Task.return result
  | Update | Replay | Rebuild | Cleanup ->
    let hash = lazy (digest prover goal) in
    let result =
      get_cache_result ~mode hash
      |> promote ?timeout ?steplimit |> VCS.cached in
    if VCS.is_verdict result
    then
      begin
        incr hits ;
        Task.return result
      end
    else
      Task.finally
        (runner ~timeout ~steplimit prover goal)
        begin function
          | Task.Result result when VCS.is_verdict result ->
            incr miss ;
            set_cache_result ~mode hash prover result
          | _ -> ()
        end

(* -------------------------------------------------------------------------- *)
