(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Signal emission} *)

let signal_emitted = ref None

let abort () =
  signal_emitted := Some Self.Abort

let kill () =
  signal_emitted := Some Sys.Break

let reset () =
  signal_emitted := None


(** {2 Signal check} *)

let check () =
  Option.iter
    (fun exn -> raise exn)
    !signal_emitted


(** {2 System signal} *)

(* Registers signal handlers for SIGUSR1 and SIGINT to cleanly abort the Eva
   analysis. Returns a function that restores previous signal behaviors after
   the analysis. *)
let setup () =
  let warn () =
    Self.warning ~once:true "Stopping analysis at user request@."
  in
  let stop _ = warn (); kill () in
  let interrupt _ = warn (); raise Sys.Break in
  let register_handler signal handler =
    match Sys.signal signal (Sys.Signal_handle handler) with
    | previous_behavior -> fun () -> Sys.set_signal signal previous_behavior
    | exception Invalid_argument _ -> fun () -> ()
    (* Ignore: SIGURSR1 is not available on Windows,
       and possibly on other platforms. *)
  in
  let restore_sigusr1 = register_handler Sys.sigusr1 stop in
  let restore_sigint = register_handler Sys.sigint interrupt in
  fun () -> restore_sigusr1 (); restore_sigint ()


(** {2 Signal catching} *)

let protect_only_once = ref true

let protect f ~cleanup =
  protect_only_once := true;
  let cleanup () =
    try cleanup ()
    with e ->
      protect_only_once := false;
      raise e
  in
  try f ();
  with
  | Self.Abort | Log.(AbortError _ | AbortFatal _ | FeatureRequest _) as exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    cleanup ();
    Printexc.raise_with_backtrace exn backtrace
  | Sys.Break as exn when !protect_only_once ->
    let backtrace = Printexc.get_raw_backtrace () in
    cleanup ();
    Printexc.raise_with_backtrace exn backtrace
