(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* Callstack tracking *)

let current : Callstack.t option ref = ref None

let get () = !current

let get_exn () =
  match !current with
  | None -> invalid_arg "callstack not initialized"
  | Some cs -> cs

let with_callstack ?finally callstack job x =
  let previous = !current in
  current := Some callstack;
  let finally () =
    Option.iter (fun f -> f ()) finally;
    current := previous
  in
  Fun.protect ~finally (fun () -> job x)
