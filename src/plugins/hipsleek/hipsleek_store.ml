(* ------------------------------------------------------------------ *)
(* Per-function HipSleek proof info, keyed by function name.           *)
(*                                                                     *)
(* Populated by Hipsleek_register after each -hipsleek run, and read   *)
(* by the server request (Hipsleek_server) that feeds the Ivette       *)
(* "HipSleek Proof" panel. Module-level table: it persists for the     *)
(* session and is simply empty until an analysis has run.              *)
(* ------------------------------------------------------------------ *)

type info = {
  verdict     : string;                    (* SUCCESS | FAIL | ERROR | UNKNOWN *)
  obligations : Hipsleek_run.obligation list;
  fidelity    : string list;
}

let empty = { verdict = "UNKNOWN"; obligations = []; fidelity = [] }

let table : (string, info) Hashtbl.t = Hashtbl.create 16

let clear () = Hashtbl.clear table
let set name info = Hashtbl.replace table name info
let get name = match Hashtbl.find_opt table name with Some i -> i | None -> empty
