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
  ss          : string;      (* generated .ss (HIP core) for this function *)
  ss_clines   : int list;    (* C source line for each .ss line (0 if unknown),
                                in order, so the GUI can link .ss <-> source *)
}

let empty =
  { verdict = "UNKNOWN"; obligations = []; fidelity = []; ss = ""; ss_clines = [] }

let table : (string, info) Hashtbl.t = Hashtbl.create 16

let clear () = Hashtbl.clear table
let set name info = Hashtbl.replace table name info
let get name = match Hashtbl.find_opt table name with Some i -> i | None -> empty
