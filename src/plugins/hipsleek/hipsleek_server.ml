(* ------------------------------------------------------------------ *)
(* Server request feeding the Ivette "HipSleek Proof" panel.          *)
(*                                                                     *)
(* Given the currently-selected function (a declaration), return its   *)
(* structured HipSleek proof info: overall verdict, the list of proof  *)
(* obligations (PRE/POST/BIND/PRE_REC with proved/unproved), and any   *)
(* translation-fidelity warnings. Data comes from Hipsleek_store,      *)
(* populated by Hipsleek_register after each -hipsleek run.            *)
(*                                                                     *)
(* Registered at module load (top-level let ()), independent of the    *)
(* -hipsleek flag, so `frama-c -server-tsc` generates the TS API.      *)
(* ------------------------------------------------------------------ *)

open Server
module Md = Markdown

let package =
  Package.package ~plugin:"hipsleek" ~title:"HipSleek Services" ()

(* One proof obligation. *)
module Obligation : Data.S with type t = Hipsleek_run.obligation =
struct
  type t = Hipsleek_run.obligation
  let jtype =
    Data.declare ~package ~name:"obligation" @@
    Jrecord [
      "kind",   Jstring ;
      "line",   Jnumber ;   (* generated .ss line *)
      "cline",  Jnumber ;   (* C source line (0 if unknown) *)
      "proved", Jboolean ;
      "entail", Jstring ;
    ]
  let to_json (o : t) =
    Json.of_fields [
      "kind",   Json.of_string o.Hipsleek_run.kind ;
      "line",   Json.of_int    o.Hipsleek_run.line ;
      "cline",  Json.of_int    o.Hipsleek_run.cline ;
      "proved", Json.of_bool   o.Hipsleek_run.proved ;
      "entail", Json.of_string o.Hipsleek_run.entail ;
    ]
  let of_json _ = failwith "Hipsleek.Obligation.of_json"
end

(* Per-function proof info returned to the panel. *)
module ProofInfo : Data.S with type t = Hipsleek_store.info =
struct
  type t = Hipsleek_store.info
  let jtype =
    Data.declare ~package ~name:"proofInfo" @@
    Jrecord [
      "verdict",     Jstring ;
      "obligations", Jarray Obligation.jtype ;
      "fidelity",    Jarray Jstring ;
    ]
  let to_json (i : t) =
    Json.of_fields [
      "verdict",     Json.of_string i.Hipsleek_store.verdict ;
      "obligations", Json.of_list
        (List.map Obligation.to_json i.Hipsleek_store.obligations) ;
      "fidelity",    Json.of_list
        (List.map Json.of_string i.Hipsleek_store.fidelity) ;
    ]
  let of_json _ = failwith "Hipsleek.ProofInfo.of_json"
end

let () =
  Request.register
    ~package ~kind:`GET ~name:"getProofInfo"
    ~descr:(Md.plain "HipSleek proof info for the selected function")
    ~input:(module Kernel_ast.Decl)
    ~output:(module ProofInfo)
    begin function
      | Printer_tag.SFunction kf ->
        Hipsleek_store.get (Kernel_function.get_name kf)
      | _ -> Hipsleek_store.empty
    end
