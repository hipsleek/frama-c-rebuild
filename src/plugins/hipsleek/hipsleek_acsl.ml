open Cil_types

(* ------------------------------------------------------------------ *)
(* HipSleek results as ACSL extensions on the Frama-C AST              *)
(*                                                                     *)
(* The SL specs are verified by HIP on a separately generated .ss      *)
(* file (see Hipsleek_translate / Hipsleek_run); they are NOT verified *)
(* by Frama-C and are not part of the Cil AST. To make them visible in *)
(* the AST/source view and the GUI we attach them here as ACSL         *)
(* extensions (display-only overlay), and emit the HIP verdict as the  *)
(* property status of the per-function extension:                      *)
(*                                                                     *)
(*   - "hipsleek"       behavior clause : per-function requires/ensures *)
(*                       (+ a reference to the generated .ss), with the *)
(*                       hip verdict as its property status;           *)
(*   - "hipsleek_pred"  global clause   : the [SL_pred] view defs.      *)
(*                                                                     *)
(* The link between the Frama-C function and what HIP verified is by    *)
(* function name; the generated .ss reference makes it inspectable.    *)
(* ------------------------------------------------------------------ *)

let emitter =
  lazy (Emitter.create "HipSleek"
          [ Emitter.Property_status; Emitter.Funspec; Emitter.Global_annot ]
          ~correctness:[] ~tuning:[])

(* id -> text to pretty-print for each extension kind *)
let spec_table : (int, string) Hashtbl.t = Hashtbl.create 16
let pred_table : (int, string) Hashtbl.t = Hashtbl.create 16

let counter = ref 0
let fresh () = let n = !counter in incr counter; n

(* Trim whitespace and drop a trailing ';' so the extension's own clause
   terminator does not produce a doubled ";;". *)
let clean s =
  let s = String.trim s in
  let n = String.length s in
  if n > 0 && s.[n-1] = ';' then String.trim (String.sub s 0 (n-1)) else s

(* Avoid accidentally closing the enclosing /*@ ... */ comment by breaking
   any "*/" occurrence into "* /". *)
let sanitize s =
  let b = Buffer.create (String.length s) in
  String.iteri (fun i c ->
      if c = '/' && i > 0 && s.[i-1] = '*' then Buffer.add_string b " /"
      else Buffer.add_char b c
    ) s;
  Buffer.contents b

let make_printer tbl _pp fmt = function
  | Ext_id id ->
    (match Hashtbl.find_opt tbl id with
     | Some s -> Format.pp_print_string fmt (sanitize s)
     | None   -> ())
  | _ -> ()

(* Only used if the keyword is ever parsed from source text; we inject
   extensions programmatically, so this just allocates a fresh id. *)
let typer_stub _ctxt _loc _lexprs = Ext_id (fresh ())

let registered = ref false
let register () =
  if not !registered then begin
    registered := true;
    Acsl_extension.register_behavior ~plugin:"hipsleek" "hipsleek"
      typer_stub ~printer:(make_printer spec_table) true;
    Acsl_extension.register_global ~plugin:"hipsleek" "hipsleek_pred"
      typer_stub ~printer:(make_printer pred_table) false
  end

let status_of_verdict : Hipsleek_run.verdict -> Property_status.emitted_status =
  function
  | Hipsleek_run.Success -> Property_status.True
  (* A failed SL proof does not prove the contract false, so the honest
     status is "don't know" rather than False. *)
  | Hipsleek_run.Fail    -> Property_status.Dont_know
  | Hipsleek_run.Error _ -> Property_status.Dont_know

(* Attach the [SL_pred] view definitions as a global annotation. *)
let attach_preds emitter preds =
  List.iter (fun pred_text ->
      let id = fresh () in
      Hashtbl.replace pred_table id (clean pred_text);
      let loc = Fileloc.unknown in
      let ext =
        Logic_const.new_acsl_extension
          ~plugin:"hipsleek" "hipsleek_pred" loc false (Ext_id id)
      in
      Annotations.add_global emitter (Dextended (ext, [], loc))
    ) preds

(* Attach one function's SL spec as a "hipsleek" behavior clause, and
   emit the hip verdict (if any) as its property status.

   The contract clause prints ONLY the clean SL spec, so the AST/source view
   stays uncluttered. The verbose ESL proof [detail] is NOT printed inline;
   instead it is attached as a separate [ip_other] property on the function
   (OLContract), which carries the verdict status and shows up in the
   Properties panel when the function is selected -- i.e. on demand, not in
   the source comment. *)
let attach_function emitter ~name ?(detail="") ~sl verdict_opt =
  match Globals.Functions.find_by_name name with
  | exception Not_found ->
    Hipsleek_parameters.debug "no kernel function named %s; skipping" name
  | kf ->
    let loc = Kernel_function.get_location kf in
    let id = fresh () in
    Hashtbl.replace spec_table id (clean sl);
    let ext =
      Logic_const.new_acsl_extension
        ~plugin:"hipsleek" "hipsleek" loc true (Ext_id id)
    in
    Annotations.add_extended emitter kf ext;
    let emit_verdict ip =
      match verdict_opt with
      | None -> ()
      | Some v -> Property_status.emit emitter ~hyps:[] ip (status_of_verdict v)
    in
    (* Verdict marker on the clean contract clause. *)
    emit_verdict (Property.ip_of_extended (Property.ELContract kf) ext);
    (* Proof detail as a separate, non-printed property (shown on selection). *)
    if detail <> "" then
      emit_verdict (Property.ip_other detail (Property.OLContract kf))

(* Entry point: attach predicate defs + per-function SL specs/verdicts.
   [proof_logs] : per-function ESL proof detail (only when -hipsleek-proof-log).
   [fidelity]   : per-function translation-fidelity warnings. *)
let attach_all ~functions ~preds ~results ~proof_logs ~fidelity =
  register ();
  let emitter = Lazy.force emitter in
  attach_preds emitter preds;
  (* Fidelity warnings always go to the message log, so a green verdict on a
     lossily-translated function is never silent. *)
  List.iter (fun (name, warns) ->
      if warns <> [] then
        Hipsleek_parameters.warning
          "%s: generated .ss may differ from your C (%s)"
          name (String.concat "; " warns)
    ) fidelity;
  let verdict_of name =
    match
      List.find_opt (fun r -> r.Hipsleek_run.func_name = name) results
    with
    | Some r -> Some r.Hipsleek_run.verdict
    | None   -> None
  in
  let proof_of name = List.assoc_opt name proof_logs in
  let fidelity_of name =
    match List.assoc_opt name fidelity with Some l -> l | None -> []
  in
  (* Detail (proof + fidelity) is shown in the property only with the flag,
     keeping the default contract clean. *)
  let show_detail = Hipsleek_parameters.ProofLog.get () in
  List.iter (fun (name, sl_opt, _ss_proc) ->
      match sl_opt with
      | None -> ()  (* no SL spec for this function: nothing to attach *)
      | Some sl ->
        let detail =
          if not show_detail then ""
          else
            let p = match proof_of name with Some p -> [p] | None -> [] in
            let f =
              match fidelity_of name with
              | [] -> []
              | w  -> [ "fidelity: " ^ String.concat "; " w ]
            in
            String.concat "\n" (p @ f)
        in
        attach_function emitter ~name ~detail ~sl (verdict_of name)
    ) functions
