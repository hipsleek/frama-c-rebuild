(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Server API for WP                                                  --- *)
(* -------------------------------------------------------------------------- *)

module P = Server.Package
module D = Server.Data
module R = Server.Request
module S = Server.States
module Md = Markdown
module AST = Server.Kernel_ast

module WP_Prover = Prover

let package = P.package ~plugin:"wp" ~title:"WP Main Services" ()

(* -------------------------------------------------------------------------- *)
(* --- WPO Index                                                          --- *)
(* -------------------------------------------------------------------------- *)

module INDEX = State_builder.Ref
    (Datatype.Make
       (struct
         include Datatype.Undefined
         type t = (string,Wpo.t) Hashtbl.t
         let name = "WpApi.INDEX.Datatype"
         let reprs = [ Hashtbl.create 0 ]
         let mem_project = Datatype.never_any_project
       end))
    (struct
      let name = "WpApi.INDEX"
      let dependencies = [ Ast.self ]
      let default () = Hashtbl.create 0
    end)

let indexGoal g =
  let id = g.Wpo.po_gid in
  let index = INDEX.get () in
  if not (Hashtbl.mem index id) then Hashtbl.add index id g ; id

module Goal : D.S with type t = Wpo.t =
struct
  type t = Wpo.t
  let jtype = D.declare ~package ~name:"goal"
      ~descr:(Md.plain "Proof Obligations") (Jkey "wpo")
  let of_json js = Hashtbl.find (INDEX.get ()) (Json.string js)
  let to_json g = `String (indexGoal g)
end

(* -------------------------------------------------------------------------- *)
(* --- Provers                                                            --- *)
(* -------------------------------------------------------------------------- *)

module Prover =
struct
  type t = Prover.t
  let jtype = D.declare ~package ~name:"prover"
      ~descr:(Md.plain "Prover Identifier") (Jkey "prover")
  let to_json prv = `String (WP_Prover.ident prv)
  let of_json js =
    match Prover.parse @@ Json.string js with
    | Some prv -> prv
    | None -> D.failure "Unknown prover name"
end

module Provers = D.Jlist(Prover)

let signal = ref None

let getProvers () =
  List.filter WP_Prover.is_extern @@ WP_Prover.provers ()

let () =
  R.register ~package
    ~name:"setProverState"
    ~descr:(Md.plain "Select/unselect prover")
    ~kind:`SET
    ~input:(module D.Jpair(Prover)(D.Jbool))
    ~output:(module D.Junit)
    begin fun (p, v) ->
      WP_Prover.set_prover p ~state:v ;
      Option.iter R.emit !signal
    end

let _ =
  let s =
    S.register_value
      ~package ~name:"provers"
      ~descr:(Md.plain "Get all available provers")
      ~output:(module Provers)
      ~get:(fun () -> getProvers())
      ()
  in
  signal := Some s

let _ : WP_Prover.t S.array =
  let model = S.model () in
  S.column ~name:"name" ~descr:(Md.plain "Prover Name")
    ~data:(module D.Jalpha) ~get:WP_Prover.name model ;
  S.column ~name:"version" ~descr:(Md.plain "Prover Version")
    ~data:(module D.Jalpha) ~get:WP_Prover.version model ;
  S.column ~name:"descr" ~descr:(Md.plain "Prover Full Name (description)")
    ~data:(module D.Jalpha) ~get:(WP_Prover.title ~version:true) model ;
  S.column model ~name:"extern" ~descr:(Md.plain "Why3 or internal")
    ~data:(module D.Jbool) ~get:WP_Prover.is_extern ;
  S.column model ~name:"auto" ~descr:(Md.plain "Automatic solver")
    ~data:(module D.Jbool) ~get:WP_Prover.is_auto ;
  S.column model ~name:"active" ~descr:(Md.plain "Whether it is enabled")
    ~data:(module D.Jbool) ~get:WP_Prover.enabled ;
  S.register_array ~package
    ~name:"ProverInfos" ~descr:(Md.plain "Available Provers")
    ~key:WP_Prover.ident
    ~keyName:"prover"
    ~keyType:Prover.jtype
    ~iter:(fun f -> List.iter f @@ WP_Prover.provers ())
    ~add_update_hook:WP_Prover.add_prover_update_hook
    ~add_reload_hook:WP_Prover.add_reload_hook
    model

(* -------------------------------------------------------------------------- *)
(* --- Server Processes                                                   --- *)
(* -------------------------------------------------------------------------- *)

let _ =
  S.register_state ~package
    ~name:"process"
    ~descr:(Md.plain "Server Processes")
    ~data:(module D.Jint)
    ~get:Wp_parameters.Procs.get
    ~set:(fun procs ->
        Wp_parameters.Procs.set procs ;
        ignore @@ ProverTask.server ~procs ())
    ~add_hook:Wp_parameters.Procs.add_hook_on_update ()

(* -------------------------------------------------------------------------- *)
(* --- Provers Timeout                                                    --- *)
(* -------------------------------------------------------------------------- *)

let _ =
  S.register_state ~package
    ~name:"timeout"
    ~descr:(Md.plain "Prover's Timeout")
    ~data:(module D.Jint)
    ~get:Wp_parameters.Timeout.get
    ~set:Wp_parameters.Timeout.set
    ~add_hook:Wp_parameters.Timeout.add_hook_on_update ()

(* -------------------------------------------------------------------------- *)
(* --- Cache mode                                                         --- *)
(* -------------------------------------------------------------------------- *)

module CacheMode =
struct
  include D.Enum

  let dictionary : Cache.mode dictionary = dictionary ()

  let tag name value = tag ~name ~descr:(Md.plain name) ~value dictionary

  let none = tag "None" NoCache
  let update = tag "Update" Update
  let replay = tag "Replay" Replay
  let rebuild = tag "Rebuild" Rebuild
  let offline = tag "Offline" Offline
  let cleanup = tag "Cleanup" Cleanup

  let lookup = function
    | Cache.NoCache -> none
    | Update -> update
    | Replay -> replay
    | Rebuild -> rebuild
    | Offline -> offline
    | Cleanup-> cleanup

  let () =
    set_lookup dictionary lookup

  include
    (val publish
        ~package
        ~descr:(Md.plain "Cache mode")
        ~name:"CacheMode"
        dictionary)
end

let _ =
  S.register_state ~package
    ~name:"cacheMode"
    ~descr:(Md.plain "Current Cache mode")
    ~data:(module CacheMode)
    ~get:Cache.get_mode
    ~set:Cache.set_mode
    ~add_hook:Cache.add_hook_on_mode_update
    ()

(* -------------------------------------------------------------------------- *)
(* --- Interactive provers                                                --- *)
(* -------------------------------------------------------------------------- *)

let _ =
  R.register
    ~package ~kind:`GET ~name:"isInteractiveProver"
    ~descr:(Md.plain "Tells whether the prover is interactive")
    ~input:(module Prover)
    ~output:(module D.Jbool)
    (fun p -> not @@ WP_Prover.is_auto p)

module InteractiveMode =
struct
  include D.Enum

  let dictionary : WP_Prover.InteractiveMode.t dictionary = dictionary ()

  let tag name value = tag ~name ~descr:(Md.plain name) ~value dictionary

  let batch  = tag "Batch"     WP_Prover.InteractiveMode.Batch
  let update = tag "Update"    WP_Prover.InteractiveMode.Update
  let edit   = tag "Edit"      WP_Prover.InteractiveMode.Edit
  let fix    = tag "Fix"       WP_Prover.InteractiveMode.Fix
  let fixup  = tag "FixUpdate" WP_Prover.InteractiveMode.FixUpdate

  let lookup = function
    | WP_Prover.InteractiveMode.Batch -> batch
    | Update -> update
    | Edit -> edit
    | Fix -> fix
    | FixUpdate -> fixup

  let () =
    set_lookup dictionary lookup

  include
    (val publish
        ~package
        ~descr:(Md.plain "interactive mode")
        ~name:"InteractiveMode"
        dictionary)
end

let _ =
  S.register_state ~package
    ~name:"interactiveMode"
    ~descr:(Md.plain "Current interactive mode")
    ~data:(module InteractiveMode)
    ~get:WP_Prover.InteractiveMode.get
    ~set:WP_Prover.InteractiveMode.set
    ~add_hook:WP_Prover.InteractiveMode.add_hook_on_update
    ()

(* -------------------------------------------------------------------------- *)
(* --- Proof Strategies                                                   --- *)
(* -------------------------------------------------------------------------- *)

module TipMode =
struct
  include D.Enum

  let dictionary : WP_Prover.TipMode.t dictionary = dictionary ()

  let tag name value = tag ~name ~descr:(Md.plain name) ~value dictionary

  let batch  = tag "Batch"  WP_Prover.TipMode.Batch
  let update = tag "Update" WP_Prover.TipMode.Update
  let dry    = tag "Dry"    WP_Prover.TipMode.Dry
  let init   = tag "Init"   WP_Prover.TipMode.Init

  let lookup = function
    | WP_Prover.TipMode.Batch -> batch
    | Update -> update
    | Dry -> dry
    | Init -> init

  let () =
    set_lookup dictionary lookup

  include
    (val publish
        ~package
        ~descr:(Md.plain "TIP mode")
        ~name:"TipMode"
        dictionary)
end

let _ =
  S.register_state ~package
    ~name:"tipMode"
    ~descr:(Md.plain "Current Strategy Mode")
    ~data:(module TipMode)
    ~get:WP_Prover.TipMode.get
    ~set:WP_Prover.TipMode.set
    ~add_hook:WP_Prover.TipMode.add_hook_on_update
    ()

let _ =
  S.register_state ~package
    ~name:"scripts"
    ~descr:(Md.plain "Whether scripts are enabled")
    ~data:(module D.Jbool)
    ~get:WP_Prover.use_scripts
    ~set:WP_Prover.set_use_scripts
    ~add_hook:WP_Prover.add_scripts_update_hook
    ()

let _ =
  S.register_state ~package
    ~name:"strategies"
    ~descr:(Md.plain "Whether strategies are enabled")
    ~data:(module D.Jbool)
    ~get:WP_Prover.use_strategies
    ~set:WP_Prover.set_use_strategies
    ~add_hook:WP_Prover.add_scripts_update_hook
    ()

(* -------------------------------------------------------------------------- *)
(* --- Counter Examples                                                   --- *)
(* -------------------------------------------------------------------------- *)

let _ =
  S.register_state ~package
    ~name:"counterExamples"
    ~descr:(Md.plain "Enabled Counter Examples")
    ~data:(module D.Jbool)
    ~get:Wp_parameters.CounterExamples.get
    ~set:Wp_parameters.CounterExamples.set
    ~add_hook:Wp_parameters.CounterExamples.add_hook_on_update ()

(* -------------------------------------------------------------------------- *)
(* --- Results and Stats                                                  --- *)
(* -------------------------------------------------------------------------- *)

module Result =
struct
  type t = VCS.result
  let jtype = D.declare ~package ~name:"result"
      ~descr:(Md.plain "Prover Result")
      (Jrecord [
          "descr", Jstring ;
          "cached", Jboolean ;
          "verdict", Jstring ;
          "solverTime", Jnumber ;
          "proverTime", Jnumber ;
          "proverSteps", Jnumber ;
        ])
  let of_json _ = failwith "Not implemented"
  let to_json (r : VCS.result) = `Assoc [
      "descr", `String (Pretty_utils.to_string VCS.pp_result r) ;
      "cached", `Bool r.cached ;
      "verdict", `String (VCS.name_of_verdict ~computing:true r.verdict) ;
      "solverTime", `Float r.solver_time ;
      "proverTime", `Float r.prover_time ;
      "proverSteps", `Int r.prover_steps ;
    ]
end

module STATUS =
struct
  type t = { smoke : bool ; verdict : VCS.verdict }
  let jtype = D.declare ~package ~name:"status"
      ~descr:(Md.plain "Test Status")
      (Junion [
          Jkey "NORESULT" ;
          Jkey "COMPUTING" ;
          Jkey "FAILED" ;
          Jkey "STEPOUT" ;
          Jkey "UNKNOWN" ;
          Jkey "VALID" ;
          Jkey "PASSED" ;
          Jkey "DOOMED" ;
        ])
  let to_json { smoke ; verdict } =
    `String begin
      match verdict with
      | Valid -> if smoke then "DOOMED" else "VALID"
      | Invalid -> if smoke then "PASSED" else "INVALID"
      | Unknown -> if smoke then "PASSED" else "UNKNOWN"
      | Timeout -> if smoke then "PASSED" else "TIMEOUT"
      | Stepout -> if smoke then "PASSED" else "STEPOUT"
      | Failed -> "FAILED"
      | NoResult -> "NORESULT"
      | Computing _ -> "COMPUTING"
    end
end

module STATS =
struct
  type t = Stats.stats
  let jtype = D.declare ~package ~name:"stats"
      ~descr:(Md.plain "Prover Result")
      (Jrecord [
          "summary", Jstring;
          "tactics", Jnumber;
          "proved", Jnumber;
          "total", Jnumber;
        ])
  let to_json cs : Json.t =
    let cache = Cache.get_mode () in
    let summary = Pretty_utils.to_string
        (Stats.pp_stats ~shell:false ~cache) cs
    in `Assoc [
      "summary", `String summary ;
      "tactics", `Int cs.tactics ;
      "proved", `Int cs.proved ;
      "total", `Int (Stats.subgoals cs) ;
    ]
end

(* -------------------------------------------------------------------------- *)
(* --- Goal Array                                                         --- *)
(* -------------------------------------------------------------------------- *)

let gmodel : Wpo.t S.model = S.model ()

let get_property g = Printer_tag.PIP (WpPropId.property_of_id g.Wpo.po_pid)

let get_marker g =
  match g.Wpo.po_formula.source with
  | Some(stmt,_) -> Printer_tag.localizable_of_stmt stmt
  | None ->
    let ip = WpPropId.property_of_id g.Wpo.po_pid in
    match ip with
    | IPOther { io_loc = OLStmt(_,stmt) } ->
      Printer_tag.localizable_of_stmt stmt
    | _ -> Printer_tag.PIP ip

let get_decl g = match g.Wpo.po_idx with
  | Function(kf,_) -> Some (Printer_tag.SFunction kf)
  | Axiomatic _ -> None (* TODO *)

let get_fct g = match g.Wpo.po_idx with
  | Function(kf,_) -> Some (Kernel_function.get_name kf)
  | Axiomatic _ -> None

let get_bhv g = match g.Wpo.po_idx with
  | Function(_,bhv) -> bhv
  | Axiomatic _ -> None

let get_thy g = match g.Wpo.po_idx with
  | Function _ -> None
  | Axiomatic ax -> ax

let get_status g =
  STATUS.{
    smoke = Wpo.is_smoke_test g ;
    verdict = (ProofEngine.consolidated g).best ;
  }

let get_ast_dependencies g =
  let open Wpo in
  let module Stmts = Cil_datatype.Stmt.Set in
  let module Props = Property.Set in
  let add_stmt s l = Printer_tag.localizable_of_stmt s :: l in
  let add_prop p l = Printer_tag.PIP p :: l in
  Stmts.fold add_stmt g.po_formula.path @@
  Props.fold add_prop g.po_formula.deps []

let () = S.column gmodel ~name:"marker"
    ~descr:(Md.plain "Associated Marker")
    ~data:(module AST.Marker) ~get:get_marker

let () = S.column gmodel ~name:"scope"
    ~descr:(Md.plain "Associated declaration, if any")
    ~data:(module D.Joption(AST.Decl)) ~get:get_decl

let () = S.column gmodel ~name:"property"
    ~descr:(Md.plain "Property Marker")
    ~data:(module AST.Marker) ~get:get_property

let () = S.option gmodel ~name:"fct"
    ~descr:(Md.plain "Associated function name, if any")
    ~data:(module D.Jstring) ~get:get_fct

let () = S.option gmodel ~name:"bhv"
    ~descr:(Md.plain "Associated behavior name, if any")
    ~data:(module D.Jstring) ~get:get_bhv

let () = S.option gmodel ~name:"thy"
    ~descr:(Md.plain "Associated axiomatic name, if any")
    ~data:(module D.Jstring) ~get:get_thy

let () = S.column gmodel ~name:"name"
    ~descr:(Md.plain "Informal Property Name")
    ~data:(module D.Jstring)
    ~get:(fun g -> g.Wpo.po_name)

let () = S.column gmodel ~name:"smoke"
    ~descr:(Md.plain "Smoking (or not) goal")
    ~data:(module D.Jbool) ~get:Wpo.is_smoke_test

let () = S.column gmodel ~name:"passed"
    ~descr:(Md.plain "Valid or Passed goal")
    ~data:(module D.Jbool) ~get:Wpo.is_passed

let () = S.column gmodel ~name:"status"
    ~descr:(Md.plain "Verdict, Status")
    ~data:(module STATUS) ~get:get_status

let () = S.column gmodel ~name:"stats"
    ~descr:(Md.plain "Prover Stats Summary")
    ~data:(module STATS) ~get:ProofEngine.consolidated

let () = S.column gmodel ~name:"proof"
    ~descr:(Md.plain "Proof Tree")
    ~data:(module D.Jbool)
    ~get:ProofEngine.has_proof

let () = S.option gmodel ~name:"script"
    ~descr:(Md.plain "Script File")
    ~data:(module D.Jstring)
    ~get:(fun wpo ->
        match ProofSession.get wpo with
        | NoScript -> None
        | Script a | Deprecated a -> Some (Filepath.to_string_abs a))

let () = S.column gmodel ~name:"saved"
    ~descr:(Md.plain "Saved Script")
    ~data:(module D.Jbool)
    ~get:(fun wpo -> ProofEngine.get wpo = `Saved)

let () = S.column gmodel ~name:"deps"
    ~descr:(Md.plain "Dependencies")
    ~data:(module D.Jlist(AST.Marker))
    ~get:get_ast_dependencies

let filter hook fn = hook (fun g -> if not @@ Wpo.is_tactic g then fn g)
let (++) h1 h2 fn = h1 fn ; h2 fn

let goals =
  let add_remove_hook =
    filter Wpo.add_removed_hook in
  let add_update_hook =
    filter Wpo.add_modified_hook ++ ProofEngine.add_goal_hook in
  let add_reload_hook = Wpo.add_cleared_hook in
  S.register_array ~package ~name:"goals"
    ~descr:(Md.plain "Generated Goals")
    ~key:indexGoal
    ~keyName:"wpo"
    ~keyType:Goal.jtype
    ~iter:(filter Wpo.iter_on_goals)
    ~preload:ProofEngine.consolidate
    ~add_remove_hook
    ~add_update_hook
    ~add_reload_hook
    gmodel

let () =
  R.register ~package ~kind:`GET ~name:"getGoalsFromASTMarker"
    ~descr:(Md.plain "Get goals from AST marker")
    ~input:(module AST.Marker)
    ~output:(module D.Jlist(Goal))
    begin fun marker ->
      let open Printer_tag in
      let has_marker g =
        let is_marker = Localizable.equal marker in
        let in_stmt = match g.Wpo.po_formula.source with
          | Some(stmt,_) -> is_marker @@ localizable_of_stmt stmt
          | None -> false
        in
        in_stmt ||
        match WpPropId.property_of_id g.Wpo.po_pid with
        | IPOther {io_loc = OLStmt(_,s)} -> is_marker @@ localizable_of_stmt s
        | ip -> is_marker @@ Printer_tag.PIP ip
      in
      let select g = has_marker g && not @@ Wpo.is_tactic g in
      let l = ref [] in
      Wpo.iter_on_goals(fun g -> if select g then l := g :: !l) ;
      List.sort Wpo.S.compare !l
    end

(* -------------------------------------------------------------------------- *)
(* --- Generate RTEs                                                      --- *)
(* -------------------------------------------------------------------------- *)

let () =
  R.register ~package ~kind:`EXEC ~name:"generateRTEGuards"
    ~descr:(Md.plain "Generate RTE guards for the function")
    ~input:(module AST.Marker)
    ~output:(module D.Junit)
    begin function
      | PVDecl (Some kf, _, _) ->
        let setup = Factory.parse (Wp_parameters.Model.get ()) in
        let driver = Driver.load_driver () in
        let model = Factory.instance setup driver in
        WpRTE.generate model kf
      | _ -> ()
    end

(* -------------------------------------------------------------------------- *)
(* --- Special case of initialization                                     --- *)
(* -------------------------------------------------------------------------- *)
(* NB: this should be factorized between Eva, RTE, Kernel *)

module Initialized_proxy = struct

  type t =
    | Only of Kernel_function.Set.t
    | Except of Kernel_function.Set.t

  type elem = All | Kf of Cil_types.kernel_function
  type init = Add of elem | Remove of elem

  let action set elem =
    match elem, set with
    | Add All, _ -> Except Kernel_function.Set.empty
    | Remove All, _ -> Only Kernel_function.Set.empty
    | Add (Kf kf), Except set ->
      Except (Kernel_function.Set.remove kf set)
    | Add (Kf kf), Only set ->
      Only (Kernel_function.Set.add kf set)
    | Remove (Kf kf), Except set ->
      Except (Kernel_function.Set.add kf set)
    | Remove (Kf kf), Only set ->
      Only (Kernel_function.Set.remove kf set)

  let parse name =
    let add e = Add e and rem e = Remove e in
    if String.equal name "@default"
    || String.equal name "+@default"
    || String.equal name "-@default"
    then None (* adds or removes nothing *)
    else if String.equal name "@all"
         || String.equal name "+@all"
    then Some (Add All)
    else if String.equal name "-@all"
    then Some (Remove All)
    else if String.starts_with ~prefix:"-" name
         || String.starts_with ~prefix:"+" name
    then
      let op = if String.get name 0 = '+' then add else rem in
      let name = String.sub name 1 ((String.length name) -1 ) in
      Some (op (Kf (Globals.Functions.find_by_name name)))
    else
      Some (Add (Kf (Globals.Functions.find_by_name name)))

  let parse_action l s =
    match parse s with
    | None -> l
    | Some value -> action l value

  let pp_actions fmt actions =
    let only, elements = match actions with
      | Only set -> true, Kernel_function.Set.elements set
      | Except set -> false, Kernel_function.Set.elements set
    in
    let pp fmt kf =
      if only
      then Kernel_function.pretty fmt kf
      else Format.fprintf fmt "-%a" Kernel_function.pretty kf
    in
    Format.fprintf fmt "%s%a"
      (if only then ""
       else if elements = [] then "@all"
       else "@all,")
      (Pretty_utils.pp_list ~sep:"," pp) elements

  let current_init_proxy =
    ref (Only Kernel_function.Set.empty)

  let hooks = ref []
  let add_hook_on_update hook =  hooks := hook :: !hooks

  let set_init_proxy value =
    current_init_proxy := value ;
    List.iter (fun hook -> hook ()) !hooks

  let update_init_proxy () =
    (* We force the kernel to compute the value so that we are sure that the
       internal string contains something that is meaningful for a kernel
       function set.
    *)
    ignore (RteGen.Options.DoInitialized.get ());
    (* Now the nice thing is that we are sure that list contains only @all,
       @default or function names (potentially prefixed with - or +), so we can
       trim spaces and split according to ','. *)
    let line = RteGen.Options.DoInitialized.As_string.get () in
    let entries = List.map String.trim @@ String.split_on_char ',' line in
    let actions =
      List.fold_left parse_action (Only Kernel_function.Set.empty) entries in
    set_init_proxy actions

  (* Note that, since we do not update the actual option with the preprocessed
     list of actions, the proxy is not *exactly* the same as the content of the
     parameter. But, it has the same meaning.
  *)
  let () =
    RteGen.Options.DoInitialized.add_set_hook
      (fun _ _ -> update_init_proxy ())

  let set actions =
    RteGen.Options.DoInitialized.As_string.set
      (Format.asprintf "%a" pp_actions actions) ;
    set_init_proxy actions

  let get () =
    !current_init_proxy
end

module JInitialized_proxy =
struct
  module Decl_list = D.Jlist(D.Jpair(AST.Decl)(D.Jstring))

  type t = Initialized_proxy.t

  let jtype =
    D.declare ~package ~name:"initializedProxy" @@
    Jrecord [ "only", Jboolean ; "elems", Decl_list.jtype]

  let to_json s =
    let only, set = match s with
      | Initialized_proxy.Only set -> true, set
      | Initialized_proxy.Except set -> false, set
    in
    let elems =
      List.map
        (fun kf -> Printer_tag.SFunction kf, Kernel_function.get_name kf)
        (Kernel_function.Set.elements set)
    in
    `Assoc [
      "only", `Bool only ;
      "elems", Decl_list.to_json elems
    ]

  let of_json json =
    let extract_function = function
      | Printer_tag.SFunction kf, _ -> kf
      | _ -> raise Not_found
    in
    try
      match Json.assoc json with
      | [ (_, only) ; (_, elems) ] ->
        let only = Json.bool only in
        let elems = Decl_list.of_json elems in
        let kfs = List.map extract_function elems in
        let kfs = Kernel_function.Set.of_list kfs in
        if only then Initialized_proxy.Only kfs else Except kfs
      | _ -> raise Not_found
    with _ ->
      Wp_parameters.fatal "Cannot parse: %a" Json.pp json
end

let () =
  ignore @@ S.register_state ~package ~name:"initialized"
    ~descr:(Md.plain "Configured properties filter")
    ~data:(module JInitialized_proxy)
    ~get:Initialized_proxy.get
    ~set:Initialized_proxy.set
    ~add_hook:Initialized_proxy.add_hook_on_update
    ()

(* -------------------------------------------------------------------------- *)
(* --- Properties filter                                                  --- *)
(* -------------------------------------------------------------------------- *)

let () =
  ignore @@ S.register_state ~package ~name:"filter"
    ~descr:(Md.plain "Configured properties filter")
    ~data:(module D.Jlist(D.Jstring))
    ~get:Wp_parameters.Properties.get
    ~set:Wp_parameters.Properties.set
    ~add_hook:(fun f -> Wp_parameters.Properties.add_set_hook (fun _ -> f))
    ()

(* -------------------------------------------------------------------------- *)
(* --- Generate goals                                                     --- *)
(* -------------------------------------------------------------------------- *)

let is_call stmt =
  match stmt.Cil_types.skind with
  | Instr (Call _) | Instr (Local_init (_, ConsInit _, _)) -> true
  | _ -> false

let start_proofs_marker = function
  | Printer_tag.PExp _  | PTermLval _ | PLval _
  | PGlobal _ | PType _ | PVDecl (None, _, _) ->
    (* We cannot run anything here *) ()
  | PStmtStart (_, stmt) | PStmt (_, stmt) when is_call stmt ->
    VC.command @@ VC.generate_call stmt
  | PStmtStart (kf, stmt) | PStmt (kf, stmt) ->
    let fold_ips _ ca bag =
      let ids = WpPropId.mk_code_annot_ids kf stmt ca in
      let props = Bag.ulist @@
        List.map VC.generate_ip @@
        List.map WpPropId.property_of_id ids
      in
      Bag.concat bag props
    in
    VC.command @@ Annotations.fold_code_annot fold_ips stmt Bag.empty
  | PVDecl (Some kf, _, _) ->
    VC.command @@ VC.generate_kf kf
  | PIP property ->
    VC.command @@ VC.generate_ip property

let () =
  R.register ~package ~kind:`EXEC ~name:"startProofs"
    ~descr:(Md.plain "Generate goals and run provers")
    ~input:(module D.Joption(AST.Marker))
    ~output:(module D.Junit)
    begin function
      | None -> VC.command @@ VC.generate_all ()
      | Some marker -> start_proofs_marker marker
    end

(* -------------------------------------------------------------------------- *)
(* --- Clear goals                                                        --- *)
(* -------------------------------------------------------------------------- *)

let () =
  R.register ~package ~kind:`EXEC ~name:"clearProofs"
    ~descr:(Md.plain "Clear goals")
    ~input:(module D.Junit)
    ~output:(module D.Junit)
    begin fun () ->
      Emitter.clear WpReached.emitter ;
      CfgInfos.clear ();
      Wpo.iter_on_goals
        (fun g ->
           let emitter = WpContext.get_emitter g.po_model in
           Emitter.clear emitter) ;
      Wpo.iter_on_goals Wpo.clear_results ;
      Wpo.clear ();
    end

(* -------------------------------------------------------------------------- *)
(* --- Proof Server                                                       --- *)
(* -------------------------------------------------------------------------- *)

let serverActivity = R.signal ~package
    ~name:"serverActivity"
    ~descr:(Md.plain "Proof Server Activity")

let () =
  let server_sig = R.signature ~input:(module D.Junit) () in
  let set_procs = R.result server_sig
      ~name:"procs" ~descr:(Md.plain "Max parallel tasks") (module D.Jint) in
  let set_active = R.result server_sig
      ~name:"active" ~descr:(Md.plain "Active tasks") (module D.Jint) in
  let set_done = R.result server_sig
      ~name:"done" ~descr:(Md.plain "Finished tasks") (module D.Jint) in
  let set_todo = R.result server_sig
      ~name:"todo" ~descr:(Md.plain "Remaining jobs") (module D.Jint) in
  R.register_sig ~package ~kind:`GET ~name:"getScheduledTasks"
    ~descr:(Md.plain "Scheduled tasks in proof server")
    ~signals:[serverActivity]
    server_sig
    begin
      let monitored = ref false in
      fun rq () ->
        let server = ProverTask.server () in
        if not !monitored then
          begin
            monitored := true ;
            let signal () = R.emit serverActivity in
            Task.on_server_activity server signal ;
            Task.on_server_start server signal ;
            Task.on_server_stop server signal ;
          end ;
        set_procs rq (Task.get_procs server) ;
        set_active rq (Task.running server) ;
        set_done rq (Task.terminated server) ;
        set_todo rq (Task.remaining server) ;
    end

let () = R.register ~package ~kind:`SET ~name:"cancelProofTasks"
    ~descr:(Md.plain "Cancel all scheduled proof tasks")
    ~input:(module D.Junit) ~output:(module D.Junit)
    (fun () -> let server = ProverTask.server () in Task.cancel_all server)

(* -------------------------------------------------------------------------- *)
