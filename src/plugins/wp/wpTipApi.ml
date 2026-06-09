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

module W = WpApi
module P = Server.Package
module D = Server.Data
module R = Server.Request
module S = Server.States
module Md = Markdown
module AST = Server.Kernel_ast

let package = P.package ~plugin:"wp" ~name:"tip"
    ~title:"WP Interactive Prover" ()

(* -------------------------------------------------------------------------- *)
(* --- Signals                                                            --- *)
(* -------------------------------------------------------------------------- *)

let proofStatus = R.signal ~package ~name:"proofStatus"
    ~descr:(Md.plain "Proof Status has changed")

let printStatus = R.signal ~package ~name:"printStatus"
    ~descr:(Md.plain "Updated TIP printer")

(* -------------------------------------------------------------------------- *)
(* --- Proof Node                                                         --- *)
(* -------------------------------------------------------------------------- *)

module Node = D.Index
    (Map.Make(ProofEngine.Node))
    (struct
      let package = package
      let name = "node"
      let descr = Md.plain "Proof Node index"
    end)

let () = Server.Main.once
    begin fun () ->
      ProofEngine.add_remove_hook Node.remove ;
      ProofEngine.add_clear_hook (fun _ -> Node.clear ()) ;
      let signal _ = R.emit proofStatus in
      ProofEngine.add_update_hook signal ;
      ProofEngine.add_goal_hook signal ;
      Wpo.add_modified_hook signal ;
    end

module Tactic : D.S with type t = Tactical.t =
struct
  type t = Tactical.t
  let jtype = D.declare ~package ~name:"tactic"
      ~descr:(Md.plain "Tactic identifier") @@ P.Jkey "tactic"
  let to_json (t : Tactical.t) = `String t#id
  let of_json (js : Json.t) = Tactical.lookup ~id:(js |> Json.string)
end

module Path = D.Jlist(Node)

let () =
  let inode = R.signature ~input:(module Node) () in
  let set_title = R.result inode ~name:"title"
      ~descr:(Md.plain "Proof node title") (module D.Jstring) in
  let set_proved = R.result inode ~name:"proved"
      ~descr:(Md.plain "Proof node complete") (module D.Jbool) in
  let set_pending = R.result inode ~name:"pending"
      ~descr:(Md.plain "Pending children") (module D.Jint) in
  let set_size = R.result inode ~name:"size"
      ~descr:(Md.plain "Proof size") (module D.Jint) in
  let set_stats = R.result inode ~name:"stats"
      ~descr:(Md.plain "Node statistics") (module D.Jstring) in
  let set_results = R.result inode ~name:"results"
      ~descr:(Md.plain "Prover results for current node")
      (module D.Jlist(D.Jpair(W.Prover)(W.Result))) in
  let set_tactic = R.result_opt inode ~name:"tactic"
      ~descr:(Md.plain "Applied tactic (if any)")
      (module Tactic) in
  let set_header = R.result_opt inode ~name:"header"
      ~descr:(Md.plain "Proof node tactic label (if any)")
      (module D.Jstring) in
  let set_child_label = R.result_opt inode ~name:"childLabel"
      ~descr:(Md.plain "Proof node child label (from parent, if any)")
      (module D.Jstring) in
  let set_path = R.result inode ~name:"path"
      ~descr:(Md.plain "Proof node path from goal")
      (module Path) in
  let set_children = R.result inode ~name:"children"
      ~descr:(Md.plain "Proof node tactic children (id any)")
      (module Path) in
  R.register_sig inode ~package ~kind:`GET ~name:"getNodeInfos"
    ~descr:(Md.plain "Proof node information")
    ~signals:[proofStatus]
    begin fun rq node ->
      set_title rq (ProofEngine.title node);
      set_proved rq (ProofEngine.fully_proved node);
      set_pending rq (ProofEngine.pending node);
      let s = ProofEngine.stats node in
      set_size rq (Stats.subgoals s);
      set_stats rq (Pretty_utils.to_string Stats.pretty s);
      set_results rq (Wpo.get_results @@ ProofEngine.goal node);
      set_tactic rq (ProofEngine.tactic node);
      set_header rq (ProofEngine.tactic_label node);
      set_child_label rq (ProofEngine.child_label node);
      set_path rq (ProofEngine.path node);
      set_children rq (ProofEngine.subgoals node);
    end

let () =
  let iresult = R.signature ~output:(module W.Result) () in
  let get_node = R.param iresult ~name:"node"
      ~descr:(Md.plain "Proof node") (module Node) in
  let get_prover = R.param iresult ~name:"prover"
      ~descr:(Md.plain "Prover") (module W.Prover) in
  R.register_sig iresult ~package ~kind:`GET ~name:"getResult"
    ~descr:(Md.plain "Result for specified node and prover")
    ~signals:[proofStatus]
    begin fun rq () ->
      let node = get_node rq in
      let prover = get_prover rq in
      Wpo.get_result (ProofEngine.goal node) prover
    end

(* -------------------------------------------------------------------------- *)
(* --- Proof Status                                                     --- *)
(* -------------------------------------------------------------------------- *)

let () =
  let status = R.signature () in
  let get_main = R.param status ~name:"main"
      ~descr:(Md.plain "Proof Obligation") (module W.Goal) in
  let get_unproved = R.param status ~name:"unproved" ~default:false
      ~descr:(Md.plain "Report unproved children only")
      (module D.Jbool) in
  let get_subtree = R.param status ~name:"subtree" ~default:false
      ~descr:(Md.plain "Report subtree children only")
      (module D.Jbool) in
  let set_size = R.result status ~name:"size"
      ~descr:(Md.plain "Proof size") (module D.Jint) in
  let set_index = R.result status ~name:"index"
      ~descr:(Md.plain "Current node index among pending nodes (else -1)")
      (module D.Jint) in
  let set_pending = R.result status ~name:"pending"
      ~descr:(Md.plain "Pending proof nodes") (module D.Jint) in
  let set_current = R.result status ~name:"current"
      ~descr:(Md.plain "Current proof node") (module Node) in
  let set_parents = R.result status ~name:"parents"
      ~descr:(Md.plain "parents nodes")
      (module Path) in
  let set_tactic = R.result_opt status ~name:"tactic"
      ~descr:(Md.plain "Applied tactic (if any)")
      (module Tactic) in
  let set_children = R.result status ~name:"children"
      ~descr:(Md.plain "Children nodes")
      (module Path) in
  R.register_sig ~package status
    ~kind:`GET ~name:"getProofStatus"
    ~descr:(Md.plain "Current Proof Status of a Goal")
    ~signals:[proofStatus]
    begin fun rq () ->
      let tree = ProofEngine.proof ~main:(get_main rq) in
      let root = ProofEngine.root tree in
      set_pending rq (ProofEngine.pending root) ;
      set_size rq (Stats.subgoals @@ ProofEngine.stats root);
      let current, index =
        match ProofEngine.current tree with
        | `Main -> root, -1
        | `Internal node -> node, -1
        | `Leaf(idx,node) -> node, idx
      in
      let subgoals = ProofEngine.subgoals current in
      set_index rq index ;
      set_current rq current ;
      set_parents rq @@ ProofEngine.path current ;
      set_tactic rq @@ ProofEngine.tactic current ;
      set_children rq @@
      match get_unproved rq, get_subtree rq with
      | false, false -> subgoals
      | false, true ->
        List.filter (fun n -> ProofEngine.subgoals n <> []) subgoals
      | true, false ->
        List.filter (fun n -> ProofEngine.pending n > 0) subgoals
      | true, true ->
        List.filter (fun n ->
            ProofEngine.pending n > 0 ||
            ProofEngine.subgoals n <> []
          ) subgoals
    end

(* -------------------------------------------------------------------------- *)
(* --- Proof Tree Management                                              --- *)
(* -------------------------------------------------------------------------- *)

let () = R.register ~package ~kind:`SET ~name:"goForward"
    ~descr:(Md.plain "Go to to first pending node, or root if none")
    ~input:(module W.Goal) ~output:(module D.Junit)
    begin fun goal ->
      let tree = ProofEngine.proof ~main:goal in
      ProofEngine.forward tree ;
    end

let () = R.register ~package ~kind:`SET ~name:"goToRoot"
    ~descr:(Md.plain "Go to root of proof tree")
    ~input:(module W.Goal) ~output:(module D.Junit)
    begin fun goal ->
      let tree = ProofEngine.proof ~main:goal in
      ProofEngine.goto tree `Main ;
    end

let () = R.register ~package ~kind:`SET ~name:"goToIndex"
    ~descr:(Md.plain "Go to k-th pending node of proof tree")
    ~input:(module D.Jpair(W.Goal)(D.Jint)) ~output:(module D.Junit)
    begin fun (goal,index) ->
      let tree = ProofEngine.proof ~main:goal in
      ProofEngine.goto tree (`Leaf index) ;
    end

let () = R.register ~package ~kind:`SET ~name:"goToNode"
    ~descr:(Md.plain "Set current node of associated proof tree")
    ~input:(module Node) ~output:(module D.Junit)
    begin fun node ->
      let tree = ProofEngine.tree node in
      ProofEngine.goto tree (`Node node) ;
    end

let () = R.register ~package ~kind:`SET ~name:"clearNode"
    ~descr:(Md.plain "Cancel all node results and sub-tree (if any)")
    ~input:(module Node) ~output:(module D.Junit)
    begin fun node ->
      let tree = ProofEngine.tree node in
      ProofEngine.clear_node tree node ;
    end

let () = R.register ~package ~kind:`SET ~name:"clearNodeTactic"
    ~descr:(Md.plain "Cancel node current tactic")
    ~input:(module Node) ~output:(module D.Junit)
    begin fun node ->
      let tree = ProofEngine.tree node in
      ProofEngine.clear_node_tactic tree node ;
    end

let () = R.register ~package ~kind:`SET ~name:"clearParentTactic"
    ~descr:(Md.plain "Cancel parent node tactic")
    ~input:(module Node) ~output:(module D.Junit)
    begin fun node ->
      let tree = ProofEngine.tree node in
      ProofEngine.clear_parent_tactic tree node ;
    end

let () = R.register ~package ~kind:`SET ~name:"clearGoal"
    ~descr:(Md.plain "Remove the complete goal proof tree")
    ~input:(module W.Goal) ~output:(module D.Junit)
    begin fun goal ->
      let tree = ProofEngine.proof ~main:goal in
      ProofEngine.clear_tree tree ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Sequent Indexers                                                   --- *)
(* -------------------------------------------------------------------------- *)

module Part = D.Tagged
    (struct
      type t = [ `Term | `Goal | `Step of int ]
      let id = function
        | `Term -> "#term"
        | `Goal -> "#goal"
        | `Step k -> Printf.sprintf "#s%d" k
    end)
    (struct
      let package = package
      let name = "part"
      let descr = Md.plain "Proof part marker"
    end)

module Term = D.Tagged
    (struct
      type t = Lang.F.term
      let id t = Printf.sprintf "#e%d" (Lang.F.QED.id t)
    end)
    (struct
      let package = package
      let name = "term"
      let descr = Md.plain "Term marker"
    end)

let of_part = function
  | Ptip.Term -> `Term
  | Ptip.Goal -> `Goal
  | Ptip.Step s -> `Step s.id

let to_part sequent = function
  | `Term -> Ptip.Term
  | `Goal -> Ptip.Goal
  | `Step k ->
    try Ptip.Step (Conditions.step_at sequent k) with Not_found -> Ptip.Term

(* -------------------------------------------------------------------------- *)
(* --- Sequent Printer                                                    --- *)
(* -------------------------------------------------------------------------- *)

let wrap tag pp fmt x =
  begin
    Format.pp_open_stag fmt (Format.String_tag tag) ;
    pp fmt x ;
    Format.pp_close_stag fmt () ;
  end

class printer () : Ptip.pseq =
  let terms : Ptip.term_wrapper =
    object
      method wrap pp fmt t = wrap (Term.get t) pp fmt t
    end in
  let focus : Ptip.term_wrapper =
    object
      method wrap pp fmt t = wrap "wp:focus" (terms#wrap pp) fmt t
    end in
  let target : Ptip.term_wrapper =
    object
      method wrap pp fmt t = wrap "wp:target" (terms#wrap pp) fmt t
    end in
  let parts : Ptip.part_marker =
    object
      method wrap pp fmt p = wrap (Part.get @@ of_part p) pp fmt p
      method mark : 'a. Ptip.part -> 'a Ptip.printer -> 'a Ptip.printer
        = fun p pp fmt x -> wrap (Part.get @@ of_part p) pp fmt x
    end in
  let target_part : Ptip.part_marker =
    object
      method wrap pp fmt p = wrap "wp:target" (parts#wrap pp) fmt p
      method mark : 'a. Ptip.part -> 'a Ptip.printer -> 'a Ptip.printer
        = fun p pp fmt x -> wrap "wp:target" (parts#mark p pp) fmt x
    end in
  let autofocus = new Ptip.autofocus in
  let plang = new Ptip.plang ~terms ~focus ~target ~autofocus in
  let pcond = new Ptip.pcond ~parts ~target:target_part ~autofocus ~plang in
  Ptip.pseq ~autofocus ~plang ~pcond

(* -------------------------------------------------------------------------- *)
(* --- Printer Registry                                                   --- *)
(* -------------------------------------------------------------------------- *)

module PRINTER = State_builder.Ref
    (Datatype.Make
       (struct
         include Datatype.Undefined
         type t = (string,printer) Hashtbl.t
         let name = "WpTipApi.PRINTER.Datatype"
         let reprs = [ Hashtbl.create 0 ]
         let mem_project = Datatype.never_any_project
       end))
    (struct
      let name = "WpApi.PRINTER"
      let dependencies = [ Ast.self ]
      let default () = Hashtbl.create 0
    end)

let () = Wpo.add_modified_hook
    (fun wpo ->
       let registry = PRINTER.get () in
       let printer = Hashtbl.find_opt registry wpo.po_gid in
       Option.iter (fun pp -> pp#update_ce_models wpo) printer
    )

let () = Wpo.add_removed_hook
    (fun wpo ->
       let registry = PRINTER.get () in
       Hashtbl.remove registry wpo.po_gid)

let () = Wpo.add_cleared_hook
    (fun () ->
       let registry = PRINTER.get () in
       Hashtbl.clear registry)

let lookup (node : ProofEngine.node) : printer =
  let tree = ProofEngine.tree node in
  let wpo = ProofEngine.main tree in
  let registry = PRINTER.get () in
  try
    let printer = Hashtbl.find registry wpo.po_gid in
    printer#update_ce_models wpo ;
    printer
  with Not_found ->
    let pp = new printer () in
    pp#update_ce_models wpo ;
    pp#on_selection (fun () -> R.emit printStatus) ;
    Hashtbl.add registry wpo.po_gid pp ; pp

(* for external use *)
let lookup_printer = lookup

let selection node = (lookup node)#selection
let setSelection node = (lookup node)#set_selection

(* -------------------------------------------------------------------------- *)
(* --- PrintSequent Request                                               --- *)
(* -------------------------------------------------------------------------- *)

let flags (type a) ~name ~descr tags : a R.input =
  (module struct
    type t = a
    let jtype = D.declare ~package ~name ~descr
        (P.Junion (List.map (fun (tg,_) -> P.Jtag tg) tags))
    let of_json js = List.assoc (Json.string js) tags
  end)

let iformat : Plang.iformat R.input =
  flags ~name:"iformat" ~descr:(Md.plain "Integer constants format")
    [ "dec", `Dec ; "hex", `Hex ; "bin", `Bin ]

let rformat : Plang.rformat R.input =
  flags ~name:"rformat" ~descr:(Md.plain "Real constants format")
    [ "ratio", `Ratio ; "float", `Float ; "double", `Double ]

let () =
  let printSequent = R.signature ~output:(module D.Jtext) () in
  let get_node = R.param_opt printSequent ~name:"node"
      ~descr:(Md.plain "Proof Node") (module Node) in
  let get_indent = R.param_opt printSequent ~name:"indent"
      ~descr:(Md.plain "Number of indentation spaces") (module D.Jint) in
  let get_margin = R.param_opt printSequent ~name:"margin"
      ~descr:(Md.plain "Maximal text width") (module D.Jint) in
  let get_iformat = R.param_opt printSequent ~name:"iformat"
      ~descr:(Md.plain "Integer constants format") iformat in
  let get_rformat = R.param_opt printSequent ~name:"rformat"
      ~descr:(Md.plain "Real constants format") rformat in
  let get_showce = R.param_opt printSequent ~name:"showce"
      ~descr:(Md.plain "Display counter examples") (module D.Jbool) in
  let get_autofocus = R.param_opt printSequent ~name:"autofocus"
      ~descr:(Md.plain "Auto-focus mode") (module D.Jbool) in
  let get_unmangled = R.param_opt printSequent ~name:"unmangled"
      ~descr:(Md.plain "Unmangled memory model") (module D.Jbool) in
  R.register_sig ~package
    ~kind:`GET
    ~name:"printSequent"
    ~descr:(Md.plain "Pretty-print the associated node")
    ~signals:[printStatus] printSequent
    begin fun rq () ->
      match get_node rq with
      | None -> D.jtext ""
      | Some node ->
        let ctxt = ProofEngine.node_context node in
        WpContext.on_context ctxt
          begin fun () ->
            let pp = lookup node in
            let indent = get_indent rq in
            let margin = get_margin rq in
            Option.iter pp#set_iformat (get_iformat rq) ;
            Option.iter pp#set_rformat (get_rformat rq) ;
            Option.iter pp#set_ce_mode (get_showce rq) ;
            Option.iter pp#set_focus_mode (get_autofocus rq) ;
            Option.iter pp#set_unmangled (get_unmangled rq) ;
            D.jpretty ?indent ?margin pp#pp_goal (ProofEngine.goal node)
          end ()
    end

(* -------------------------------------------------------------------------- *)
(* --- Selection Requests                                                 --- *)
(* -------------------------------------------------------------------------- *)

let () =
  R.register ~package
    ~kind:`SET
    ~name:"clearSelection"
    ~descr:(Md.plain "Reset node selection")
    ~input:(module Node)
    ~output:(module D.Junit)
    begin fun node ->
      let pp = lookup node in
      pp#reset ;
      pp#selected ;
    end

let () =
  let setSelection = R.signature ~output:(module D.Junit) () in
  let get_node = R.param setSelection ~name:"node"
      ~descr:(Md.plain "Proof Node") (module Node) in
  let get_part = R.param setSelection ~name:"part" ~default:`Term
      ~descr:(Md.plain "Selected part") (module Part) in
  let get_term = R.param_opt setSelection ~name:"term"
      ~descr:(Md.plain "Selected term") (module Term) in
  let get_extend = R.param setSelection ~name:"extend"
      ~descr:(Md.plain "Extending selection mode")
      ~default:false (module D.Jbool) in
  R.register_sig ~package
    ~kind:`SET
    ~name:"setSelection"
    ~descr:(Md.plain "Set node selection")
    setSelection
    begin fun rq () ->
      let node = get_node rq in
      let part = get_part rq in
      let term = get_term rq in
      let extend = get_extend rq in
      let pp = lookup node in
      let part = to_part (fst pp#sequent) part in
      pp#restore ~focus:(if extend then `Extend else `Focus) (part,term) ;
    end

let () =
  let getSelection = R.signature ~input:(module Node) () in
  let set_part = R.result_opt getSelection ~name:"part"
      ~descr:(Md.plain "Selected part") (module Part) in
  let set_term = R.result_opt getSelection ~name:"term"
      ~descr:(Md.plain "Selected term") (module Term) in
  R.register_sig ~package
    ~kind:`GET
    ~name:"getSelection"
    ~descr:(Md.plain "Get current selection in proof node")
    ~signals:[printStatus;proofStatus]
    getSelection
    begin fun rq node ->
      let (part,term) = (lookup node)#target in
      set_part rq (if part <> Term then Some (of_part part) else None);
      set_term rq term;
    end

(* -------------------------------------------------------------------------- *)
(* --- Prover Scheduling                                                  --- *)
(* -------------------------------------------------------------------------- *)

let runProvers ?mode ?timeout ?provers node =
  let wpo = ProofEngine.goal node in
  let provers = match provers with
    | Some ps -> ps
    | None -> WpApi.getProvers () in
  let timeout = match timeout with
    | Some t -> t
    | None -> Wp_parameters.Timeout.get () in
  let mode = match mode with
    | Some mode -> mode
    | None -> Prover.InteractiveMode.get () in
  let config =
    let cfg = VCS.current () in
    { cfg with timeout = Some (float timeout) } in
  if not @@ Wpo.is_trivial wpo then
    List.iter
      (fun prv ->
         let backup = Wpo.get_result wpo prv in
         let result _ p r =
           if p = Prover.Qed && VCS.is_valid r then
             Wpo.set_result wpo prv VCS.no_result
           else if not @@ VCS.is_verdict r then
             Wpo.set_result wpo prv backup in
         let process () = ProverTask.prove ~config ~mode ~result wpo prv in
         let thread = Task.thread @@ Task.later process () in
         let status = VCS.computing (fun () -> Task.cancel thread) in
         Wpo.set_result wpo prv status ;
         let server = ProverTask.server () in
         Task.spawn server thread ;
         Task.launch server ;
      ) provers

let () =
  let iRunProvers = R.signature ~output:(module D.Junit) () in
  let get_node = R.param iRunProvers (module Node)
      ~name:"node" ~descr:(Md.plain "Proof node") in
  let get_timeout = R.param_opt iRunProvers (module D.Jint)
      ~name:"timeout"
      ~descr:(Md.plain "Prover timeout (in seconds, default: current)") in
  let get_provers = R.param_opt iRunProvers (module WpApi.Provers)
      ~name:"provers"
      ~descr:(Md.plain "Prover selection (default: current") in
  let get_mode = R.param_opt iRunProvers (module WpApi.InteractiveMode)
      ~name:"mode"
      ~descr:(Md.plain "Interactive provers mode") in
  R.register_sig iRunProvers ~package ~kind:`SET
    ~name:"runProvers"
    ~descr:(Md.plain "Schedule provers on proof node")
    begin fun rq () ->
      let node = get_node rq in
      let provers = get_provers rq in
      let timeout = get_timeout rq in
      let mode = get_mode rq in
      runProvers ?mode ?timeout ?provers node
    end

let killProvers ?provers node =
  let wpo = ProofEngine.goal node in
  let filter =
    match provers with
    | None | Some [] -> fun _ -> true
    | Some prvs -> fun p -> List.exists (Prover.equal p) prvs
  in
  List.iter
    (fun (prv,res) ->
       match res.VCS.verdict with
       | Computing kill when filter prv -> kill ()
       | _ -> ()
    ) @@ Wpo.get_results wpo

let () =
  let iKillProvers = R.signature ~output:(module D.Junit) () in
  let get_node = R.param iKillProvers (module Node)
      ~name:"node" ~descr:(Md.plain "Proof node") in
  let get_provers = R.param_opt iKillProvers (module WpApi.Provers)
      ~name:"provers"
      ~descr:(Md.plain "Prover selection (default: all running provers") in
  R.register_sig iKillProvers ~package ~kind:`SET
    ~name:"killProvers"
    ~descr:(Md.plain "Interrupt running provers on proof node")
    begin fun rq () ->
      let node = get_node rq in
      let provers = get_provers rq in
      killProvers ?provers node
    end

let clearProvers ?provers node =
  let wpo = ProofEngine.goal node in
  let clear p = if Prover.is_extern p then Wpo.set_result wpo p VCS.no_result in
  begin
    match provers with
    | None -> List.iter (fun (prv,_) -> clear prv) @@ Wpo.get_results wpo ;
    | Some prvs -> List.iter clear prvs
  end

let () =
  let iClearProvers = R.signature ~output:(module D.Junit) () in
  let get_node = R.param iClearProvers (module Node)
      ~name:"node" ~descr:(Md.plain "Proof node") in
  let get_provers = R.param_opt iClearProvers (module WpApi.Provers)
      ~name:"provers"
      ~descr:(Md.plain "Prover selection (default: all results") in
  R.register_sig iClearProvers ~package ~kind:`SET
    ~name:"clearProvers"
    ~descr:(Md.plain "Remove prover results from proof node")
    begin fun rq () ->
      let node = get_node rq in
      let provers = get_provers rq in
      clearProvers ?provers node
    end

(* -------------------------------------------------------------------------- *)
(* --- Script Management                                                  --- *)
(* -------------------------------------------------------------------------- *)

let () =
  let script = R.signature ~input:(module WpApi.Goal) () in
  let set_proof = R.result script (module D.Jbool)
      ~name:"proof" ~descr:(Md.plain "Some Proof Tree can be Saved") in
  let set_script = R.result_opt script (module D.Jstring)
      ~name:"script" ~descr:(Md.plain "Script File (if any)") in
  let set_saved = R.result script (module D.Jbool)
      ~name:"saved" ~descr:(Md.plain "Current Proof Script has been Saved") in
  R.register_sig script ~package ~kind:`GET
    ~name:"getScriptStatus"
    ~descr:(Md.plain "Script Status for a given Goal")
    ~signals:[proofStatus]
    begin fun rq goal ->
      let tree = ProofEngine.proof ~main:goal in
      set_saved rq (ProofEngine.saved tree) ;
      set_proof rq (ProofEngine.has_script tree) ;
      set_script rq
        begin
          match ProofSession.get goal with
          | NoScript -> None | Script file | Deprecated file ->
            Some (Filepath.to_string_abs file)
        end ;
    end

let () =
  R.register ~package ~kind:`SET
    ~name:"saveScript" ~descr:(Md.plain "Save Script for the Goal")
    ~input:(module WpApi.Goal)
    ~output:(module D.Junit)
    begin fun goal ->
      let tree = ProofEngine.proof ~main:goal in
      let json = ProofScript.encode (ProofEngine.script tree) in
      ProofSession.save ~stdout:false goal json ;
      ProofEngine.set_saved tree true ;
    end

let () =
  R.register ~package ~kind:`SET
    ~name:"runScript"
    ~descr:(Md.plain "Replay Saved Script for the Goal (if any)")
    ~input:(module WpApi.Goal)
    ~output:(module D.Junit)
    begin fun goal ->
      let tree = ProofEngine.proof ~main:goal in
      ProofEngine.clear_tree tree ;
      ProverScript.spawn
        ~provers:(WpApi.getProvers())
        ~success:(fun _ _ -> ProofEngine.forward tree)
        goal ;
      let server = ProverTask.server () in
      Task.launch server ;
    end

(* -------------------------------------------------------------------------- *)
(* --- Full Trash                                                         --- *)
(* -------------------------------------------------------------------------- *)

let () =
  R.register ~package ~kind:`SET
    ~name:"clearProofScript"
    ~descr:(Md.plain "Clear Proof and Remove any Saved Script for the Goal")
    ~input:(module WpApi.Goal)
    ~output:(module D.Junit)
    begin fun goal ->
      let tree = ProofEngine.proof ~main:goal in
      ProofEngine.clear_tree tree ;
      ProofSession.remove goal ;
    end

(* -------------------------------------------------------------------------- *)
