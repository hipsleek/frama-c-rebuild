(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open LogicUsage
open VCS
open Cil_types
open Cil_datatype
open Lang

type index =
  | Axiomatic of string option
  | Function of kernel_function * string option

let bar = String.make 60 '-'
let flow = ref false

(* -------------------------------------------------------------------------- *)
(* --- Pretty Printers                                                    --- *)
(* -------------------------------------------------------------------------- *)

let pp_index fmt = function
  | Axiomatic None -> Format.pp_print_string fmt "Axiomatics"
  | Axiomatic (Some a) -> Format.pp_print_string fmt a
  | Function(f,None) -> Kernel_function.pretty fmt f
  | Function(f,Some b) -> Format.fprintf fmt "%a for %s:" Kernel_function.pretty f b

let pp_axiomatics fmt ax =
  flow := true ;
  match ax with
  | None -> Format.fprintf fmt "%s@\n  Global@\n%s@\n@\n" bar bar
  | Some a -> Format.fprintf fmt "%s@\n  Axiomatic '%s'@\n%s@\n@\n" bar a bar

let pp_function fmt kf bhv =
  flow := true ;
  match bhv with
  | None ->
    Format.fprintf fmt
      "%s@\n  Function %s@\n%s@\n@\n"
      bar (Kernel_function.get_name kf) bar
  | Some bhv ->
    Format.fprintf fmt
      "%s@\n  Function %s with behavior %s@\n%s@\n@\n"
      bar (Kernel_function.get_name kf) bhv bar

let pp_warnings fmt ws =
  List.iter (fun w -> Format.fprintf fmt "%a@\n" Warning.pretty w) ws

(* ------------------------------------------------------------------------ *)
(* ---  Proof Obligations Definition                                    --- *)
(* ------------------------------------------------------------------------ *)

module DISK =
struct

  let file ~id ~model ?prover ?suffix ~ext () =
    let mid = Wp_parameters.Output.get_dir (WpContext.MODEL.id model) in
    let buffer = Buffer.create 80 in
    let fmt = Format.formatter_of_buffer buffer in
    Format.fprintf fmt "%s/%s" (Filepath.to_string_abs mid) id ;
    (match prover with None -> () | Some p ->
        Format.fprintf fmt "_%s" (Prover.filename_for p)) ;
    (match suffix with None -> () | Some s ->
        Format.fprintf fmt "_%s" s) ;
    Format.fprintf fmt ".%s" ext ;
    Format.pp_print_flush fmt ();
    Filepath.of_string (Buffer.contents buffer)

  let file_logout ~pid ~model ~prover =
    let id = WpPropId.get_propid pid in
    file ~id ~model ~prover ~ext:"out" ()

  let file_logerr ~pid ~model ~prover =
    let id = WpPropId.get_propid pid in
    file ~id ~model ~prover ~ext:"err" ()

  let file_goal  ~pid ~model driver prover =
    let basefile = Why3.Driver.get_filename driver
        ~input_file:"f" ~theory_name:"t" ~goal_name:"g" in
    let ext = Filename.extension basefile in
    let ext = if ext = "" then ".why" else ext in
    let ext = String.sub ext 1 (String.length ext - 1) in
    let id = WpPropId.get_propid pid in
    file ~id ~model ~prover:(Why3 prover) ~ext ()

end

module GOAL =
struct

  type t = {
    mutable time : float ;
    mutable simplified : bool ;
    mutable sequent : Conditions.sequent ;
    mutable opened : F.pred ;
    mutable closed : F.pred ;
    mutable probes : F.term Probe.Map.t ;
  }

  let empty = Conditions.empty

  let dummy = {
    time = 0.0 ;
    simplified = false ;
    sequent = empty , F.p_false ;
    opened = F.p_false ;
    closed = F.p_false ;
    probes = Probe.Map.empty ;
  }

  let trivial = {
    time = 0.0 ;
    simplified = true ;
    sequent = empty , F.p_true ;
    opened = F.p_true ;
    closed = F.p_true ;
    probes = Probe.Map.empty ;
  }

  let make sequent = {
    time = 0.0 ;
    simplified = false ;
    sequent = sequent ;
    opened = F.p_false ;
    closed = F.p_false ;
    probes = Probe.Map.empty ;
  }

  let is_computed g = g.simplified
  let is_trivial g = Conditions.is_trivial g.sequent

  let dkey = Wp_parameters.register_category "qed"

  let apply option phi g =
    try
      Async.yield () ;
      Wp_parameters.debug ~dkey "Apply %s" option ;
      g.sequent <- phi g.sequent ;
    with exn when Wp_parameters.protect exn ->
      Wp_parameters.warning ~current:false ~once:true
        "Goal simplification aborted (%s):@\n\
         Exception %S@\n\
         Re-run with debug level 1+ for details."
        option (Printexc.to_string exn)

  let default_simplifiers = [
    Wp_parameters.SimplifyIsCint.get, Cint.is_cint_simplifier ;
    Wp_parameters.SimplifyLandMask.get, Cint.mask_simplifier ;
  ]

  let preprocess g =
    if Wp_parameters.Qed.get () then
      begin
        apply "introduction" Conditions.introduction_eq g ;
        let fold acc (get,solver) = if get () then solver::acc else acc in
        let solvers = List.fold_left fold [] default_simplifiers in
        apply "-wp-simplify-*" (Conditions.simplify ~solvers) g ;
        if Wp_parameters.FilterInit.get ()
        then apply "-wp-filter-init" Conditions.init_filter g ;
        if Wp_parameters.Prune.get ()
        then apply "-wp-pruning" (Conditions.pruning ~solvers) g ;
        if Wp_parameters.Filter.get ()
        then apply "-wp-filter" Conditions.filter g ;
        if Wp_parameters.Parasite.get ()
        then apply "-wp-parasite" Conditions.parasite g ;
      end
    else
      begin
        if Wp_parameters.Clean.get ()
        then apply "-wp-clean" Conditions.clean g ;
      end ;
    begin
      if Conditions.is_trivial g.sequent then
        g.sequent <- Conditions.trivial ;
      g.opened <- Conditions.property g.sequent ;
      g.closed <- F.p_close g.opened ;
    end

  let safecompute ~pid g =
    begin
      g.simplified <- true ;
      let timer = ref 0.0 in
      Wp_parameters.debug ~dkey "Simplify %a" WpPropId.pretty pid ;
      Command.time ~rmax:timer preprocess g ;
      Wp_parameters.debug ~dkey "Simplification time: %a"
        Rformat.pp_time !timer ;
      g.time <- !timer ;
      g.probes <- Conditions.probes @@ fst g.sequent ;
    end

  let compute ~pid g =
    if not g.simplified then
      Lang.local ~vars:(Conditions.vars_seq g.sequent)
        (safecompute ~pid) g

  let compute_proof ~pid ?(opened=false) g =
    compute ~pid g ; if opened then g.opened else g.closed
  let compute_probes ~pid g = compute ~pid g ; g.probes
  let compute_descr ~pid g = compute ~pid g ; g.sequent
  let get_descr g = g.sequent
  let qed_time g = g.time

end

module VC_Annot =
struct

  type t = {
    (* Generally empty, but for Lemmas *)
    axioms : Definitions.axioms option ;
    goal : GOAL.t ;
    tags : Splitter.tag list ;
    warn : Warning.t list ;
    deps : Property.Set.t ;
    path : Stmt.Set.t ;
    source : (stmt * Mcfg.goal_source) option ;
  }

  let repr = {
    axioms = None ;
    goal = GOAL.dummy ;
    tags = [] ;
    warn = [] ;
    deps = Property.Set.empty ;
    path = Stmt.Set.empty ;
    source = None ;
  }

  let resolve ~pid vcq = GOAL.compute_proof ~pid vcq.goal == Lang.F.p_true
  let is_trivial vcq = GOAL.is_trivial vcq.goal

  let pp_effect fmt s e =
    let line = Stmt.loc s |> Fileloc.line in
    let desc = match e with
      | Mcfg.FromCode -> "Effect"
      | Mcfg.FromCall -> "Call Effect"
      | Mcfg.FromReturn -> "Call Result"
    in
    Format.fprintf fmt "%s at line %d@\n" desc line

  let pp_terminates fmt  s e =
    let line = Stmt.loc s |> Fileloc.line in
    let desc = match e with
      | Mcfg.Loop -> "Loop termination"
      | Mcfg.Terminates -> "Call terminates"
      | Mcfg.Decreases -> "Call decreases"
      | Mcfg.MissingTerminates -> "Call terminates (missing terminates)"
      | Mcfg.MissingDecreases -> "Call terminates (missing decreases)"
      | Mcfg.Dependencies -> "Call terminates (dependencies)"
    in
    Format.fprintf fmt "%s at line %d@\n" desc line

  let pp_source fmt = function
    | None -> ()
    | Some (s, Mcfg.Effect e) -> pp_effect fmt s e
    | Some (s, Mcfg.Terminates e) -> pp_terminates fmt s e

  let pretty fmt pid vc results =
    begin
      Format.fprintf fmt "@{<bf>Goal@} %a:@\n" WpPropId.pretty pid ;
      pp_source fmt vc.source ;
      if vc.tags <> [] then
        begin
          Format.fprintf fmt "@[<hov 2>@{<bf>Tags@}:" ;
          List.iter (fun tg -> Format.fprintf fmt "@ %a" Splitter.pretty tg) vc.tags ;
          Format.fprintf fmt "@].@\n" ;
        end ;
      begin match vc.axioms with
        | Some (_, depends) when depends <> [] ->
          Format.fprintf fmt "@[<hov 2>@{<bf>Assume Lemmas@}:" ;
          List.iter
            (fun a -> Format.fprintf fmt "@ '%s'" a.lem_name)
            depends ;
          Format.fprintf fmt "@]@." ;
        | _ -> ()
      end ;
      pp_warnings fmt vc.warn ;
      Pcond.pretty fmt (GOAL.compute_descr ~pid vc.goal) ;
      List.iter
        (fun (prover,result) ->
           if result.verdict <> NoResult then
             Format.fprintf fmt "Prover %a returns %t@\n"
               Prover.pretty prover
               (pp_result_qualif prover result) ;
           if Wp_parameters.CounterExamples.get () then
             pp_model fmt result.prover_model
        ) results ;
    end

end

(* ------------------------------------------------------------------------ *)
(* ---  Proof Obligations Database                                      --- *)
(* ------------------------------------------------------------------------ *)

type po = t and t = {
    po_gid   : string ;  (* goal identifier *)
    po_sid   : string ;  (* goal short identifier (without model) *)
    po_name  : string ;  (* goal informal name *)
    po_idx   : index ;   (* goal index *)
    po_model : WpContext.model ;
    po_pid   : WpPropId.prop_id ; (* goal target property *)
    po_formula : VC_Annot.t ; (* proof obligation *)
  }

let get_index w = w.po_idx
let get_label w = WpPropId.label_of_prop_id w.po_pid
let get_model x = x.po_model
let get_scope w = match w.po_idx with
  | Axiomatic _ -> WpContext.Global
  | Function(kf,_) -> WpContext.Kf kf
let get_context w = w.po_model , get_scope w

let get_depend wpo =
  let open LogicUsage in
  let deps = wpo.po_formula.deps in
  let axioms = wpo.po_formula.axioms in
  List.rev_append
    (Option.fold ~none:[] ~some:(fun (_, l) -> List.map ip_lemma l) axioms)
    (Property.Set.elements deps)

let get_file_logout w prover =
  DISK.file_logout ~pid:w.po_pid ~model:(get_model w) ~prover

let get_file_logerr w prover =
  DISK.file_logerr ~pid:w.po_pid ~model:(get_model w) ~prover

module Index =
struct
  type t = index
  let cmpopt a b =
    match a,b with
    | Some a,Some b -> String.compare a b
    | None,Some _ -> (-1)
    | Some _,None -> 1
    | None,None -> 0
  let compare a b =
    match a,b with
    | Axiomatic a , Axiomatic b -> cmpopt a b
    | Axiomatic _ , Function _ -> (-1)
    | Function _ , Axiomatic _ -> 1
    | Function(f,a) , Function(g,b) ->
      let c =
        if Kernel_function.equal f g then 0 else
          String.compare
            (Kernel_function.get_name f)
            (Kernel_function.get_name g)
      in
      if c=0 then cmpopt a b else c
end

module S =
  Datatype.Make_with_collections
    (struct
      type t = po
      include Datatype.Undefined
      let hash a = Hashtbl.hash a.po_gid
      let equal a b = (a.po_gid = b.po_gid)
      let compare a b =
        let c = Index.compare a.po_idx b.po_idx in
        if c<>0 then c else
          let c = WpPropId.compare_prop_id a.po_pid b.po_pid in
          if c<>0 then c else
            let ma = get_model a |> WpContext.MODEL.descr in
            let mb = get_model b |> WpContext.MODEL.descr in
            let c = String.compare ma mb in
            if c<>0 then c else
              String.compare a.po_gid b.po_gid
      let pretty fmt wpo = Format.pp_print_string fmt wpo.po_name
      let name = "Wpo.po"
      let reprs =
        [{
          po_idx = Function(List.hd Kernel_function.reprs,Some "default") ;
          po_pid = List.hd WpPropId.PropId.reprs;
          po_sid = "";
          po_gid = "";
          po_model = WpContext.MODEL.repr ;
          po_name = "dummy";
          po_formula = VC_Annot.repr ;
        }]
    end)

module WpoType = S
module ProverType =
  Datatype.Make
    (struct
      type t = Prover.t
      include Datatype.Undefined
      let name = "Wpo.prover"
      let reprs = [ Prover.Qed ]
    end)

module ResultType =
  Datatype.Make
    (struct
      type t = result
      include Datatype.Undefined
      let name = "Wpo.result"
      let reprs =
        List.map VCS.result
          [ Valid ; Unknown ; Timeout ; Failed ]
    end)

(* -------------------------------------------------------------------------- *)
(* --- Getters                                                            --- *)
(* -------------------------------------------------------------------------- *)

let get_gid g = g.po_gid
let get_property g = WpPropId.property_of_id g.po_pid

let qed_time wpo =
  GOAL.qed_time wpo.po_formula.goal

(* -------------------------------------------------------------------------- *)
(* --- Proof Collector                                                    --- *)
(* -------------------------------------------------------------------------- *)

let is_tactic t = WpPropId.is_tactic t.po_pid
let is_smoke_test t = WpPropId.is_smoke_test t.po_pid

module Hproof = Hashtbl.Make(Datatype.Pair(Datatype.String)(Property))
(* Table indexed by ( Model name , Property proved ) *)

module Results =
struct

  type t = {
    mutable dps : result Prover.Pmap.t ;
  }

  let create () = { dps = Prover.Pmap.empty }

  let get w p =
    Prover.Pmap.find p w.dps

  let clear w =
    Prover.Pmap.iter (fun _ r ->
        match r.verdict with
        | VCS.Computing kill -> kill ()
        | _ -> ()
      ) w.dps ;
    w.dps <- Prover.Pmap.empty

  let replace w p r =
    begin
      if p = Prover.Qed then
        (w.dps <- Prover.Pmap.filter (fun _ r -> VCS.is_verdict r) w.dps) ;
      w.dps <- Prover.Pmap.add p r w.dps
    end

  let list w =
    List.filter (fun (_,r) -> not @@ VCS.is_none r) @@ Prover.Pmap.bindings w.dps

end

(* -------------------------------------------------------------------------- *)
(* --- Wpo Hooks                                                          --- *)
(* -------------------------------------------------------------------------- *)

let modified_hooks : (t -> unit) list ref = ref []
let removed_hooks : (t -> unit) list ref = ref []
let cleared_hooks : (unit -> unit) list ref = ref []

let add_modified_hook f = modified_hooks := !modified_hooks @ [f]
let add_removed_hook f = removed_hooks := !removed_hooks @ [f]
let add_cleared_hook f = cleared_hooks := !cleared_hooks @ [f]

let modified g =
  List.iter (fun f -> f g) !modified_hooks

let removed g =
  List.iter (fun f -> f g) !removed_hooks

(* -------------------------------------------------------------------------- *)
(* --- Wpo Database                                                       --- *)
(* -------------------------------------------------------------------------- *)

module WPOset = WpoType.Set
module WPOmap = WpoType.Map
module Gmap = Map.Make(Index)
module Fmap = Kernel_function.Map
module Pmap = Property.Map

let index_wpo iadd iget k w m =
  let set = try iget k m with Not_found -> WPOset.empty in
  iadd k (WPOset.add w set) m

let unindex_wpo iadd iget k w m =
  try
    let set = iget k m in
    iadd k (WPOset.remove w set) m
  with Not_found -> m

type system = {
  mutable wpo_idx : WPOset.t Gmap.t ; (* index -> WPOs *)
  mutable wpo_kf : WPOset.t Fmap.t ; (* kf -> WPOs *)
  mutable wpo_ip : WPOset.t Pmap.t ; (* ip -> WPOs *)
  mutable age : int WPOmap.t ; (* wpo -> age *)
  mutable results : Results.t WPOmap.t ; (* results collector *)
  proofs : WpPropId.proof Hproof.t ; (* proof collector *)
}

let create_system () =
  {
    wpo_idx = Gmap.empty ;
    wpo_kf = Fmap.empty ;
    wpo_ip = Pmap.empty ;
    results = WPOmap.empty ;
    age = WPOmap.empty ;
    proofs = Hproof.create 131 ;
  }

let clear_system system =
  begin
    system.wpo_idx <- Gmap.empty ;
    system.wpo_kf <- Fmap.empty ;
    system.wpo_ip <- Pmap.empty ;
    system.results <- WPOmap.empty ;
    system.age <- WPOmap.empty ;
    Hproof.clear system.proofs ;
    List.iter (fun f -> f ()) !cleared_hooks ;
  end

module SYSTEM = State_builder.Ref
    (Datatype.Make
       (struct
         include Datatype.Undefined
         type t = system
         let name = "Wpo.SYSTEM.Datatype"
         let reprs = [ create_system () ]
         let mem_project = Datatype.never_any_project
       end))
    (struct
      let name = "Wpo.SYSTEM.System"
      let dependencies = [ Ast.self ]
      let default = create_system
    end)

let clear () = clear_system (SYSTEM.get ())

(* ------------------------------------------------------------------------ *)
(* ---  WPO Construction                                                --- *)
(* ------------------------------------------------------------------------ *)

(* A WPO is uniquely determined by :
   1. The model name (unique per updater by construction)
   2. The kernel-function
   3. The behavior
   4. The target prop-id
*)

(* -------------------------------------------------------------------------- *)
(* --- Registry of POs                                                    --- *)
(* -------------------------------------------------------------------------- *)

let added = ref 0

let age g =
  let system = SYSTEM.get () in
  try WPOmap.find g system.age with Not_found -> 0

let current_age = ref (-1)

let proof g ip = ( get_context g |> WpContext.S.id , ip )

let add g =
  let system = SYSTEM.get () in
  begin
    let ip = WpPropId.property_of_id g.po_pid in
    Hproof.remove system.proofs (proof g ip) ;
    let age = incr current_age; !current_age in
    system.age <- WPOmap.add g age system.age ;
    system.results <- WPOmap.remove g system.results ;
    system.wpo_idx <- index_wpo Gmap.add Gmap.find g.po_idx g system.wpo_idx ;
    system.wpo_ip <- index_wpo Pmap.add Pmap.find ip g system.wpo_ip ;
    begin
      match g.po_idx with
      | Function(kf,_) ->
        system.wpo_kf <- index_wpo Fmap.add Fmap.find kf g system.wpo_kf
      | _ -> ()
    end ;
    incr added ;
    if !added >= 100 then
      begin
        added := 0 ;
        Gmap.iter
          (fun _ ws -> WPOset.iter (fun _ -> incr added) ws)
          system.wpo_idx ;
        if not (Wp_parameters.has_dkey Prover.dkey_shell) then
          Wp_parameters.feedback ~ontty:`Feedback "Computing [%d goals...]" !added ;
        added := 0 ;
      end ;
    modified g ;
  end

let remove g =
  let system = SYSTEM.get () in
  begin
    let ip = WpPropId.property_of_id g.po_pid in
    system.wpo_idx <- unindex_wpo Gmap.add Gmap.find g.po_idx g system.wpo_idx ;
    system.wpo_ip <- unindex_wpo Pmap.add Pmap.find ip g system.wpo_ip ;
    begin
      match g.po_idx with
      | Function(kf,_) ->
        system.wpo_kf <- unindex_wpo Fmap.add Fmap.find kf g system.wpo_kf
      | Axiomatic _ -> ()
    end ;
    system.results <- WPOmap.remove g system.results ;
    Hproof.remove system.proofs (proof g ip) ;
    removed g ;
  end

let warnings wpo = wpo.po_formula.VC_Annot.warn

let get_target g = WpPropId.property_of_id g.po_pid

let get_proof g =
  let system = SYSTEM.get () in
  let target = get_target g in
  let status =
    try
      let proof = Hproof.find system.proofs (proof g target) in
      if is_smoke_test g then
        if WpPropId.is_proved proof then `Failed else
        if WpPropId.is_invalid proof then `Passed else
          `Unknown
      else
      if WpPropId.is_proved proof then `Passed else `Unknown
    with Not_found -> `Unknown
  in status , target

let find_proof system g =
  let pi = proof g (WpPropId.property_of_id g.po_pid) in
  try Hproof.find system.proofs pi
  with Not_found ->
    let proof = WpPropId.create_proof g.po_pid in
    Hproof.add system.proofs pi proof ; proof

let clear_results g =
  let system = SYSTEM.get () in
  try
    let rs = WPOmap.find g system.results in
    Results.clear rs ;
    modified g ;
  with Not_found -> ()

let wp_reached_set_doomed = ref (fun _ _ -> assert false)
let set_doomed emitter pid = !wp_reached_set_doomed emitter pid

let set_result g p r =
  let system = SYSTEM.get () in
  let rs =
    try WPOmap.find g system.results
    with Not_found ->
      let rs = Results.create () in
      system.results <- WPOmap.add g rs system.results ; rs
  in
  Results.replace rs p r ;
  if not (WpPropId.is_check g.po_pid) &&
     not (WpPropId.is_tactic g.po_pid) &&
     VCS.is_verdict r
  then
    begin
      let smoke = is_smoke_test g in
      let proof = find_proof system g in
      let emitter = WpContext.get_emitter g.po_model in
      let target = WpPropId.target proof in
      let unproved = not (WpPropId.is_proved proof) in
      begin
        if VCS.is_valid r then
          WpPropId.add_proof proof g.po_pid (get_depend g)
        else if smoke then
          WpPropId.add_invalid_proof proof ;
      end ;
      let proved = WpPropId.is_proved proof in
      let status =
        if smoke then
          if proved
          then Property_status.False_if_reachable (* All goals SAT *)
          else if WpPropId.is_invalid proof
          then Property_status.True (* Some goal is UNSAT *)
          else Property_status.Dont_know (* Not finished yet *)
        else
        if proved
        then Property_status.True
        else Property_status.Dont_know
      in
      let hyps = if smoke then [] else WpPropId.dependencies proof in
      Property_status.emit emitter ~hyps target status ;
      if smoke && unproved && proved then
        set_doomed emitter g.po_pid ;
    end ;
  modified g

let has_verdict g p =
  let system = SYSTEM.get () in
  try VCS.is_verdict (Results.get (WPOmap.find g system.results) p)
  with Not_found -> false

let get_result g p : VCS.result =
  let system = SYSTEM.get () in
  try Results.get (WPOmap.find g system.results) p
  with Not_found -> VCS.no_result

let get_results g =
  let system = SYSTEM.get () in
  try Results.list @@ WPOmap.find g system.results
  with Not_found -> []

let get_prover_results g =
  List.filter (fun (p,_) -> not @@ Prover.is_tactical p) @@ get_results g

let is_trivial g =
  VC_Annot.is_trivial g.po_formula

let reduce g =
  let pid = g.po_pid in
  WpContext.on_context (get_context g) (VC_Annot.resolve ~pid) g.po_formula

let resolve g =
  is_trivial g ||
  let valid = reduce g in
  if valid then
    let result = VCS.result ~solver:(qed_time g) VCS.Valid in
    ( set_result g Prover.Qed result ; true )
  else false

let computed g =
  GOAL.is_computed g.po_formula.goal

let compute g =
  let ctxt = get_context g in
  let pid = g.po_pid in
  g.po_formula.axioms ,
  let goal = g.po_formula.goal in
  let qed = GOAL.is_computed goal in
  let seq = WpContext.on_context ctxt (GOAL.compute_descr ~pid) goal in
  if not qed then modified g ; seq

let is_fully_valid g =
  is_trivial g ||
  List.exists (fun (_,r) -> VCS.is_valid r) @@ get_results g

let is_locally_valid g =
  is_trivial g ||
  List.exists
    (fun (p,r) -> not (Prover.is_tactical p) && VCS.is_valid r) @@ get_results g

let all_not_valid g =
  not (is_trivial g) &&
  List.for_all (fun (_,r) -> VCS.is_not_valid r) @@ get_results g

let is_passed g =
  if is_smoke_test g then
    all_not_valid g
  else
    is_fully_valid g

let has_unknown g =
  not (is_fully_valid g) &&
  List.exists
    (fun (p,r) ->
       not (Prover.is_tactical p) && VCS.is_verdict r && not (VCS.is_valid r))
    (get_results g)

(* -------------------------------------------------------------------------- *)
(* --- Proof Obligations : Pretty-printing                                --- *)
(* -------------------------------------------------------------------------- *)

let pp_title fmt w = Format.pp_print_string fmt w.po_name

let pp_goal_model fmt w =
  VC_Annot.pretty fmt w.po_pid w.po_formula (get_results w)

let pp_goal fmt w = WpContext.on_context (get_context w) (pp_goal_model fmt) w

let pp_flow fmt =
  Format.fprintf fmt "@\n%s@\n" bar ;
  flow := false

let pp_goal_flow fmt g =
  begin
    if not !flow then Format.pp_print_newline fmt () ;
    pp_goal fmt g ;
    Format.fprintf fmt "@\n%s@." bar ;
    flow := false ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Iterator                                                           --- *)
(* -------------------------------------------------------------------------- *)

type part =
  | Pnone
  | Paxiomatic of string option
  | Pbehavior of kernel_function * string option


let iter ?ip ?index ?on_axiomatics ?on_behavior ?on_goal () =
  let system = SYSTEM.get () in
  let current = ref Pnone in
  let apply_lemma a =
    match on_axiomatics with None -> () | Some phi -> phi a in
  let apply_behavior f bhv =
    match on_behavior with None -> () | Some phi -> phi f bhv in
  let on_part idx =
    match !current , idx with
    | Paxiomatic a , Axiomatic b when a=b -> ()
    | _ , Axiomatic b -> apply_lemma b ; current := Paxiomatic b
    | Pbehavior(f,None) , Function(g,None) when Kernel_function.equal f g -> ()
    | Pbehavior(f,Some a) , Function(g,Some b) when Kernel_function.equal f g && a=b -> ()
    | _ , Function(g,bhv) -> apply_behavior g bhv ; current := Pbehavior(g,bhv)
  in
  let on_goals poset =
    if not (WPOset.is_empty poset) then
      begin
        match on_goal with
        | None -> ()
        | Some phi -> WPOset.iter phi poset
      end
  in
  match index,ip with
  | None,None ->
    Gmap.iter (fun idx ws -> on_part idx ; on_goals ws) system.wpo_idx
  | _,Some ip ->
    begin
      match on_goal with
      | None -> ()
      | Some phi ->
        let poset =
          try Pmap.find ip system.wpo_ip
          with Not_found -> WPOset.empty in
        WPOset.iter phi poset
    end
  | Some (Function(kf,None)),None ->
    begin
      try on_goals (Fmap.find kf system.wpo_kf)
      with Not_found -> ()
    end
  | Some idx,None ->
    begin
      try on_goals (Gmap.find idx system.wpo_idx)
      with Not_found -> ()
    end

let iter_on_goals f = iter ~on_goal:f ()

let goals_of_property prop =
  let system = SYSTEM.get () in
  let poset =
    try Pmap.find prop system.wpo_ip
    with Not_found -> WPOset.empty
  in
  WPOset.elements poset

(* -------------------------------------------------------------------------- *)
(* --- Generators                                                         --- *)
(* -------------------------------------------------------------------------- *)

class type generator =
  object
    method model : WpContext.model
    method compute_ip : Property.t -> t Bag.t
    method compute_call : stmt -> t Bag.t
    method compute_main :
      ?fct:Wp_parameters.functions ->
      ?bhv:string list ->
      ?prop:string list ->
      unit -> t Bag.t
  end

(* -------------------------------------------------------------------------- *)
