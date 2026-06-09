(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Tactical
open ProofScript

let dkey_pp_allgoals = Wp_parameters.register_category "script:allgoals"

(* -------------------------------------------------------------------------- *)
(* --- Alternatives Ordering                                              --- *)
(* -------------------------------------------------------------------------- *)

module Priority =
struct
  open VCS

  let stage = function
    | Prover( Qed , { verdict = Valid } ) -> 0
    | Prover( Why3 _ , { verdict = Valid } ) -> 1
    | Tactic _ -> 2
    | Prover _ -> 3
    | Error _ -> 4

  let time = function
    | Tactic _ | Error _ -> 0.0
    | Prover( _ , r ) -> r.prover_time +. r.solver_time

  let compare a b =
    let sa = stage a in
    let sb = stage b in
    if sa = sb
    then Stdlib.compare (time a) (time b)
    else sa - sb

  let sort script = List.stable_sort compare script

end

(* -------------------------------------------------------------------------- *)
(* --- Running Json-Tactical                                              --- *)
(* -------------------------------------------------------------------------- *)

let jconfigure (console : #Tactical.feedback) jtactic goal =
  let _ , sequent = Wpo.compute goal in
  match ProofScript.configure jtactic sequent with
  | None -> None
  | Some(tactical,selection) ->
    console#set_title "%s" tactical#title ;
    let verdict =
      try Lang.local ~pool:console#pool (tactical#select console) selection
      with Not_found | Exit -> Not_applicable
    in
    begin
      match verdict with
      | Applicable process when not console#has_error ->
        let strategy = jtactic.strategy in
        let script = ProofScript.jtactic ?strategy tactical selection in
        Some (script , process)
      | _ -> None
    end

let jfork tree ?node jtactic =
  let console = new ProofScript.console
    ~pool:(ProofEngine.pool tree)
    ~title:jtactic.header in
  try
    let anchor = ProofEngine.anchor tree ?node () in
    Option.iter (ProofEngine.set_hint anchor) jtactic.strategy ;
    let goal = ProofEngine.goal anchor in
    let ctxt = ProofEngine.node_context anchor in
    match WpContext.on_context ctxt (jconfigure console jtactic) goal with
    | None -> None
    | Some (script,process) ->
      Some (ProofEngine.fork tree ~anchor script process)
  with
  | Exit | Not_found | Invalid_argument _ ->
    console#set_error "Can not configure tactic" ; None
  | e ->
    console#set_error "Exception <%s>" (Printexc.to_string e) ;
    raise e

(* -------------------------------------------------------------------------- *)
(* --- Running Alternatives                                               --- *)
(* -------------------------------------------------------------------------- *)

open Task

module Env =
struct

  type t = {
    tree : ProofEngine.tree ;
    valid : bool ; (* play valid provers *)
    failed : bool ; (* play failed provers *)
    provers : Prover.t list ;
    progress : Wpo.t -> string -> unit ;
    result : Wpo.t -> Prover.t -> VCS.result -> unit ;
    success : Wpo.t -> Prover.t option -> unit ;
    depth : int ;
    width : int ;
    auto : Strategy.heuristic list ; (* DEPRECATED *)
    strategies : bool ;
    mutable pending : int ; (* pending jobs *)
    mutable signaled : bool ;
    backtrack : int ;
    mutable backtracking : backtracking option ;
  }

  and backtracking = {
    bk_node : ProofEngine.node ;
    bk_depth : int ; (* depth of search *)
    mutable bk_best : int ;    (* best index, (-1) for none *)
    mutable bk_pending : int ; (* best pending, max_int when none *)
  }

  let tree env = env.tree

  let play env prv res =
    List.mem prv env.provers &&
    if VCS.is_valid res then env.valid else env.failed

  let progress env msg = env.progress (ProofEngine.main env.tree) msg

  let validate env =
    ProofEngine.validate env.tree ;
    if not env.signaled then
      let wpo = ProofEngine.main env.tree in
      let proved = Wpo.is_passed wpo in
      if proved || env.pending = 0 then
        begin
          env.signaled <- true ;
          List.iter
            (fun (prv,res) -> env.result wpo prv res)
            (Wpo.get_results wpo) ;
          env.success wpo (if proved then Some Prover.Tactical else None)
        end

  let goal env = function
    | Some n -> ProofEngine.goal n
    | None -> ProofEngine.main env.tree

  let prove env wpo ?config prover =
    ProverTask.prove wpo ?config ~mode:Prover.InteractiveMode.Batch
      ~progress:env.progress prover

  let backtracking env =
    match ProofEngine.status env.tree with
    | `Unproved | `Invalid | `Proved | `Passed -> 0
    | `Pending n | `StillResist n -> n

  let setup_backtrack env node depth =
    if env.backtrack > 0 then
      let is_nearer = match env.backtracking with
        | None -> true
        | Some { bk_depth } -> depth < bk_depth in
      if is_nearer then
        let _,hs = ProofEngine.get_strategies node in
        if Array.length hs > 1 then
          env.backtracking <- Some {
              bk_node = node ;
              bk_best = (-1) ;
              bk_depth = depth ;
              bk_pending = backtracking env ;
            }

  let search env node ~depth =
    if env.auto <> [] && depth < env.depth && backtracking env < env.width
    then
      match ProverSearch.search env.tree ~anchor:node env.auto with
      | None -> None
      | Some _ as fork -> setup_backtrack env node depth ; fork
    else None

  let backtrack env =
    if env.backtrack <= 0 then None else
      match env.backtracking with
      | None -> None
      | Some point ->
        let n = backtracking env in
        let anchor = point.bk_node in
        if n < point.bk_pending then
          begin
            point.bk_best <- fst (ProofEngine.get_strategies anchor) ;
            point.bk_pending <- n ;
          end ;
        match ProverSearch.backtrack env.tree ~anchor ~loop:false () with
        | Some fork ->
          env.backtracking <- None ; Some (point.bk_depth,fork)
        | None -> (* end of backtrack *)
          env.backtracking <- None ;
          match ProverSearch.index env.tree ~anchor ~index:point.bk_best
          with None -> None | Some fork -> Some (point.bk_depth,fork)

  let provers env = env.provers

  let make tree
      ~valid ~failed ~provers ~strategies
      ~depth ~width ~backtrack ~auto
      ~progress ~result ~success =
    { tree ; valid ; failed ; provers ; pending = 0 ;
      depth ; width ; backtrack ; auto ; strategies ;
      progress ; result ; success ;
      backtracking = None ;
      signaled = false }

end

(* -------------------------------------------------------------------------- *)
(* --- Choosing Alternatives                                              --- *)
(* -------------------------------------------------------------------------- *)

let fst_order _ _ = 0
let key_order (a,_) (b,_) = String.compare a b

let rec zip order nodes scripts =
  match nodes , scripts with
  | _ , [] | [] , _ -> (*TODO: saveback forgiven scripts *) ()
  | node :: o_nodes , script :: o_scripts ->
    let cmp = order node script in
    if cmp < 0 then zip order o_nodes scripts else
    if cmp > 0 then zip order nodes o_scripts else
      (ProofEngine.bind (snd node) (snd script) ;
       zip order o_nodes o_scripts)

let reconcile nodes scripts =
  match nodes , scripts with
  | [] , [] -> ()
  | [_,n] , [_,s] -> ProofEngine.bind n s
  | _ ->
    if List.for_all (fun (k,_) -> k = "") scripts
    then zip fst_order nodes scripts
    else zip key_order
        (List.stable_sort key_order nodes)
        (List.stable_sort key_order scripts)

let rec forall phi = function
  | x::xs ->
    phi x >>= fun ok ->
    if ok then forall phi xs else Task.return false
  | [] -> Task.return true

let rec exists phi = function
  | x::xs ->
    phi x >>= fun ok ->
    if ok then Task.return true else exists phi xs
  | [] -> Task.return false

let prove_node env node prv =
  let wpo = Env.goal env (Some node) in
  if not (VCS.is_verdict (Wpo.get_result wpo prv)) then
    Env.prove env wpo prv
  else Task.return false

(* -------------------------------------------------------------------------- *)
(* --- Auto & Search Mode (DEPRECATED)                                    --- *)
(* -------------------------------------------------------------------------- *)

let rec auto env ?(depth=0) node : bool Task.task =
  exists (prove_node env node) (Env.provers env) >>= fun ok ->
  if ok then Task.return true else
  if depth > 0 then
    autosearch env ~depth node
  else
    begin
      autosearch env ~depth node >>= fun ok ->
      if ok then Task.return true else
        match Env.backtrack env with
        | Some (depth,fork) ->
          Env.progress env "Backtracking" ;
          autofork env ~depth fork
        | None ->
          Task.return false
    end

and autosearch env ~depth node : bool Task.task =
  match Env.search env node ~depth with
  | None -> Task.return false
  | Some fork -> autofork env ~depth fork

and autofork env ~depth fork =
  let _,children = ProofEngine.commit fork in
  let pending = Env.backtracking env in
  if pending > 0 then
    begin
      Env.progress env (Printf.sprintf "Auto %d" pending) ;
      let depth = succ depth in
      forall (auto env ~depth) (List.map snd children)
    end
  else
    Task.return true

(* -------------------------------------------------------------------------- *)
(* --- Proof Strategy Alternatives                                        --- *)
(* -------------------------------------------------------------------------- *)

let dkey_strategy = Wp_parameters.register_category "strategy"

type solver = ProofEngine.node -> bool Task.task

let success = Task.return true
let failed = Task.return false
let unknown : solver = fun _ -> failed

let (+>>) (a : solver) (b : solver) : solver =
  fun node -> a node >>= fun ok -> if ok then success else b node

let progress a b =
  let sa = ProofEngine.goal a |> Wpo.compute |> snd in
  let sb = ProofEngine.goal b |> Wpo.compute |> snd in
  not (Conditions.equal sa sb)

let rec sequence (f : 'a -> solver) = function
  | [] -> unknown
  | x::xs -> f x +>> sequence f xs

let pp_node fmt node =
  Format.pp_print_string fmt (ProofEngine.goal node).Wpo.po_gid

let rec explore_strategy env process strategy : solver =
  fun node ->
  if ProofEngine.depth node > env.Env.depth then
    begin
      Wp_parameters.debug ~dkey:dkey_strategy "[%a] Depth limit reached (%d)"
        pp_node node (ProofEngine.depth node) ;
      failed
    end
  else
    begin
      Wp_parameters.debug ~dkey:dkey_strategy "[%a] Strategy %s: enter@."
        pp_node node (ProofStrategy.name strategy) ;
      sequence
        (explore_alternative env process strategy)
        (ProofStrategy.alternatives strategy) node
      >>= fun ok ->
      Wp_parameters.debug ~dkey:dkey_strategy "[%a] Strategy %s: %s@."
        pp_node node (ProofStrategy.name strategy)
        (if ok then "proved" else "failed");
      Task.return ok
    end

and explore_alternative env process strategy alternative : solver =
  explore_provers env alternative +>>
  explore_tactic env process strategy alternative +>>
  explore_auto env process alternative +>>
  explore_fallback env process alternative

and explore_provers env alternative : solver =
  let provers,timeout =
    ProofStrategy.provers ~default:env.Env.provers alternative in
  sequence (explore_prover env timeout) provers

and explore_prover env timeout prover node =
  let wpo = ProofEngine.goal node in
  let result = Cache.promote ~timeout @@ Wpo.get_result wpo prover in
  if VCS.is_valid result then success else
  if VCS.is_verdict result then failed else
    let config = { VCS.default with timeout = Some timeout } in
    Env.prove env wpo ~config prover

and explore_tactic env process strategy alternative node =
  Wp_parameters.debug ~dkey:dkey_strategy
    "@[<hov 2>[%a] Trying@ %a@]"
    pp_node node
    ProofStrategy.pp_alternative alternative ;
  match ProofStrategy.tactic env.tree node strategy alternative with
  | None -> failed
  | Some [node'] when not (progress node node') ->
    Wp_parameters.debug ~dkey:dkey_strategy
      "@[<hov 2>[%a] tactic has made no progress"
      pp_node node ;
    failed
  | Some nodes ->
    Wp_parameters.debug ~dkey:dkey_strategy
      "@[<hov 2>[%a] success (%d children) (at depth %d/%d)"
      pp_node node (List.length nodes)
      (ProofEngine.depth node) env.depth;
    List.iter process nodes ; success

and explore_auto env process alternative node =
  match ProofStrategy.auto alternative with
  | None -> failed
  | Some h ->
    match ProverSearch.search env.tree ~anchor:node [h] with
    | None -> failed
    | Some fork ->
      List.iter (fun (_,node) -> process node) @@
      snd @@ ProofEngine.commit fork ; success

and explore_fallback env process alternative node =
  match ProofStrategy.fallback alternative with
  | None -> failed
  | Some strategy -> explore_strategy env process strategy node

let explore_local_hint env process node =
  match ProofEngine.get_hint node with
  | None -> failed
  | Some s ->
    match ProofStrategy.find s with
    | None -> failed
    | Some s -> explore_strategy env process s node

let explore_further env process strategy node =
  let marked =
    match ProofEngine.get_hint node with
    | None -> false
    | Some s -> ProofStrategy.name strategy = s
  in if marked then failed else explore_strategy env process strategy node

let explore_further_hints env process =
  let wpo = ProofEngine.main env.Env.tree in
  sequence (explore_further env process) (ProofStrategy.hints wpo)

let explore_hints env process =
  explore_local_hint env process +>> explore_further_hints env process

(* -------------------------------------------------------------------------- *)
(* --- Automated Solving                                                  --- *)
(* -------------------------------------------------------------------------- *)

let automated env process : solver =
  auto env +>> if env.Env.strategies then explore_hints env process else unknown

(* -------------------------------------------------------------------------- *)
(* --- Apply Script Tactic                                                --- *)
(* -------------------------------------------------------------------------- *)

let apply env node jtactic subscripts =
  match jfork (Env.tree env) ?node jtactic with
  | None -> failwith "Selector not found"
  | Some fork ->
    let _,children = ProofEngine.commit fork in
    reconcile children subscripts ; (*TODO: saveback forgiven script ? *)
    let ok =
      List.for_all
        (fun (_,node) -> ProofEngine.locally_proved node) children in
    if ok then [] else children

(* -------------------------------------------------------------------------- *)
(* --- Script Crawling                                                    --- *)
(* -------------------------------------------------------------------------- *)

let rec crawl env on_child node = function

  | [] ->
    let node = ProofEngine.anchor (Env.tree env) ?node () in
    automated env on_child node

  | Error(msg,json) :: alternatives ->
    Wp_parameters.warning "@[<hov 2>Script Error: on goal %a@\n%S: %a@]@."
      WpPropId.pretty (Env.goal env node).po_pid
      msg Json.pp json ;
    crawl env on_child node alternatives

  | Prover( prv , res ) :: alternatives ->
    begin
      let task =
        if Env.play env prv res then
          let wpo = Env.goal env node in
          let config = VCS.configure res in
          Env.prove env wpo ~config prv
        else Task.return false in
      let continue ok =
        if ok
        then success
        else crawl env on_child node alternatives
      in
      task >>= continue
    end

  | Tactic( _ , jtactic , subscripts ) :: alternatives ->
    begin
      try
        let residual = apply env node jtactic subscripts in
        List.iter (fun (_,n) -> on_child n) residual ;
        Task.return true
      with exn when Wp_parameters.protect exn ->
        Wp_parameters.warning
          "Script Error: on goal %a@\n\
           can not apply '%s'@\n\
           exception %S@\n\
           @[<hov 2>Params: %a@]@\n\
           @[<hov 2>Select: %a@]@."
          WpPropId.pretty (Env.goal env node).po_pid
          jtactic.tactic
          (Printexc.to_string exn)
          Json.pp jtactic.params
          Json.pp jtactic.select ;
        crawl env on_child node alternatives
    end

(* -------------------------------------------------------------------------- *)
(* --- Main Process                                                       --- *)
(* -------------------------------------------------------------------------- *)

let pp_subgoal env fmt node =
  let main = Env.goal env None in
  let wpo = Env.goal env (Some node) in
  Format.fprintf fmt "%s subgoal:@\n%a" (Wpo.get_gid main) Wpo.pp_goal_flow wpo

let schedule job =
  Task.spawn (ProverTask.server ()) (Task.thread (Task.todo job))

let rec process env node =
  env.Env.pending <- succ env.Env.pending ;
  schedule
    begin fun () ->
      Wp_parameters.debug ~dkey:dkey_pp_allgoals "%a" (pp_subgoal env) node ;
      if ProofEngine.locally_proved node then
        begin
          env.pending <- pred env.pending ;
          Env.validate env ;
          Task.return () ;
        end
      else
        let script = Priority.sort (ProofEngine.bound node) in
        crawl env (process env) (Some node) script >>=
        begin fun _ ->
          env.pending <- pred env.pending ;
          Env.validate env ;
          Task.return ()
        end
    end

let task
    ~valid ~failed ~provers
    ~depth ~width ~backtrack ~auto ~scratch ~strategies
    ~start ~progress ~result ~success wpo =
  begin fun () ->
    Wp_parameters.debug ~dkey:dkey_pp_allgoals "%a" Wpo.pp_goal_flow wpo ;
    ProverTask.simplify ~start ~result wpo >>= fun succeed ->
    if succeed
    then
      ( success wpo (Some Prover.Qed) ; Task.return ())
    else
      let script =
        if scratch then [] else
          Priority.sort @@ ProofScript.decode @@ ProofSession.load wpo
      in
      let tree = ProofEngine.proof ~main:wpo in
      let env = Env.make tree
          ~valid ~failed ~provers
          ~depth ~width ~backtrack ~auto ~strategies
          ~progress ~result ~success in
      crawl env (process env) None script >>= fun _ ->
      Env.validate env ;
      ProofEngine.forward tree ;
      Task.return ()
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Entry Points                                                  --- *)
(* -------------------------------------------------------------------------- *)

type 'a process =
  ?valid:bool -> ?failed:bool -> ?scratch:bool -> ?provers:Prover.t list ->
  ?depth:int -> ?width:int -> ?backtrack:int ->
  ?auto:Strategy.heuristic list ->
  ?strategies:bool ->
  ?start:(Wpo.t -> unit) ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> Prover.t -> VCS.result -> unit) ->
  ?success:(Wpo.t -> Prover.t option -> unit) ->
  Wpo.t -> 'a

let skip1 _ = ()
let skip2 _ _ = ()
let skip3 _ _ _ = ()

let prove
    ?(valid = true) ?(failed = true) ?(scratch = false) ?(provers = [])
    ?(depth = 0) ?(width = 0) ?(backtrack = 0) ?(auto = [])
    ?(strategies = false)
    ?(start = skip1) ?(progress = skip2) ?(result = skip3) ?(success = skip2)
    wpo =
  Task.todo (task
               ~valid ~failed ~provers
               ~depth ~width ~backtrack ~auto ~scratch ~strategies
               ~start ~progress ~result ~success wpo)

let spawn
    ?(valid = true) ?(failed = true) ?(scratch = false) ?(provers = [])
    ?(depth = 0) ?(width = 0) ?(backtrack = 0) ?(auto = [])
    ?(strategies = false)
    ?(start = skip1) ?(progress = skip2) ?(result = skip3) ?(success = skip2)
    wpo =
  schedule (task
              ~valid ~failed ~provers
              ~depth ~width ~backtrack ~auto ~scratch ~strategies
              ~start ~progress ~result ~success wpo)

let search
    ?(depth = 0) ?(width = 0) ?(backtrack = 0) ?(auto = []) ?(provers = [])
    ?(progress = skip2) ?(result = skip3) ?(success = skip2)
    tree node =
  begin
    let env = Env.make tree
        ~valid:false ~failed:false ~provers
        ~depth ~width ~backtrack ~auto ~strategies:false
        ~progress ~result ~success in
    schedule
      begin fun () ->
        autosearch env ~depth:0 node >>= fun _ ->
        Env.validate env ;
        Task.return ()
      end
  end

let explore ?(depth=0) ?(strategy)
    ?(progress = skip2) ?(result=skip3) ?(success = skip2)
    tree node =
  begin
    let depth = ProofEngine.depth node + depth in
    let env : Env.t =
      Env.make tree ~valid:false ~failed:false
        ~strategies:(strategy <> None)
        ~provers:[] ~depth ~width:0 ~backtrack:0 ~auto:[]
        ~progress ~result ~success in
    schedule
      begin fun () ->
        let solver =
          match strategy with
          | None -> explore_hints env (process env)
          | Some s -> explore_strategy env (fun _ -> ()) s
        in solver node >>= fun _ ->
        Env.validate env ;
        Task.return ()
      end
  end

(* -------------------------------------------------------------------------- *)
(* --- Save Session                                                       --- *)
(* -------------------------------------------------------------------------- *)

let proofs = Hashtbl.create 32

let has_proof wpo =
  let wid = wpo.Wpo.po_gid in
  try Hashtbl.find proofs wid
  with Not_found ->
    if ProofSession.exists wpo then
      let ok =
        try
          let script = ProofScript.decode (ProofSession.load wpo) in
          ProofScript.has_proof script
        with _ -> false in
      (Hashtbl.add proofs wid ok ; ok)
    else false

let get wpo =
  match ProofEngine.get wpo with
  | `None -> `None
  | `Proof -> `Proof
  | `Saved -> `Saved
  | `Script -> if has_proof wpo then `Script else `Proof

(* -------------------------------------------------------------------------- *)
