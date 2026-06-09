(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Interactive Proof Engine                                           --- *)
(* -------------------------------------------------------------------------- *)

type node = {
  tree : Wpo.t ; (* root, to check consistency *)
  goal : Wpo.t ; (* only GoalAnnot of a sequent *)
  child : string option ; (* child name from parent node *)
  parent : node option ;
  mutable script : script ;
  mutable stats : Stats.stats option ; (* memoized *)
  mutable strategy : string option ; (* hint *)
  mutable search_index : int ;
  mutable search_space : Strategy.t array ; (* sorted by priority *)
}

and script =
  | Opened
  | Script of ProofScript.jscript (* to replay *)
  | Tactic of ProofScript.jtactic * (string * node) list (* played *)

module Node =
struct
  type t = node
  let hash t = Wpo.S.hash t.goal
  let equal a b = Wpo.S.equal a.goal b.goal
  let compare a b = Wpo.S.compare a.goal b.goal
  let pretty fmt a = Wpo.S.pretty fmt a.goal
end

type tree = {
  main : Wpo.t ; (* Main goal to be proved. *)
  mutable pool : Lang.F.pool option ; (* Global pool variable *)
  mutable saved : bool ; (* Saved on Disk. *)
  mutable dirty : bool ; (* Tactical result is outdated *)
  mutable gid : int ; (* WPO goal numbering *)
  mutable head : node option ; (* the current node *)
  mutable root : node option ; (* the root node *)
}

module PROOFS = WpContext.StaticGenerator(Wpo.S)
    (struct
      type key = Wpo.S.t
      type data = tree
      let name = "Wp.ProofEngine.Proofs"
      let compile main =
        ignore (Wpo.resolve main) ;
        {
          main ; gid = 0 ;
          pool = None ;
          head = None ;
          root = None ;
          dirty = true ;
          saved = false ;
        }
    end)

module NODES = WpContext.Static
    (struct
      type key = Wpo.t
      type data = node
      let name = "Wp.ProofEngine.Nodes"
      let compare = Wpo.S.compare
      let pretty = Wpo.S.pretty
    end)

(* -------------------------------------------------------------------------- *)
(* --- Signaling                                                          --- *)
(* -------------------------------------------------------------------------- *)

let goal_hooks = ref []
let clear_hooks = ref []
let remove_hooks = ref []
let update_hooks = ref []

let add_goal_hook fn = goal_hooks := !goal_hooks @ [fn]
let add_clear_hook fn = clear_hooks := !clear_hooks @ [fn]
let add_remove_hook fn = remove_hooks := !remove_hooks @ [fn]
let add_update_hook fn = update_hooks := !update_hooks @ [fn]

let signal_goal g = List.iter (fun fn -> fn g) !goal_hooks
let signal_node n = List.iter (fun fn -> fn n) !update_hooks

let dirty_root g =
  try
    let tree = PROOFS.find g in
    if not tree.dirty then
      begin
        tree.dirty <- true ;
        signal_goal g ;
      end
  with Not_found -> ()

let rec dirty_node n =
  match n.stats with
  | None -> ()
  | Some _ ->
    n.stats <- None ;
    signal_node n ;
    match n.parent with
    | Some p -> dirty_node p
    | None -> dirty_root n.tree

let self_updating = ref false

let dirty_goal g =
  if not !self_updating then
    match NODES.get g with
    | Some n -> dirty_node n
    | None -> dirty_root g

let get wpo =
  try
    let proof = PROOFS.find wpo in
    match proof.root with
    | None | Some { script = Opened | Script _ } -> raise Not_found
    | Some { script = Tactic _ } -> if proof.saved then `Saved else `Proof
  with Not_found ->
    if ProofSession.exists wpo then `Script else `None

let iter_children f ns = List.iter (fun m -> f (snd m)) ns
let map_children f ns = List.map (fun m -> fst m,f m) ns

let pool tree =
  match tree.pool with
  | Some pool -> pool
  | None ->
    let _,sequent = Wpo.compute tree.main in
    let pool = Lang.new_pool ~vars:(Conditions.vars_seq sequent) () in
    tree.pool <- Some pool ; pool

(* -------------------------------------------------------------------------- *)
(* --- Proofs                                                             --- *)
(* -------------------------------------------------------------------------- *)

let proof ~main =
  assert (not (Wpo.is_tactic main)) ;
  PROOFS.get main

let saved t = t.saved
let set_saved t s = t.saved <- s ; signal_goal t.main

(* -------------------------------------------------------------------------- *)
(* --- Removal                                                            --- *)
(* -------------------------------------------------------------------------- *)

let rec revert_tactic t n =
  n.strategy <- None ;
  match n.script with
  | Opened | Script _ -> ()
  | Tactic(_,children) ->
    t.saved <- false ;
    n.script <- Opened ;
    iter_children (remove_node t) children

and remove_node t n =
  NODES.remove n.goal ;
  if Wpo.is_tactic n.goal then
    Wpo.remove n.goal
  else
    Wpo.clear_results n.goal ;
  List.iter (fun f -> f n) !remove_hooks ;
  Option.iter (fun n0 -> if n0 == n then t.head <- None) t.head ;
  revert_tactic t n

let clear_tree t =
  begin
    Option.iter (remove_node t) t.root ;
    t.gid <- 0 ;
    t.head <- None ;
    t.root <- None ;
    t.saved <- false ;
    List.iter (fun fn -> fn t.main) !clear_hooks ;
    signal_goal t.main ;
  end

let clear_node_tactic t n =
  revert_tactic t n ; dirty_node n ;
  if t.head = None then t.head <- Some n

let clear_parent_tactic t n =
  match n.parent with
  | None -> clear_tree t
  | Some p as h -> revert_tactic t p ; dirty_node p ; t.head <- h

let clear_node t n =
  Wpo.clear_results n.goal ;
  clear_node_tactic t n

let clear_goal w =
  try clear_tree (PROOFS.find w)
  with Not_found -> Wpo.clear_results w

(* -------------------------------------------------------------------------- *)
(* --- Removal From Wpo                                                   --- *)
(* -------------------------------------------------------------------------- *)

let removed_from_wpo g =
  if not @@ Wpo.is_tactic g then
    begin
      try
        clear_tree (PROOFS.find g) ;
        PROOFS.remove g ;
      with Not_found ->
        signal_goal g
    end

let cleared_from_wpo () =
  begin
    NODES.clear () ;
    PROOFS.clear () ;
  end

let () = Wpo.add_removed_hook removed_from_wpo
let () = Wpo.add_cleared_hook cleared_from_wpo
let () = Wpo.add_modified_hook dirty_goal

(* -------------------------------------------------------------------------- *)
(* --- Walking                                                            --- *)
(* -------------------------------------------------------------------------- *)

let rec walk f node =
  if not (Wpo.is_locally_valid node.goal) then
    match node.script with
    | Tactic (_,children) -> iter_children (walk f) children
    | Opened | Script _ -> f node

let iteri f tree =
  match tree.root with
  | None -> ()
  | Some r ->
    let k = ref 0 in
    walk (fun node -> f !k node ; incr k) r

let rec depth node =
  match node.parent with
  | None -> 1
  | Some p -> succ @@ depth p

(* -------------------------------------------------------------------------- *)
(* --- Consolidating                                                      --- *)
(* -------------------------------------------------------------------------- *)

let pending n =
  let k = ref 0 in
  walk (fun _ -> incr k) n ; !k

let locally_proved n = Wpo.is_locally_valid n.goal

let fully_proved n =
  let exception HasPending in
  try walk (fun _ -> raise HasPending) n ; true
  with HasPending -> false

let is_prover (p,_) = not @@ Prover.is_tactical p

let prover_stats ~smoke goal =
  Stats.results ~smoke @@
  List.filter is_prover @@
  Wpo.get_results goal

let rec consolidate ~smoke n =
  match n.stats with
  | Some s -> s
  | None -> let s = compute ~smoke n in n.stats <- Some s ; s

and compute ~smoke n =
  if Wpo.is_locally_valid n.goal then
    prover_stats ~smoke n.goal
  else
    match n.script with
    | Opened | Script _ -> prover_stats ~smoke n.goal
    | Tactic(_,children) ->
      Stats.tactical ~qed:(Wpo.qed_time n.goal) @@
      List.map (fun (_,n) -> consolidate ~smoke n) children

let tactical tree =
  match tree.root with
  | None -> VCS.no_result
  | Some root ->
    let smoke = Wpo.is_smoke_test tree.main in
    Stats.script @@ consolidate ~smoke root

let validate tree =
  if tree.dirty then
    let result =
      if Wpo.is_locally_valid tree.main then VCS.no_result else tactical tree
    in
    try
      self_updating := true ;
      Wpo.set_result tree.main Prover.Tactical result ;
      tree.dirty <- false ;
      self_updating := false ;
    with exn ->
      self_updating := false ;
      raise exn

let consolidated wpo =
  let smoke = Wpo.is_smoke_test wpo in
  try
    match (PROOFS.find wpo).root with
    | Some node -> consolidate ~smoke node
    | _ -> raise Not_found
  with Not_found -> prover_stats ~smoke wpo

let consolidate wpo =
  try validate (PROOFS.find wpo) with Not_found -> ()

let results wpo = consolidate wpo ; Wpo.get_results wpo

let stats node =
  match node.stats with Some s -> s | None ->
    let smoke = Wpo.is_smoke_test node.tree in
    let s = compute ~smoke node in
    node.stats <- Some s ; s

(* -------------------------------------------------------------------------- *)
(* --- Accessors                                                          --- *)
(* -------------------------------------------------------------------------- *)

let main t = t.main
let head t = t.head
let head_goal t = match t.head with
  | None -> t.main
  | Some n -> n.goal
let tree n = proof ~main:n.tree
let goal n = n.goal

let tree_context t = Wpo.get_context t.main
let node_context n = Wpo.get_context n.goal
let parent n = n.parent
let title n = n.goal.Wpo.po_name

let tactical n =
  match n.script with
  | Tactic(tactic,_) -> Some tactic
  | Opened | Script _ -> None

let get_strategies n = n.search_index , n.search_space
let set_strategies n ?(index=0) hs =
  n.search_index <- index ; n.search_space <- hs ; signal_node n

let children n =
  match n.script with
  | Tactic(_,children) -> children
  | Opened | Script _ -> []

let subgoals n = List.map snd @@ children n

let rec path n = match n.parent with
  | None -> []
  | Some p -> p::path p

let child_label n = n.child

let tactic_label n =
  match n.script with Tactic({ header }, _)  -> Some header | _ -> None

let tactic n =
  match n.script with
  | Tactic({ tactic }, _)  ->
    begin try Some (Tactical.lookup ~id:tactic) with Not_found -> None end
  | _ -> None

(* -------------------------------------------------------------------------- *)
(* --- State & Status                                                     --- *)
(* -------------------------------------------------------------------------- *)

type status = [
  | `Unproved (* proof obligation not proved *)
  | `Proved   (* proof obligation is proved *)
  | `Pending of int (* proof is pending *)
  | `Passed   (* smoke test is passed (PO is not proved) *)
  | `Invalid  (* smoke test has failed (PO is proved) *)
  | `StillResist of int (* proof is pending *)
]

let status t : status =
  match t.root with
  | None ->
    if Wpo.is_locally_valid t.main
    then if Wpo.is_smoke_test t.main then `Invalid else `Proved
    else if Wpo.is_smoke_test t.main then `Passed else `Unproved
  | Some root ->
    match root.script with
    | Opened | Script _ ->
      if Wpo.is_smoke_test t.main then `Passed else `Unproved
    | Tactic _ ->
      let n = pending root in
      if n = 0 then
        if Wpo.is_smoke_test t.main then `Invalid else `Proved
      else
      if Wpo.is_smoke_test t.main then `StillResist n else `Pending n

(* -------------------------------------------------------------------------- *)
(* --- Navigation                                                         --- *)
(* -------------------------------------------------------------------------- *)

type current = [ `Main | `Internal of node | `Leaf of int * node ]

let current t : current =
  match t.head with
  | Some h ->
    let p = ref (`Internal h) in
    iteri (fun i n -> if n == h then p := `Leaf(i,n)) t ; !p
  | None -> `Main

type position = [ `Main | `Node of node | `Leaf of int ]
let goto t = function
  | `Main ->
    t.head <- t.root ; signal_goal t.main
  | `Node n ->
    if n.tree == t.main then t.head <- Some n ; signal_goal t.main
  | `Leaf k ->
    t.head <- t.root ;
    iteri (fun i n -> if i = k then t.head <- Some n) t ;
    signal_goal t.main

let fetch t node =
  try
    t.head <- t.root ;
    walk (fun n -> t.head <- Some n ; raise Exit) node ;
    false
  with Exit -> true

let rec forward t =
  match t.head with
  | None -> t.head <- t.root ; signal_goal t.main
  | Some hd ->
    if not (fetch t hd)
    then ( t.head <- hd.parent ; forward t )
    else signal_goal t.main

let cancel_parent_tactic t =
  match t.head with
  | None -> ()
  | Some n -> clear_parent_tactic t n

let cancel_current_node t =
  match t.head with
  | None -> ()
  | Some node -> clear_node t node

(* -------------------------------------------------------------------------- *)
(* --- Sub-Goal                                                           --- *)
(* -------------------------------------------------------------------------- *)

let mk_formula ~main axioms sequent =
  Wpo.VC_Annot.{ main with goal = Wpo.GOAL.make sequent ; axioms }

let mk_goal t ~title ~part ~axioms sequent =
  let id = t.gid in t.gid <- succ id ;
  let gid = Printf.sprintf "%s-%d" t.main.Wpo.po_gid id in
  let sid = Printf.sprintf "%s-%d" t.main.Wpo.po_sid id in
  Wpo.({
      po_gid = gid ;
      po_sid = sid ;
      po_name = Printf.sprintf "%s (%s)" title part ;
      po_idx = t.main.po_idx ;
      po_pid = WpPropId.tactical ~gid ;
      po_model = t.main.po_model ;
      po_formula = mk_formula ~main:t.main.po_formula axioms sequent ;
    })

let mk_node ~main ?parent ?child goal =
  let node = {
    tree=main ; parent ; child ; goal ;
    script = Opened ;
    stats = None ;
    search_index = 0 ;
    search_space = [| |] ;
    strategy = None ;
  } in NODES.define goal node ; node

let mk_root ~tree =
  let main = tree.main in
  let node = mk_node ~main main in
  let root = Some node in
  tree.root <- root ;
  tree.head <- root ;
  node

let root tree =
  match tree.root with
  | Some node -> node
  | None -> mk_root ~tree

(* -------------------------------------------------------------------------- *)
(* --- Forking                                                            --- *)
(* -------------------------------------------------------------------------- *)

module Fork =
struct
  type t = {
    tree : tree ;
    anchor : node ;
    tactic : ProofScript.jtactic ;
    goals : (string * Wpo.t) list ;
  }

  let create tree ~anchor tactic process =
    let axioms , sequent = Wpo.compute anchor.goal in
    let vars = Conditions.vars_seq sequent in
    let dseqs = Lang.local ~vars process sequent in
    let title = tactic.ProofScript.header in
    let goals = List.map
        (fun (part,s) -> part , mk_goal tree ~title ~part ~axioms s) dseqs
    in { tree ; tactic ; anchor ; goals }

  let iter f w = iter_children f w.goals

  let header frk = frk.tactic.ProofScript.header
end

let pretty fmt frk = Format.pp_print_string fmt (Fork.header frk)

type fork = Fork.t

let fork = Fork.create
let iter = Fork.iter

let anchor tree ?node () =
  match node with
  | Some n -> n
  | None ->
    match tree.head with
    | Some n -> n
    | None ->
      match tree.root with
      | Some n -> n
      | None -> mk_root ~tree

let commit fork =
  List.iter (fun (_,wp) -> ignore (Wpo.resolve wp)) fork.Fork.goals ;
  let tree = fork.Fork.tree in
  let parent = fork.Fork.anchor in
  let subnode (child,goal) = mk_node ~main:tree.main ~parent ~child goal in
  let children = map_children subnode fork.Fork.goals in
  tree.saved <- false ;
  parent.script <- Tactic( fork.Fork.tactic , children ) ;
  dirty_node parent ;
  parent , children

(* -------------------------------------------------------------------------- *)
(* --- Scripting                                                          --- *)
(* -------------------------------------------------------------------------- *)

let script_provers wpo =
  List.map (fun (p,r) -> ProofScript.a_prover p r) @@
  Wpo.get_prover_results wpo

let rec script_node (node : node) =
  script_provers node.goal @
  begin
    match node.script with
    | Script s -> List.filter ProofScript.is_tactic s
    | Tactic( tactic , children ) ->
      [ ProofScript.a_tactic tactic (List.map subscript_node children) ]
    | Opened -> []
  end

and subscript_node (key,node) = key , script_node node

let script tree =
  match tree.root with
  | None -> script_provers tree.main
  | Some node -> script_node node

let bind node script =
  match node.script with
  | Tactic _ ->
    (*TODO: saveback the thrown script *)
    ()
  | Opened | Script _ ->
    (*TODO: saveback the previous script *)
    node.script <- Script script ;
    signal_node node ;
    signal_goal node.tree

let bound node =
  match node.script with
  | Tactic _ | Opened -> []
  | Script s -> s

let is_script_result ~margin (prv,res) =
  Prover.is_extern prv && VCS.is_valid res &&
  res.prover_time > margin

let has_result tree =
  let margin = float_of_string @@ Wp_parameters.TimeMargin.get () in
  let results = Wpo.get_results tree.main in
  List.exists (is_script_result ~margin) results

let has_tactics tree =
  match tree.root with
  | None -> false
  | Some node ->
    match node.script with
    | Opened -> false
    | Tactic _ -> true
    | Script s -> List.exists ProofScript.is_tactic s

let has_script tree = has_tactics tree || has_result tree
let has_proof wpo =
  try has_script @@ PROOFS.find wpo
  with Not_found -> false

let get_hint node = node.strategy
let set_hint node strategy = node.strategy <- Some strategy ; signal_node node

(* -------------------------------------------------------------------------- *)
