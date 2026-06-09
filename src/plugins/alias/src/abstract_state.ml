(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

open Simplified

module EdgeLabel = struct
  type t =
    | Pointer (* represents dereferentiation of pointers as well as arrays *)
    | Field of fieldinfo
  let compare l r = match l, r with
    | Pointer, Pointer -> 0
    | Pointer, Field _ -> -1
    | Field lv, Field rv -> Fieldinfo.compare lv rv
    | Field _, Pointer -> 1
  let equals l r = compare l r = 0
  let default = Pointer
  let is_pointer = function Pointer -> true | _ -> false
  let is_field = function Field _ -> true | _ -> false
  let pretty fmt = function
    | Pointer -> ()
    | Field f -> Format.fprintf fmt "-%s" f.fname
end

module G = struct
  include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Datatype.Int) (EdgeLabel)

  let psucc g v =
    let only_pointer_succ e =
      if EdgeLabel.is_pointer (E.label e) then Some (E.dst e) else None
    in
    List.filter_map only_pointer_succ (succ_e g v)

  let ppred g v =
    let p e = match E.label e with Pointer -> Some (E.src e) | _ -> None in
    List.filter_map p (pred_e g v)

  let fsucc_opt g v f =
    assert (List.for_all (fun e -> EdgeLabel.is_field @@ E.label e) @@ succ_e g v);
    let is_field_f e = match E.label e with
      | Field f' -> Fieldinfo.equal f f'
      | _ -> false
    in
    let edges = succ_e g v in
    assert (List.length (List.filter is_field_f edges) <= 1);
    Option.map E.dst @@ List.find_opt is_field_f edges

  let psucc_opt g v = match psucc g v with
    | [] -> None
    | [v] -> Some v
    | _ -> Options.fatal "Invariant violated: more than one successor"

  let check_loop v1 v2 =
    if v1 = v2 then
      Options.warning ~once:true ~wkey:Options.Warn.incoherent
        "loop on vertex %d (following unsafe cast?); analysis may be unsound" v1

  let add_edge g v1 v2 =
    check_loop v1 v2;
    add_edge g v1 v2

  let add_edge_e g ((v1, _, v2) as edge) =
    check_loop v1 v2;
    add_edge_e g edge
end

type v = G.V.t

module V = G.V
module E = struct
  include G.E
  include EdgeLabel
  module Map = Stdlib.Map.Make (struct
      type t = EdgeLabel.t
      let compare = EdgeLabel.compare
    end)
end

module VMap = Datatype.Int.Map
module VSet = Datatype.Int.Set
module LSet = Cil_datatype.LvalStructEq.Set
module VarSet = Cil_datatype.Varinfo.Set

module VarMap = struct
  include Cil_datatype.Varinfo.Map

  let intersect = merge @@ fun _ l r -> match l,r with
    | Some l, Some r -> Some (l,r)
    | _ -> None

  let pretty ?format ?item ?sep ?last ?empty =
    pretty ?format ?item ?sep ?last ?empty Varinfo.pretty Datatype.Int.pretty
end

type state =
  {graph : G.t;
   vmap : VarSet.t VMap.t; (* associate with each node a set of variables *)
   varmap : V.t VarMap.t (* reverse of varmap *)}

let node_counter = ref 0
let fresh_node_id () =
  let id = !node_counter in
  node_counter := !node_counter + 1;
  id

let get_vars v s : VarSet.t =
  try VMap.find v s.vmap with Not_found -> VarSet.empty

(* raises Not_found *)
let rec find_lval_vertex ((lhost, offset) : lval) s : V.t =
  let find_psucc v = match G.psucc_opt s.graph v with Some v -> v | _ -> raise Not_found in
  let find_fsucc v fname = match G.fsucc_opt s.graph v fname with Some v -> v | _ -> raise Not_found in
  let find_lhost = function
    | Var var -> VarMap.find var s.varmap
    | Mem e ->
      match LvalOrRef.from_exp e with
      | None -> Options.fatal "unexpected result: Lval.from (%a) = None" Exp.pretty e
      | Some (LvalOrRef.Ref lv1) -> find_lval_vertex lv1 s
      | Some (LvalOrRef.Lval lv1) ->
        let v1 = find_lval_vertex lv1 s in
        find_psucc v1
  in
  let rec find_offset v = function
    | NoOffset -> v
    | Index (_, o) ->
      let v' = find_psucc v in
      find_offset v' o
    | Field (f, o) ->
      let v' = find_fsucc v f in
      find_offset v' o
  in
  let hv = find_lhost lhost in
  find_offset hv offset


let add_field_and_cast f lv =
  let off = Field(f,NoOffset) in
  match lv with
  | Mem p, NoOffset when Ast_types.is_void_ptr (Cil.typeOf p) ->
    let newt = Cil_const.(mk_tptr (mk_tcomp f.fcomp)) in
    Mem (Cil.mkCast ~newt p), off
  | _ -> Cil.addOffsetLval off lv

module Readout = struct

  (* Reconstruct all lvals that are represented by the given node.
     Nodes only carry varinfos. In order to obtain lvals we recursively walk
     backwards in the graph inductively constructing lvals from scratch.
     - The lvals of the current node are the stored varinfos themselves.
     - After gathering the lvals for the predecessor, the lvals are
       modified according to the edge type used:
       * Pointer: add a star (x → *x)
       * Field f: add an offset (x -> x.f) *)
  let get_lval_set v s : LSet.t =
    assert (G.mem_vertex s.graph v);
    (* cycles can occur with unsafe casts such as: x->f = (int* ) x; *)
    let rec checking_for_cycles s visited v =
      if VSet.mem v visited then
        let () =
          Options.warning ~once:true ~wkey:Options.Warn.incoherent
            "cycle during readout of vertex %d, \
             (following unsafe cast?); analysis may be unsound" v
        in LSet.empty
      else
        let visited = VSet.add v visited in
        let modified_predecessors = List.map
            (fun e ->
               let pred_lvals = checking_for_cycles s visited @@ E.src e in
               let modify_lval lv =
                 match E.label e with
                 | Field f -> add_field_and_cast f lv
                 | Pointer ->
                   let ty = Cil.typeOfLval lv in
                   if Ast_types.is_array ty then
                     Cil.addOffsetLval (Index (Simplified.nul_exp, NoOffset)) lv
                   else
                     let () = if not @@ Ast_types.is_ptr ty then
                         Options.debug "unexpected type: %a" Printer.pp_typ ty
                     in
                     Mem (Cil.dummy_exp @@ Lval lv), NoOffset
               in
               LSet.map modify_lval pred_lvals
            )
            (G.pred_e s.graph v)
        in
        let lvals_of_v =
          let mk_lval var = (Var var), NoOffset in
          LSet.of_seq @@ Seq.map mk_lval @@ VarSet.to_seq @@ get_vars v s
        in
        List.fold_left LSet.union lvals_of_v modified_predecessors
    in
    checking_for_cycles s VSet.empty v

  let lvals_pointing_to_vertex v s : LSet.t =
    assert (G.mem_vertex s.graph v);
    let list_pred = List.map (fun b -> get_lval_set b s) (G.ppred s.graph v) in
    List.fold_left LSet.union LSet.empty list_pred

  let vars_pointing_to_vertex v s : VarSet.t =
    let preds = G.ppred s.graph v in
    let pred_vars = List.map (fun p -> get_vars p s) preds in
    List.fold_left VarSet.union VarSet.empty pred_vars

  let find_vars lv s =
    let lv = Lval.simplify lv in
    try let v = find_lval_vertex lv s in get_vars v s
    with Not_found -> VarSet.empty

  let find_synonyms lv s =
    let lv = Lval.simplify lv in
    try let v = find_lval_vertex lv s in get_lval_set v s
    with Not_found -> LSet.empty

  let alias_vars lv s : VarSet.t =
    try
      let v = find_lval_vertex lv s in
      List.fold_left
        (fun acc succ -> VarSet.union acc @@ vars_pointing_to_vertex succ s)
        VarSet.empty
        (G.psucc s.graph v)
    with Not_found -> VarSet.empty

  let alias_lvals lv s : LSet.t =
    let v_opt = try Some (find_lval_vertex lv s) with Not_found -> None in
    match Option.bind (G.psucc_opt s.graph) v_opt with
    | None -> LSet.empty
    | Some succ -> lvals_pointing_to_vertex succ s

  let points_to_vars lv s : VarSet.t =
    let succ = try G.psucc_opt s.graph @@ find_lval_vertex lv s with Not_found -> None in
    match succ with
    | None -> VarSet.empty
    | Some succ_v -> get_vars succ_v s

  let points_to_lvals lv s : LSet.t =
    let succ = try G.psucc_opt s.graph @@ find_lval_vertex lv s with Not_found -> None in
    match succ with
    | None -> LSet.empty
    | Some succ_v -> get_lval_set succ_v s

  let alias_sets_vars s =
    let alias_set_of_vertex (i, _) =
      let aliases = vars_pointing_to_vertex i s in
      if VarSet.cardinal aliases >= 2 then Some aliases else None
    in
    List.filter_map alias_set_of_vertex @@ VMap.bindings s.vmap

  let alias_sets_lvals s =
    let alias_set_of_vertex (i, _) =
      let aliases = lvals_pointing_to_vertex i s in
      if LSet.cardinal aliases >= 2 then Some aliases else None
    in
    List.filter_map alias_set_of_vertex @@ VMap.bindings s.vmap

end


module Pretty = struct
  let pp_internal fmt s =
    Format.fprintf fmt "@[<v>";
    Format.fprintf fmt "@[Edges:";
    G.iter_edges_e
      (fun e ->
         Format.fprintf fmt "@;<2 8>@[%d@ @[%a%t@]@ %d@]"
           (E.src e) E.pretty (E.label e) Unicode.pp_right_arrow (E.dst e))
      s.graph;
    Format.fprintf fmt "@]@;<6>";
    VarMap.pretty ~format:"@[VarMap:@;<1 2>%t@]@;<6>" ~sep:"@;<2 8>" fmt s.varmap;
    VMap.pretty ~format:"@[VMap:@;<3 2>%t@]" ~item:"@[%a:%a@]" ~sep:"@;<2 8>"
      Datatype.Int.pretty VarSet.pretty fmt s.vmap;
    Format.fprintf fmt "@]"

  let libc_regexp = Str.regexp "^__fc_"

  let vars_filter_libc vars =
    if Options.is_debug_key_enabled Options.DebugKeys.show_libc_vars
    then vars
    else
      let is_from_libc vi = Str.string_match libc_regexp vi.vname 0 in
      VarSet.filter (fun v -> not @@ is_from_libc v) vars

  let lvals_filter_libc lvals =
    if Options.is_debug_key_enabled Options.DebugKeys.show_libc_vars
    then lvals
    else
      let rec is_from_libc = function
        | Var vi, _ -> Str.string_match libc_regexp vi.vname 0
        | Mem {enode = Lval l; _}, _ -> is_from_libc l
        | Mem _, _ -> false
      in
      LSet.filter (fun lval -> not @@ is_from_libc lval) lvals

  let pp_graph fmt s =
    let is_first = ref true in
    let pp_node v fmt vars =
      Format.fprintf fmt "%d:%a" v
        VarSet.pretty (vars_filter_libc vars) in
    let pp_edge e =
      let v1 = E.src e and v2 = E.dst e in
      if !is_first then is_first := false else Format.fprintf fmt "@;<3>";
      Format.fprintf fmt "@[%a@] %a%t @[%a@]"
        (pp_node v1) (VMap.find v1 s.vmap)
        E.pretty (E.label e)
        Unicode.pp_right_arrow
        (pp_node v2) (VMap.find v2 s.vmap)
    in
    let pp_unconnected_vertex v =
      if G.in_degree s.graph v = 0 && G.out_degree s.graph v = 0 then begin
        if !is_first then is_first := false else Format.fprintf fmt "@;<3>";
        pp_node v fmt (VMap.find v s.vmap)
      end
    in
    if G.nb_vertex s.graph = 0
    then Format.fprintf fmt "<empty>"
    else begin
      G.iter_edges_e pp_edge s.graph;
      G.iter_vertex pp_unconnected_vertex s.graph
    end;
    if Options.is_debug_key_enabled Options.DebugKeys.show_internal_state
    then begin
      Format.pp_print_newline fmt ();
      pp_internal fmt s
    end

  let pp_aliases fmt s =
    let alias_sets = List.map lvals_filter_libc @@ Readout.alias_sets_lvals s in
    let alias_sets =
      List.stable_sort LSet.compare @@ (* sort for more robust oracles *)
      List.filter
        (fun lvals -> LSet.cardinal lvals >= 2)
        alias_sets
    in
    let pp_set fmt = Format.fprintf fmt "@[<hov 2>%a@]" LSet.pretty in
    Pretty_utils.pp_list ~empty:"<none>" ~sep:"@;<2>" pp_set fmt alias_sets
end

(* invariants of type t must be true before and after each function call *)
let assert_invariants s : unit =
  (* check that all vertex of the graph have entries in vmap,
     and are integer between 0 and node_counter, and have at most 1 successor *)
  assert (!node_counter >= 0);
  let assert_vertex v =
    Options.debug ~level:5 "checking coherence of vertex %d" v;
    assert (v >= 0);
    assert (v < !node_counter);
    assert (VMap.mem v s.vmap);
    let succ_e = G.succ_e s.graph v in
    let is_pointer_vertex =
      List.exists (fun e -> E.is_pointer @@ E.label e) succ_e
    and is_struct_vertex =
      List.exists (fun e -> E.is_field @@ E.label e) succ_e
    in
    assert (not (is_pointer_vertex && is_struct_vertex));
    assert (not is_pointer_vertex || List.length (G.succ s.graph v) <= 1);
  in
  G.iter_vertex assert_vertex s.graph;
  let assert_edge v1 v2 =
    Options.debug ~level:5
      "checking coherence of edge %d %t %d" v1 Unicode.pp_right_arrow v2;
    assert (G.mem_vertex s.graph v1);
    assert (G.mem_vertex s.graph v2)
  in
  G.iter_edges assert_edge s.graph;
  let assert_varmap (var : varinfo) v =
    assert (G.mem_vertex s.graph v);
    assert (VarSet.mem var (VMap.find v s.vmap))
  in
  VarMap.iter assert_varmap s.varmap;
  let assert_vmap v (ls:VarSet.t) =
    assert (G.mem_vertex s.graph v);
    (* TODO: we removed the invariant because of OSCS*)
    (* if not (VarSet.is_empty ls)
     * then
     *   begin
     *     let lv = VarSet.choose ls in
     *     let is_ptr_lv = Lval.is_pointer lv in
     *     assert (VarSet.for_all (fun x -> Lval.is_pointer x = is_ptr_lv) ls)
     *   end; *)
    assert (VarSet.fold (fun lv acc -> acc && V.equal (VarMap.find lv s.varmap) v) ls true)
  in
  VMap.iter assert_vmap s.vmap

(* Ensure that assert_invariants is not executed if the -noassert flag is supplied. *)
let assert_invariants s =
  try assert (assert_invariants s; true)
  with Assert_failure _ as exn ->
    let bt = Printexc.get_raw_backtrace () in
    Options.debug "incoherent graph:@ @[%a@]" Pretty.pp_graph s;
    Printexc.raise_with_backtrace exn bt

let asserting_invariants s = assert_invariants s; s

let pretty ?(debug = false) fmt s =
  assert_invariants s;
  if debug then Pretty.pp_graph fmt s
  else Pretty.pp_aliases fmt s

(* NOTE on "constant vertex": a constant vertex represents an unnamed
   scalar value (type bottom in steensgaard's paper), or the address
   of a variable. It means that in [vmap], its associated VarSet is
   empty.  By definition, constant vertex cannot be associated to a
   lval in [varmap] *)
let create_empty_vertex s : V.t * state =
  let new_v = fresh_node_id () in
  new_v, {graph = G.add_vertex s.graph new_v;
          varmap = s.varmap;
          vmap = VMap.add new_v VarSet.empty s.vmap}

let create_var_vertex var s =
  assert (not @@ VarMap.mem var s.varmap);
  let v = fresh_node_id () in
  let s = {graph = G.add_vertex s.graph v;
           varmap = VarMap.add var v s.varmap;
           vmap = VMap.add v (VarSet.singleton var) s.vmap} in
  let rec create_typ_vertex s v ty = match ty.tnode with
    | TArray (ty, _) | TPtr ty ->
      (* create more vertices for each level of dereferentiation *)
      let v', s = create_empty_vertex s in
      let s = {s with graph = G.add_edge s.graph v v'} in
      create_typ_vertex s v' ty
    | _ -> s (* until the type becomes scalar *)
  in
  v, create_typ_vertex s v var.vtype

let find_or_create_var_vertex (var : varinfo) s =
  try VarMap.find var s.varmap, s
  with Not_found -> create_var_vertex var s

let rec find_or_create_lval_vertex ((lhost, offset) : lval) s : V.t * state =
  let find_or_create_psucc v s =
    match G.psucc_opt s.graph v with
    | None ->
      let v', s = create_empty_vertex s in
      (* finally add a points-to edge between v and v' *)
      let new_graph = G.add_edge s.graph v v' in
      v', {s with graph = new_graph}
    | Some v' -> v', s
  in
  let find_or_create_fsucc v s f =
    match G.fsucc_opt s.graph v f with
    | None ->
      let v', s = create_empty_vertex s in
      (* finally add a points-to edge between v and v' *)
      let new_graph = G.add_edge_e s.graph @@ E.create v (Field f) v' in
      v', {s with graph = new_graph}
    | Some v' -> v', s
  in
  let find_or_create_lhost s = function
    | Var var -> find_or_create_var_vertex var s
    | Mem e ->
      match LvalOrRef.from_exp e with
      | None -> Options.fatal "unexpected result: Lval.from (%a) = None" Exp.pretty e
      | Some (LvalOrRef.Ref lv1) -> find_or_create_lval_vertex lv1 s
      | Some (LvalOrRef.Lval lv1) ->
        let v1, s = find_or_create_lval_vertex lv1 s in
        find_or_create_psucc v1 s
  in
  let rec find_or_create_offset v s = function
    | NoOffset -> v, s
    | Index (_, o) ->
      let v', s = find_or_create_psucc v s in
      find_or_create_offset v' s o
    | Field (f, o) ->
      let v', s = find_or_create_fsucc v s f in
      find_or_create_offset v' s o
  in
  let hv, s = find_or_create_lhost s lhost in
  let v, s = find_or_create_offset hv s offset in
  Options.debug ~level:5 "graph after find_or_create_lval_vertex @[%a@] (%d):@ %a"
    Printer.pp_lval (lhost, offset) v Pretty.pp_graph s;
  v, s

and find_or_create_ref_vertex lv s : V.t * state =
  let v1, s = find_or_create_lval_vertex lv s in
  let va, s = create_empty_vertex s in
  let s = {s with graph = G.add_edge s.graph va v1} in
  Options.debug ~level:5 "graph after find_or_create_ref_vertex @[%a@] (%d):@ %a"
    LvalOrRef.pretty (LvalOrRef.Ref lv) va Pretty.pp_graph s;
  va, s

and find_or_create_lval_or_ref_vertex (lv : LvalOrRef.t) s : V.t * state =
  match lv with
  | LvalOrRef.Lval lv -> find_or_create_lval_vertex lv s
  | LvalOrRef.Ref lv -> find_or_create_ref_vertex lv s

(* TODO is there a better way to do it ? *)
let find_vertex lv s =
  let lv = Lval.simplify lv in
  let v,x1 = find_or_create_lval_vertex lv s in
  if s == x1
  then v (* if s has not been modified, then the vertex was found, not created *)
  else raise Not_found

(* merge of two vertices; the first vertex carries both sets, the second is
   removed from the graph and from varmap and vmap *)
let merge s v1 v2 =
  if V.equal v1 v2 || not (G.mem_vertex s.graph v1) || not (G.mem_vertex s.graph v2)
  then s
  else
    (* update varmap : every lval in v2 must now be associated with v1 *)
    let new_varmap = VarSet.fold (fun lv2 -> VarMap.add lv2 v1) (get_vars v2 s) s.varmap in
    let new_vmap =
      let new_set = VarSet.union (get_vars v1 s) (get_vars v2 s) in
      VMap.add v1 new_set @@ VMap.remove v2 s.vmap
    in
    let new_graph = (* update the graph *)
      let f_fold_succ e g : G.t =
        G.add_edge_e g @@ E.create v1 (E.label e) (E.dst e)
      and f_fold_pred e g : G.t =
        G.add_edge_e g @@ E.create (E.src e) (E.label e) v1
      in
      let g = s.graph in
      (* add all new edges *)
      let g = G.fold_succ_e f_fold_succ g v2 g in
      let g = G.fold_pred_e f_fold_pred g v2 g in
      G.remove_vertex g v2 (* remove v2 *)
    in
    {graph = new_graph; varmap = new_varmap; vmap = new_vmap}

(* functions join and unify-pointer of steensgaard's paper *)
(* join_without_check may break the invariants *)
let rec join_without_check s v1 v2 : state =
  if V.equal v1 v2 || not (G.mem_vertex s.graph v1 && G.mem_vertex s.graph v2)
  then s
  else
    let mk_edge_map succs =
      let mk_succ e = E.label e, E.dst e in
      E.Map.of_seq @@ Seq.map mk_succ @@ List.to_seq succs
    in
    let succs1 = mk_edge_map @@ G.succ_e s.graph v1 in
    let succs2 = mk_edge_map @@ G.succ_e s.graph v2 in
    let succ_pairs =
      let mk_pair _ succ1 succ2 = match succ1, succ2 with
        | Some s1, Some s2 -> Some (s1, s2)
        | _ -> None
      in
      E.Map.merge mk_pair succs1 succs2
    in
    let s = merge s v1 v2 in
    assert (not (G.mem_vertex s.graph v2));
    let merge_succs _ (succ1, succ2) s =
      assert (succ1 <> v2);
      assert (succ2 <> v1);
      join_without_check s succ1 succ2
    in
    E.Map.fold merge_succs succ_pairs s

let join s v1 v2 : state =
  Options.debug ~level:5 "graph before join(%d,%d):@;<2>@[%a@]" v1 v2 Pretty.pp_graph s;
  assert_invariants s;
  let res = join_without_check s v1 v2 in
  Options.debug ~level:5 "graph after join(%d,%d):@;<2>@[%a@]" v1 v2 Pretty.pp_graph res;
  begin try assert_invariants res
    with Assert_failure _ ->
      Options.debug "join(%d,%d) failed" v1 v2;
      Options.debug "before join:@;<2>@[%a@]" Pretty.pp_internal s;
      Options.debug "after join:@;<2>@[ %a@]" Pretty.pp_internal res
  end;
  res

let merge_set s (vs:VSet.t) : V.t * state =
  let v0 = VSet.choose vs in
  if VSet.cardinal vs < 2 then v0, s else begin
    Options.debug ~level:5 "graph before merge_set %a:@;<2>@[%a@]"
      VSet.pretty vs Pretty.pp_internal s;
    assert (G.mem_vertex s.graph v0);
    let result = VSet.fold (fun v acc -> merge acc v0 v) vs s in
    Options.debug ~level:5 "graph after merge_set %a:@;<2>@[%a@]"
      VSet.pretty vs Pretty.pp_internal result;
    v0, result
  end

(* may operate on an unsound state, where nodes may have multiple successors
   of the same edge type *)
let rec join_succs s v =
  if not @@ G.mem_vertex s.graph v then s else
    let edge_map =
      List.fold_left (fun m e ->
          let add_dst = function
            | None -> Some (VSet.singleton @@ E.dst e)
            | Some vs -> Some (VSet.add (E.dst e) vs)
          in
          E.Map.update (E.label e) add_dst m
        )
        E.Map.empty
        (G.succ_e s.graph v)
    in
    let merge_vset _e vs s =
      if VSet.cardinal vs < 2
      then s
      else let v0, s = merge_set s vs in join_succs s v0
    in
    E.Map.fold merge_vset edge_map s

(* in Steensgard's paper, this is written settype(v1,ref(v2,bot)) *)
let set_type s v1 v2 : state =
  assert_invariants s;
  (* if v1 points to another node, suppress current outgoing edge (and the node if it is a constant node) *)
  let g, new_vmap =
    match G.psucc_opt s.graph v1 with
    | None -> s.graph, s.vmap
    | Some v2 ->
      (* if v2 is a constant node suppress it directly *)
      if VarSet.is_empty (VMap.find v2 s.vmap)
      then G.remove_vertex s.graph v2, VMap.remove v2 s.vmap
      else G.remove_edge s.graph v1 v2, s.vmap
  in
  let new_g = G.add_edge g v1 v2 in
  asserting_invariants {s with graph = new_g; vmap = new_vmap}

let assignment s lv exp : state =
  assert_invariants s;
  let v1, s = find_or_create_lval_vertex (Lval.simplify lv) s in
  match exp with
  | None -> s
  | Some e ->
    match Ast_types.is_ptr (Cil.typeOf e), LvalOrRef.from_exp e with
    | false, _ | _, None -> s
    | true, Some y ->
      let v2, s = find_or_create_lval_or_ref_vertex y s in
      if List.mem v2 (G.psucc s.graph v1) || List.mem v1 (G.psucc s.graph v2)
      then
        let () =
          Options.warning ~source:(fst e.eloc)
            "ignoring assignment of the form: %a = %a"
            Printer.pp_lval lv Printer.pp_exp e;
        in s
      else asserting_invariants @@ join s v1 v2

(* assignment x = allocate(y) *)
let assignment_x_allocate_y s lv : state =
  assert_invariants s;
  let v1, s = find_or_create_lval_vertex (Lval.simplify lv) s in
  match G.psucc_opt s.graph v1 with
  | None ->
    let v2, s = create_empty_vertex s in
    set_type s v1 v2
  | Some _ -> s

let is_included s s' =
  (* tests if s is included in s', at least as the nodes with lval *)
  assert_invariants s;
  assert_invariants s';
  let exception Not_included in
  try
    let check_var (var : varinfo) v : unit =
      let v' = try VarMap.find var s'.varmap with Not_found -> raise Not_included in
      let succs' = G.succ_e s'.graph v' in
      let check_succ (_, l, succ) =
        match List.find_opt (fun (_, l', _) -> EdgeLabel.equals l l') succs' with
        | Some (_, _, succ') when
            V.equal succ succ'
            && VarSet.subset (VMap.find succ s.vmap) (VMap.find succ' s'.vmap) -> ()
        | _ -> raise Not_included
      in
      G.iter_succ_e check_succ s.graph v
    in
    VarMap.iter check_var s.varmap; true
  with Not_included -> false

let empty : state = {graph = G.empty; varmap = VarMap.empty; vmap = VMap.empty}

let is_empty s = compare s empty = 0

(* refresh vertex values to an interval. *)
let refresh s : state =
  assert_invariants s;
  if is_empty s then s else begin
    let fill_map idx map =
      let map = VMap.add idx !node_counter map in incr node_counter; map
    in
    let map_idx = G.fold_vertex fill_map s.graph VMap.empty in
    let convert idx = VMap.find idx map_idx in
    let vmap_convert idx value acc = VMap.add (convert idx) value acc in
    let {graph; varmap; vmap} = s in
    let result =
      {graph = G.map_vertex convert graph;
       varmap = VarMap.map convert varmap;
       vmap = VMap.fold vmap_convert vmap VMap.empty}
    in
    asserting_invariants result
  end

let union_find vmap intersections =
  let module Store : UnionFind.STORE = UnionFind.StoreMap.Make (VMap) in
  let module UF = UnionFind.Make (Store) in
  let uf = UF.new_store () in
  let refs = VMap.mapi (fun i _ -> UF.make uf i) vmap in
  let put_into_uf (v1,v2) =
    let r1 = VMap.find v1 refs in
    let r2 = VMap.find v2 refs in
    ignore @@ UF.union uf r1 r2
  in
  let _vs = Seq.iter put_into_uf intersections in
  let sets_to_be_joined =
    let add_to_map i r sets =
      let repr = UF.find uf r in
      let add_to_set = function
        | None -> Some (VSet.singleton i)
        | Some set -> Some (VSet.add i set)
      in
      VMap.update (UF.get uf repr) add_to_set sets
    in
    VMap.fold add_to_map refs VMap.empty in
  sets_to_be_joined

let union s1 s2 : state =
  assert_invariants s1;
  assert_invariants s2;
  let new_graph =
    G.fold_vertex
      (fun v2 g -> G.add_vertex g v2)
      s2.graph
      s1.graph
  in
  let new_graph =
    G.fold_edges_e (fun e g -> G.add_edge_e g e) s2.graph new_graph
  in
  let new_vmap =
    VMap.union (fun _ lset1 lset2 -> Option.some @@ VarSet.union lset1 lset2)
      s2.vmap
      s1.vmap
  in
  let sets_to_be_joined =
    let intersections = VarMap.to_seq @@ VarMap.intersect s1.varmap s2.varmap in
    union_find new_vmap @@ Seq.map snd intersections
  in
  let new_varmap = VarMap.union (fun _ l _r -> Some l) s1.varmap s2.varmap in
  let s = {graph = new_graph; varmap = new_varmap; vmap = new_vmap} in
  let merged_nodes, s =
    VMap.fold
      (fun _ set (merged_nodes, s) -> let v0, s = merge_set s set in (v0 :: merged_nodes), s)
      sets_to_be_joined
      ([], s)
  in
  let s = List.fold_left join_succs s merged_nodes in
  Options.debug ~level:4 "Union: Result graph:%a" Pretty.pp_graph s;
  begin try assert_invariants s
    with Assert_failure _ ->
      Options.debug "union failed";
      Options.debug "Union: First graph:%a" Pretty.pp_internal s1;
      Options.debug "Union: Second graph:%a" Pretty.pp_internal s2;
      Options.debug "Union: Result graph:%a" Pretty.pp_internal s;
      assert_invariants s
  end;
  s


module Summary = struct
  (* a type for summaries of functions *)
  type t = {state   : state option;
            formals : lval list;
            return  : exp option}

  let make s (kf : kernel_function) =
    let exp_return : exp option =
      if Kernel_function.has_definition kf then
        let return_stmt = Kernel_function.find_return kf in
        match return_stmt.skind with
        | Return (e, _) -> e
        | _ -> Options.fatal "this should not happen"
      else None
    in
    let s =
      match exp_return with
      | None -> s
      | Some e ->
        begin match s, LvalOrRef.from_exp e with
          | _, None -> s
          | s, Some lv ->
            let _, new_s = find_or_create_lval_or_ref_vertex lv s in
            new_s
        end
    in
    {state = Some s;
     formals = List.map (fun v -> (Var v,NoOffset)) (Kernel_function.get_formals kf);
     return = exp_return}

  let pretty ?(debug=false) fmt summary =
    let pp_list_lval s fmt (l: lval list) =
      let is_first = ref true in
      let pp_elem lv =
        if !is_first then is_first := false else Format.fprintf fmt "@  ";
        Format.fprintf fmt "@[%a" Cil_datatype.Lval.pretty lv;
        let pointees = Readout.points_to_vars lv s in
        if not @@ VarSet.is_empty pointees then
          Format.fprintf fmt "%t%a" Unicode.pp_right_arrow VarSet.pretty pointees;
        Format.fprintf fmt "@]";
      in
      List.iter pp_elem l
    in
    let pp_option pp fmt = function
      | Some x -> pp fmt x
      | None -> Format.fprintf fmt "<none>"
    in
    match summary.state with
    | None -> if debug then Format.fprintf fmt "not found"
    | Some s when is_empty s -> if debug then Format.fprintf fmt "empty"
    | Some s ->
      Format.fprintf fmt "@[formals: @[%a@]@;<4>returns: @[%a@]@;<4>state: @[%a@] "
        (pp_list_lval s) summary.formals
        (pp_option Exp.pretty) summary.return
        (pp_option @@ pretty ~debug) summary.state

end

(* the algorithm:
   - unify the two graphs dropping all the variables from the summary
   - pair arguments with formals assigning the formal's successor as the argument's successor
*)
let call s (res : lval option) (args : exp list) (summary : Summary.t) : state =
  assert_invariants s;
  let formals = summary.Summary.formals in
  assert (List.length args = List.length formals);
  let summary_state = refresh @@ Option.get summary.state in

  (* pair up formals and their corresponding arguments,
     as well as the bound result with the returned value *)
  let arg_formal_pairs =
    let res_ret = match res, summary.return with
      | None, None -> []
      | Some res, Some ret ->
        let simplify_ret x = match LvalOrRef.from_exp x with
          | Some (LvalOrRef.Lval lval) -> lval
          | _ -> Options.fatal "unexpected form of return statement"
        in
        [LvalOrRef.Lval (Lval.simplify res), simplify_ret ret]
      | None, Some _ -> []
      | Some _, None -> (* Shouldn't happen: Frama-C adds missing returns *)
        Options.fatal "unexpected case: result without return"
    in
    let simplify_both (arg, formal) =
      try
        match LvalOrRef.from_exp arg with
        | None -> None
        | Some lv -> Some (lv, Lval.simplify formal)
      with Explicit_pointer_address loc ->
        Options.warning ~source:(fst loc) ~wkey:Options.Warn.unsupported_address
          "unsupported feature: explicit pointer address: %a; analysis may be unsound"
          Printer.pp_exp arg;
        None
    in
    res_ret @ List.filter_map simplify_both @@ List.combine args formals
  in

  (* for each pair (lv1,lv2) find (or create) the corresponding vertices *)
  let s, arg_formal_vertex_pairs =
    let s = ref s in
    let find_vertex (lv1, lv2) =
      try
        let v2 = find_lval_vertex lv2 summary_state in
        let v1, new_state = find_or_create_lval_or_ref_vertex lv1 !s in
        s := new_state;
        Some (v1, v2)
      with Not_found -> None
    in
    let args = List.filter_map find_vertex arg_formal_pairs in
    !s, args
  in

  let find_global (vi, v1) =
    try
      ignore @@ Globals.Vars.find vi;
      Some (VarMap.find vi s.varmap, v1)
    with Not_found -> None
  in
  let global_pairs = List.filter_map find_global @@ VarMap.bindings summary_state.varmap in

  let vertex_pairs = arg_formal_vertex_pairs @ global_pairs in

  (* merge the function graph;
     for every arg/formal vertex pair (v1,v2) and every edge v2→v create edge v1→v. *)
  let g =
    let transfer_succs g (v1,v2) =
      List.fold_left
        (fun g e -> G.add_edge_e g @@ E.create v1 (E.label e) (E.dst e))
        g
        (G.succ_e summary_state.graph v2)
    in
    let g = s.graph in
    let g = G.fold_vertex (fun i g -> G.add_vertex g i) summary_state.graph g in
    let g = G.fold_edges_e (fun e g -> G.add_edge_e g e) summary_state.graph g in
    List.fold_left transfer_succs g vertex_pairs
  in

  (* garbage collect: remove leaf vertices from g that originate from summary_state *)
  let vertices_added_to_g, g =
    let g = ref g in
    let remove_if_leaf v _ =
      if G.in_degree !g v = 0
      then let () = g := G.remove_vertex !g v in None
      else Some VarSet.empty
    in
    let remaining_vertices = VMap.filter_map remove_if_leaf summary_state.vmap in
    remaining_vertices, !g
  in

  let s = {
    graph = g;
    varmap = s.varmap;
    vmap =
      let left_bias _ l _ = Some l in
      VMap.union left_bias s.vmap vertices_added_to_g}
  in

  asserting_invariants
    (List.fold_left join_succs s @@ List.map fst vertex_pairs)

module Dot = struct
  let find_vars_ref = Extlib.mk_fun "find_vars"

  include Graph.Graphviz.Dot (struct
      include G
      let edge_attributes _ = []
      let default_edge_attributes _ = []
      let get_subgraph _ = None
      let vertex_attributes v =
        let lset = !find_vars_ref v in
        let label =
          VarSet.pretty Format.str_formatter lset;
          Format.flush_str_formatter ()
        in
        [`Label label]

      let vertex_name v = string_of_int v
      let default_vertex_attributes _ = [`Shape `Box]
      let graph_attributes _ = []
    end)
end

module API = struct
  type t = state

  type summary = Summary.t

  let pretty_summary = Summary.pretty

  let make_summary = Summary.make

  let vid v : int = v

  let rec closure_find_lset v s : (V.t * LSet.t) list =
    match G.psucc_opt s.graph v with
    | None -> [v, Readout.get_lval_set v s]
    | Some v_next -> (v, Readout.get_lval_set v s) :: closure_find_lset v_next s

  let find_transitive_closure lv s : (V.t * LSet.t) list =
    let lv = Lval.simplify lv in
    assert_invariants s;
    try closure_find_lset (find_lval_vertex lv s) s with Not_found -> []
  (* TODO : what about offsets ? *)

  let get_lval_set = Readout.get_lval_set
  let find_vars = Readout.find_vars
  let find_synonyms = Readout.find_synonyms
  let alias_vars = Readout.alias_vars
  let alias_lvals = Readout.alias_lvals
  let points_to_vars = Readout.points_to_vars
  let points_to_lvals = Readout.points_to_lvals
  let alias_sets_vars = Readout.alias_sets_vars
  let alias_sets_lvals = Readout.alias_sets_lvals

  let get_graph s = s.graph

  let print_dot filename s =
    let file = open_out filename in
    Dot.find_vars_ref := (fun v -> get_vars v s);
    Dot.output_graph file s.graph;
    close_out file
end

include API
