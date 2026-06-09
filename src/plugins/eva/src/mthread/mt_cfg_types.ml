(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Mt_types
open Mt_memory.Types
open Mt_shared_vars_types

type thread = Thread.t

(* -------------------------------------------------------------------------- *)
(* --- Live threads/taken mutexes at a given point of execution           --- *)
(* -------------------------------------------------------------------------- *)

type context = {
  started_threads : ThreadPresence.t;
  locked_mutexes : MutexPresence.t;
}

module Context = struct

  type t = context

  let pretty fmt c =
    if not (ThreadPresence.is_empty c.started_threads) then
      Format.fprintf fmt "@[<h>Threads: %a@]"
        ThreadPresence.pretty c.started_threads;
    if not (MutexPresence.is_empty c.locked_mutexes) then
      Format.fprintf fmt "@[<h>Mutexes: %a@]"MutexPresence.pretty c.locked_mutexes;
  ;;

  let empty = {
    started_threads = ThreadPresence.empty;
    locked_mutexes = MutexPresence.empty;
  }

end


(* -------------------------------------------------------------------------- *)
(* --- Accesses to shared memory                                              *)
(* -------------------------------------------------------------------------- *)

type var_access_kind =
  | NotReallySharedVar
  | SharedVarNonConcurrentAccess
  | ConcurrentAccess


type cfg_concur = {
  concur_accesses: SetZoneAccess.t;
  var_access_kind: var_access_kind;
}

module CfgConcur = struct
  type t = cfg_concur


  let combine_access_kind a1 a2 = match a1, a2 with
    | ConcurrentAccess, _ | _, ConcurrentAccess -> ConcurrentAccess
    | SharedVarNonConcurrentAccess, _ | _, SharedVarNonConcurrentAccess ->
      SharedVarNonConcurrentAccess
    | NotReallySharedVar, NotReallySharedVar -> NotReallySharedVar

  let default = {
    concur_accesses = SetZoneAccess.empty;
    var_access_kind = NotReallySharedVar;
  }

  let has_concur_accesses c = not (SetZoneAccess.is_empty c.concur_accesses)

  let must_be_in_cfg ~keep c =
    match keep with
    | NotReallySharedVar -> has_concur_accesses c
    | SharedVarNonConcurrentAccess -> c.var_access_kind <> NotReallySharedVar
    | ConcurrentAccess -> c.var_access_kind = ConcurrentAccess

  let combine c1 c2 = {
    concur_accesses = SetZoneAccess.union c1.concur_accesses c2.concur_accesses;
    var_access_kind = combine_access_kind c1.var_access_kind c2.var_access_kind;
  }

  let add_access (rw, z) c = {
    c with concur_accesses = SetZoneAccess.add (rw, z) c.concur_accesses
  }

end


(* -------------------------------------------------------------------------- *)
(* --- State of a cfg node                                                    *)
(* -------------------------------------------------------------------------- *)

type node_value_state = {
  state_before: state;
  state_after: state;
}

module NodeValueState = struct
  type t = node_value_state

  let dummy = {
    state_before = Cvalue.Model.bottom;
    state_after  = Cvalue.Model.bottom;
  }

  let aux_presence raw_id f state =
    let open Result.Operators in
    let* l = Mt_ids.read_id_state_enumerate 4 state raw_id in
    let error () =
      Format.asprintf "Id %a contains strange state {%a}"
        Mt_ids.pretty_raw_id raw_id
        (Pretty_utils.pp_list ~sep:" " Format.pp_print_int) l;
    in
    f (List.sort compare l) |> Result.map_error error

  let mutex_presence m =
    aux_presence (Mt_ids.of_mutex m)
      (function
        | [0] |[1] | [0;1] -> Ok NotPresent
        | [2] -> Ok Present
        | [0;2] | [1;2] | [0;1;2] -> Ok MaybePresent
        | _ -> Error ())

  let threads_presence started th =
    aux_presence (Mt_ids.of_thread th)
      (fun l -> match l, started with
         | [0], (`Prior | `Started) -> Ok Present
         | [0], `MaybeStarted -> Ok MaybePresent
         | [0], `NotStarted -> Ok NotPresent
         | [1], _ -> Ok Present
         | [2], _ -> Ok NotPresent
         | [0;2], `NotStarted -> Ok NotPresent
         | ([0;1] | [0;2] | [1;2] | [0;1;2]), _ -> Ok MaybePresent
         | _ -> Error ())

end


(* -------------------------------------------------------------------------- *)
(* --- Concurrent control-flow grapg - Type declarations                      *)
(* -------------------------------------------------------------------------- *)

type node = {
  cfgn_id : int;
  mutable cfgn_stack: Callstack.t;
  mutable cfgn_var_access: cfg_concur;
  mutable cfgn_kind : node_kind;
  mutable cfgn_preds: node list;
  mutable cfgn_value_state: node_value_state;
  mutable cfgn_context: context;
}
and node_kind =
  | NMT of stmt * Mt_types.events_set * node
  | NInstr of stmt * node
  | NCall of stmt * (Kernel_function.t list * node list)
  | NWholeCall of
      Kernel_function.t * stmt list * Mt_types.events_set * node
  | NWhile of stmt * node
  | NIf of stmt * node * node
  | NSwitch of stmt * exp * node list
  | NJump of jump_type * node
  | NStart of Kernel_function.t * node
  | NEOP
  | NDead

and jump_type =
  | JBreak of stmt
  | JContinue of stmt
  | JGoto of stmt
  | JReturn of stmt
  | JExit of stmt
  | JBlock of stmt

and cfg = node

module CfgNode = struct

  let make_aux id stack kind = {
    cfgn_id = id;
    cfgn_stack = stack;
    cfgn_var_access = CfgConcur.default;
    cfgn_kind = kind;
    cfgn_preds = [];
    cfgn_value_state = NodeValueState.dummy;
    cfgn_context = Context.empty;
  }

  let dead =
    let stack = Callstack.init ~thread:0 ~entry_point:Cil_datatype.Kf.dummy in
    make_aux (-1) stack NDead

  include Datatype.Make_with_collections(
    struct
      include Datatype.Undefined
      type t = node

      (* XXX incorrect descr: cfgn_value_state contains value datatypes. *)
      let structural_descr = Structural_descr.t_abstract

      let reprs = [dead]
      let name = "Mt_cfg_types.node"

      let rehash x = x

      let compare t1 t2 = Stdlib.compare t1.cfgn_id t2.cfgn_id
      let equal t1 t2 = t1.cfgn_id = t2.cfgn_id
      let hash t = Hashtbl.hash t.cfgn_id

      let pretty fmt node =
        Format.fprintf fmt "node %d" node.cfgn_id

    end)

  let new_node =
    let x = ref 0 in
    fun stack -> make_aux (incr x; !x) stack NEOP
  ;;


  let node_kind_stmt = function
    | NInstr (s, _) | NIf (s, _,_) | NMT (s, _,_) | NCall (s, _)
    | NWhile (s, _) | NSwitch (s, _, _)
    | NJump ((JBreak s | JContinue s | JGoto s |
              JReturn s | JExit s | JBlock s), _) -> [s]
    | NWholeCall (_, l, _, _) -> l
    | NDead | NEOP | NStart _ -> []
  ;;

  let node_stmt n = node_kind_stmt n.cfgn_kind
  let node_first_loc n = match node_stmt n with
    | [] -> None
    | s :: _ -> Some (fst (Cil_datatype.Stmt.loc s))

  let pretty_stmts fmt node =
    match node_stmt node with
    | [] -> Format.pp_print_string fmt "<no stmt>"
    | stmts ->
      let pp_loc fmt s = Fileloc.pretty fmt (Cil_datatype.Stmt.loc s) in
      Pretty_utils.pp_list ~sep:",@ " pp_loc fmt stmts

  let pretty_with_stmts fmt node =
    Format.fprintf fmt "%a@ (%a)" pretty node pretty_stmts node

  let node_kind_succs = function
    | NEOP | NDead -> []
    | NMT (_, _, a) | NInstr (_, a) | NWhile (_, a) | NJump (_, a)
    | NWholeCall (_, _, _, a) | NStart (_, a) -> [a]
    | NIf (_, a1, a2) -> [a1; a2]
    | NCall (_, (_, l)) | NSwitch (_, _, l) -> l
  ;;

  let node_succs n = node_kind_succs n.cfgn_kind

  let pretty_jump_type a fmt j =
    let p x = Format.fprintf fmt x in
    match j with
    | JBreak _ -> p "break"
    | JContinue _ -> p "continue"
    | JGoto _ -> p "goto"
    | JReturn _ -> p "return"
    | JExit _ -> p "exit"
    | JBlock _ -> p "Jump %d" a.cfgn_id
  ;;

  let pretty_kind fmt = function
    | NInstr _ -> Format.pp_print_string fmt "Instr"
    | NIf _ ->  Format.pp_print_string fmt "If"
    | NCall _ -> Format.pp_print_string fmt "Call"
    | NWhile _ -> Format.pp_print_string fmt "Loop"
    | NSwitch _ -> Format.pp_print_string fmt "Switch"
    | NJump (j, a) -> pretty_jump_type a fmt j
    | NEOP -> Format.pp_print_string fmt "EOP"
    | NDead -> Format.pp_print_string fmt "Dead"
    | NMT _ -> Format.pp_print_string fmt "MT Events"
    | NWholeCall _ -> Format.pp_print_string fmt "WholeCall"
    | NStart _ -> Format.pp_print_string fmt "Start"
  ;;

  let pretty_kind_debug fmt nk =
    (* Format.fprintf fmt "[s %a]" (node_kind_stmt nk); *)
    pretty_kind fmt nk;
    match node_kind_succs nk with
    | [] -> ()
    | _ :: _ as l ->
      Format.fprintf fmt " -> ";
      Pretty_utils.pp_list ~sep:"@ "
        (fun fmt a -> Format.fprintf fmt "n%d" a.cfgn_id) fmt l
  ;;

  let pretty_kinds_node_list =
    Pretty_utils.pp_list ~pre:"@[<v>" ~sep:"@ "
      (fun fmt node -> Format.fprintf fmt "@[<hov 2>%a@]"
          pretty_kind_debug node.cfgn_kind)


  let has_concur_accesses n =
    CfgConcur.has_concur_accesses n.cfgn_var_access
  let must_be_in_cfg ~keep n =
    CfgConcur.must_be_in_cfg ~keep n.cfgn_var_access


  let iter_aux
      ~keep_prevs
      ?(f_before=(fun ~prevs:_ _ -> ()))
      ?(f_after=(fun ~prevs:_ _ -> ()))
      a =
    let visited = Hashtbl.create 17 in

    let rec visit a prevs =
      try Hashtbl.find visited a
      with Not_found ->
        Hashtbl.add visited a ();

        let aux a' =
          let prevs = if keep_prevs then a :: prevs else [] in
          visit a' prevs
        in

        (f_before ~prevs a : unit);
        (match a.cfgn_kind with
         | NIf (_, a1, a2) -> aux a1; aux a2

         | NInstr (_, a) | NMT (_, _, a) | NJump (_, a) | NWhile (_, a)
         | NWholeCall (_, _, _, a) | NStart (_, a) -> aux a

         | NCall (_, (_, l)) | NSwitch (_, _, l) -> List.iter aux l

         | NEOP | NDead -> ()
        );
        (f_after ~prevs a : unit)
    in
    visit a []


  let iter ?f_before ?f_after =
    iter_aux ~keep_prevs:false
      ?f_before:(match f_before with
          | None -> None
          | Some f -> Some (fun ~prevs:_ -> f))
      ?f_after:(match f_after with
          | None -> None
          | Some f -> Some (fun ~prevs:_ -> f))


  let iter_with_prevs = iter_aux ~keep_prevs:true

end


module NodeIdAccess = struct

  include Datatype.Triple_with_collections (RW) (CfgNode) (Thread)

  let pretty_aux f fmt ((op, node, th) as v : t) =
    Format.fprintf fmt "@[<hov 3>%a@ by %a@ at %a%a@]"
      RW.pretty op Thread.pretty th CfgNode.pretty_stmts node f v

  let pretty = pretty_aux (fun _fmt _v -> ())

end

module SetNodeIdAccess = struct
  include Abstract_interp.Make_Lattice_Set (NodeIdAccess) (NodeIdAccess.Set)

  let pretty_aux f =
    Pretty_utils.pp_iter ~pre:"@[<v 2>  " ~sep:"@ " iter
      (fun fmt v -> Format.fprintf fmt "@[%a@]" (NodeIdAccess.pretty_aux f) v)

  let pretty = pretty_aux (fun _fmt _v -> ())
end

module AccessesByZoneNode = struct
  include Lmap_bitwise.Make_bitwise(
    struct
      include SetNodeIdAccess
      let default = bottom
      let default_is_bottom = true
    end)

  let pretty_map fmt m =
    Format.fprintf fmt "@[<v 0>";
    fold_fuse_same
      (fun z s () ->
         if not (SetNodeIdAccess.(equal empty s)) then
           Format.fprintf fmt "@[<hov 2>[%a]@ %a@]@ "
             Memory_zone.pretty z (SetNodeIdAccess.pretty) s
      ) m ();
    Format.fprintf fmt "@]";
  ;;

  let pretty fmt = function
    | Top -> Format.pp_print_string fmt "TOP ACCESSES NODE"
    | Bottom -> Format.pp_print_string fmt "BOTTOM ACCESSES NODE"
    | Map m -> pretty_map fmt m

end
