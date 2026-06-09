(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype
open Mt_cil
open Mt_types
open Mt_shared_vars_types
open Mt_cfg_types
open Mt_thread


type mt_status =
  | Dead  (** Unreachable statement according to value analysis *)

  | BasicInstr (** Basic statement, containing no multithreaded content
                   and that can be safely skipped in the automata **)

  | MTIndirectCall of stack_elt list
  (** Calling a function doing a multithreaded event *)

  | MTCall of events_set (** Immediate multithread call(s) *)

  | Complex (** Complex statement, that will always be represented in the
                automata. This includes control-flow operations,
                destination of gotos, etc *)

let _pretty_status fmt status =
  Format.fprintf fmt "%s"
    (match status with
     | Dead -> "dead"
     | BasicInstr -> "basic instr"
     | Complex -> "complex statement"
     | MTIndirectCall _ -> "mt indirect call"
     | MTCall _ -> "mt direct call"
    )
;;

(* Read and write accesses at a given statement *)
let mt_access_at_stmt subtrace stmt =
  match Trace.at_call subtrace (Stack.access_to_var stmt) with
  | None -> CfgConcur.default
  | Some { Trace.trace_events = events } ->
    EventsSet.fold
      (fun evt watch ->
         match evt with
         | VarAccess (rw, z) -> CfgConcur.add_access (rw, z) watch
         | _ -> watch
      ) events CfgConcur.default


(* This is used for the option -mt-compact-in-cfg: we obtain all the
   multithreaded events that occur at the given callstack, and coalesce
   them together *)
let all_events trie =
  Trace.fold trie
    (fun stack evt (evts, watch, stmts) ->
       let stmt = match List.hd stack with
         | _, Kglobal -> assert false (* Cannot happen outside the root function
                                         of the thread, but -mt-compact is not used for those functions *)
         | _, Kstmt stmt -> stmt
       in
       let stmts' = Cil_datatype.Stmt.Set.add stmt stmts in
       match evt with
       | VarAccess (rw, z) ->
         let watch' = CfgConcur.add_access (rw, z) watch in
         (evts, watch', stmts')
       | _ -> (EventsSet.add evt evts, watch, stmts')
    ) (EventsSet.empty, CfgConcur.default, Cil_datatype.Stmt.Set.empty)



let stmt_mt_status subtrace get_state stmt =

  let maybe_basic () =
    (* if a statement is reachable by more than two parents, we cannot flag
       it as basic *)
    if List.length stmt.preds > 1 then Complex else BasicInstr
  in

  if not (Cvalue.Model.is_reachable (get_state stmt)) then
    Dead
  else
    match stmt.skind with
    | Instr (Cil_types.Call _ | Local_init (_, ConsInit _, _)) ->
      (let callsites =
         (* MT events that originate from this statement. There
            can be multiple ones through a pointer call *)
         Trace.find_at_stmt subtrace stmt in
       let events = ref EventsSet.empty
       and deep_calls = ref [] in
       (* We separate those MT events in two classes: those that occur
          immediately (ie. calls to an Mthread function), and those
          than take deeper in the stack *)
       List.iter
         (fun (call, subtrace') ->
            (* We do not consider calls to [Stack.fun_access_vars], as
               they are only used to represent var accesses *)
            if not (Stack.is_access_to_var call) then
              if not (Trace.no_deep_call subtrace') then
                deep_calls := call :: !deep_calls;

            match Trace.at_root subtrace' with
            | None -> ()
            | Some { Trace.trace_events = evts } ->
              events := EventsSet.union !events evts
         ) callsites;
       events := EventsSet.filter
           (function VarAccess _ -> false | _ -> true) !events;
       match !deep_calls = [], EventsSet.is_empty !events with
       | true, true ->
         (* No concurrent action at all *)
         maybe_basic ()
       | true, false ->
         (* One or more calls to mthreads functions *)
         MTCall !events
       | false, true ->
         (* Some mthreads events deeper in the stack. *)
         MTIndirectCall !deep_calls
       | false, false ->
         (* This case is supposed to handle [*p(...)], with [p] pointing to
            both an Mthread function and a non-Mthread one. *)
         Mt_self.debug "%a"
           (Pretty_utils.pp_list
              (fun fmt (selt, subtrace) ->
                 Format.fprintf fmt "@[<v>-- %a@.%a@]"
                   Mt_cil.StackElt.pretty selt
                   Trace.pretty subtrace
              )) callsites;
         Mt_self.abort ?source:(Mt_cil.kinstr_to_source (Kstmt stmt))
           "@[simultaneous@ call@ to@ a@ mthread@ function@ and@ \
            to@ another@ function:@ very@ strangely@ written@ \
            mthread@ binding?@]";
      )
    | Instr _ ->
      maybe_basic ()

    | Loop (_, _, _, _, _) | Block _ | UnspecifiedSequence _ | If _ | Switch _
      -> Complex

    | Continue _  | Goto (_, _) | Break _ ->
      maybe_basic ()

    | Return _ -> (* Return statement are not considered basic 1) because they
                     do not have successors  2) because we want to notice
                     them in the automata *)
      Complex

    | TryFinally _ | TryExcept _ | Throw _ | TryCatch _ ->
      Mt_self.not_yet_implemented "try finally/except/throw/catch"
;;

let rec make_cfg_aux ~eop ~subtrace ~caller_succ callstack =
  let f = Callstack.top_kf callstack in

  (* Mapping between statements and automata nodes. We store all nodes
     that are not basic instr (for which we are sure they are never
     visited twice *)
  let node_tbl = Cil_datatype.Stmt.Hashtbl.create 71

  and get_state, get_state_after = match Trace.at_root subtrace with
    | None -> (* Can happen in the root thread function, if
                 nothing multithreaded happens, but we handle this case
                 explicitly *)
      Mt_self.fatal "No events at subtrace %a" Callstack.pretty callstack
    | Some { Trace.trace_states = states;
             Trace.trace_states_after = states_after } ->
      assert (Stmt.Map.cardinal states > 0);
      Mt_memory.Types.map_functions_states_to_get_state states,
      Mt_memory.Types.map_functions_states_to_get_state states_after
  in

  let rec aux stmt =
    try Cil_datatype.Stmt.Hashtbl.find node_tbl stmt
    with Not_found ->

      let mt_access = lazy (mt_access_at_stmt subtrace stmt) in

      (* Fresh node, containing a dummy content at first, and a function
         updating the action and setting the preds field of the other nodes *)
      let tg () =
        let r = CfgNode.new_node callstack (* XXX *) in
        Cil_datatype.Stmt.Hashtbl.add node_tbl stmt r;
        r.cfgn_value_state <- {
          state_before = get_state stmt;
          state_after = get_state_after stmt;
        };
        let set nk =
          r.cfgn_kind <- nk;
          r.cfgn_var_access <- Lazy.force mt_access;
          let succs = CfgNode.node_kind_succs nk in
          List.iter (fun a -> a.cfgn_preds <- r :: a.cfgn_preds) succs;
          r
        in
        r, set
      in

      (* Function that extracts the hopefully unique successor of [stmt],
         translate its, or fails very verbosely otherwise. Beware where you
         call it *)
      let next () =
        match stmt.succs with
        | [] -> Mt_self.fatal
                  ?source:(Mt_cil.kinstr_to_source (Kstmt stmt))
                  "Statement with no successor encountered at \
                   an unexpected place (sid %d)" stmt.sid
        | [stmt'] -> aux stmt'
        | _ :: _ :: _ -> Mt_self.fatal
                           ?source:(Mt_cil.kinstr_to_source (Kstmt stmt))
                           "Statement with more than one successor encountered at an \
                            unexpected place: (sid %d), succs %a"
                           stmt.sid pretty_succs stmt
      in
      (* Specialized case of the above function, that captures the case
         of a function that never returns *)
      let next_call () =
        if stmt.succs = [] || Cvalue.Model.(equal bottom (get_state_after stmt))
        then CfgNode.dead
        else next ()
      in

      match stmt_mt_status subtrace get_state stmt with
      | Dead -> CfgNode.dead

      | BasicInstr ->
        (* Basic instr always one successor by construction of the
           previous phase, and only one predecessor. It may be safe
           to skip the construction of this node altogether *)
        if Mt_options.FullCfg.get () ||
           CfgConcur.has_concur_accesses (Lazy.force mt_access)
        then
          let _, set = tg () in
          let n = next_call () in
          set (NInstr (stmt,  n))
        else
          next_call ()

      | MTIndirectCall callstacks ->
        let nwhole, set = tg () in
        let n = next_call () in
        let sub_cfgs = List.map
            (fun (kf,kinstr as call) ->
               let callsite = match kinstr with
                 | Kglobal -> assert false
                 | Kstmt stmt -> stmt
               in
               let callstack' = Callstack.push kf callsite callstack in
               let subtrace = Trace.subtrace_at_call subtrace call in
               if Function_calls.use_spec_instead_of_definition kf then
                 let evts, access, stmts = all_events subtrace in
                 let node = CfgNode.new_node callstack'
                 and stmts = Cil_datatype.Stmt.Set.elements stmts in
                 node.cfgn_value_state <- nwhole.cfgn_value_state;
                 node.cfgn_kind <- NWholeCall (kf, stmts, evts, n);
                 node.cfgn_var_access <- access;
                 n.cfgn_preds <- node :: n.cfgn_preds;
                 kf, node
               else
                 kf,
                 make_cfg_aux ~eop ~subtrace ~caller_succ:n callstack'
            ) callstacks
        in
        set (NCall (stmt, List.split sub_cfgs))

      | MTCall evset ->
        let _, set = tg () in
        let n = next_call () in
        set (NMT (stmt, evset, n))

      | Complex ->
        match stmt.skind with
        | Instr _ ->
          let _, set = tg () in
          let n = next_call () in
          set (NInstr (stmt, n))

        | Block _ | UnspecifiedSequence _ ->
          if List.length stmt.Cil_types.preds = 1 then
            (* Only one way to reach the block, we always skip
               the jump *)
            next ()
          else
            let _, set = tg () in
            set (NJump (JBlock stmt, next ()))

        | If (c, bthen, belse, _) ->
          let _, set = tg () in

          (* Successor of an if is not what we need. If the 'then'
             or the 'else' part is empty, we try to extract the
             successor from the succs field *)
          let nt, ne =
            match bthen.bstmts, belse.bstmts, stmt.succs with
            | st :: _, se :: _, _ ->
              st, se
            | [], se :: _, [st; se'] when se.sid = se'.sid ->
              st, se
            | st :: _, [], [st'; se] when st.sid = st'.sid ->
              st, se
            | [], [], [s; s'] when s.sid = s'.sid ->
              s, s
            | _ -> Mt_self.fatal
                     "Strange looking if: %a, (%a) as succs"
                     Printer.pp_stmt stmt pretty_succs stmt
          in

          (* We try to detect dead branches by evaluating the
             condition, as we cannot just look at the liveness
             of the successors (which can be live for other
             reasons if there is no then/else block) *)
          let request = Results.in_cvalue_state (get_state stmt) in
          let c = Results.(eval_exp c request |> as_cvalue) in
          let eval_then = Cvalue.V.contains_non_zero c
          and eval_else = Cvalue.V.contains_zero c
          in
          let at, ae = match eval_then, eval_else with
            | true, true -> aux nt, aux ne
            | false, true -> CfgNode.dead, aux ne
            | true, false -> aux nt, CfgNode.dead
            | false, false -> CfgNode.dead, CfgNode.dead
          in
          set (NIf (stmt, at, ae))

        | Loop (_, lbody, _, _continue_stmt, _break_stmt) ->
          let tg, set = tg () in
          (match lbody.bstmts with
           | [] -> set (NWhile (stmt, tg))
           | s :: _ ->
             let r = aux s in
             set (NWhile (stmt, r))
          )

        | Break _ ->
          let _, set = tg () in
          set (NJump (JBreak stmt, next ()))

        | Continue _ ->
          let _, set = tg () in
          set (NJump (JContinue stmt, next ()))

        | Goto _ ->
          let _, set = tg () in
          set (NJump (JGoto stmt, next ()))

        | Return _ ->
          let _, set = tg () in
          let k =
            if CfgNode.equal caller_succ eop
            then JExit stmt
            else JReturn stmt
          in
          set (NJump (k, caller_succ))

        | Switch (e, _, _, _) ->
          let _, set = tg () in
          let l = List.map aux stmt.succs in
          set (NSwitch (stmt, e, l))

        | TryFinally _ | TryExcept _ | Throw _ | TryCatch _ ->
          Mt_self.not_yet_implemented "try finally/try except"
  in
  match (Kernel_function.get_definition f).sbody.bstmts with
  | [] -> Mt_self.abort "Function with empty body: %s"
            (Kernel_function.get_name f)
  | s :: _ -> aux s
;;

(* [replace_succs ~prev ~next ~a] changes the [cfgn_kind] field
   of [a], by replacing all occurrences of [prev] by [next].
   Warning: this function does not update the [cfgn_preds] field
   of [next] *)
let replace_succs ~prev ~next ~a =
  let c = a.cfgn_kind
  and r n = if CfgNode.equal n prev then next else n
  in
  a.cfgn_kind <-
    match c with
    | NEOP | NDead -> c
    | NMT (s, mt, a) -> NMT (s, mt, r a)
    | NWholeCall (kf, s, mt, a) -> NWholeCall (kf, s, mt, r a)
    | NJump (j, a) -> NJump (j, r a)
    | NWhile (s, a) -> NWhile (s, r a)
    | NInstr (s, a) -> NInstr (s, r a)
    | NStart (kf, a) -> NStart (kf, r a)
    | NIf (s, a1, a2) -> NIf (s, r a1, r a2)
    | NCall (s, (ln, l)) -> NCall (s, (ln, List.map r l))
    | NSwitch (s, e, l) -> NSwitch (s, e, List.map r l)


(* Try to remove this node from its cfg if it does not bring multi-threaded
   content. Returns the successor of the argument, or the argument itself
   if it cannot be removed *)
let remove_node ~keep a =

  let remove_from_preds ~n ~remove =
    n.cfgn_preds <- List.filter
        (fun n' -> not (List.exists (CfgNode.equal n') remove)) n.cfgn_preds
  in

  (* Auxiliary function: replace [a] by [succ] *)
  let replace_by_succ ?(remove_also=[]) succ =
    (* Rewrite all the nodes pointing to [a] *)
    List.iter (fun pred -> (* In [pred], replace [a] by [succ] *)
        replace_succs ~a:pred ~prev:a ~next:succ
      ) a.cfgn_preds;
    (* Rewrite the predecessors of [succ] *)
    succ.cfgn_preds <- succ.cfgn_preds @ a.cfgn_preds;
    remove_from_preds ~n:succ ~remove:(a :: remove_also);
    succ
  in

  if not (CfgNode.must_be_in_cfg ~keep a) then
    (* The nodes above must remain in the cfg because they contain a
       multithread access *)
    match a.cfgn_kind with
    | NJump ((JBreak _ | JContinue _ | JGoto _ | JBlock _), succ)
    | NInstr (_, succ) -> replace_by_succ succ

    | NIf (_, nthen, nelse) ->
      (match nthen, nelse with
       | { cfgn_kind = NDead }, succ
       | succ, { cfgn_kind = NDead } ->
         (* IF with one dead branch *)
         replace_by_succ succ
       | _ ->
         (* IF with the same target in the two branches *)
         if CfgNode.equal nthen nelse then
           replace_by_succ nthen
         else a
      )

    | NWhile (_, ({ cfgn_kind = NIf (_, nthen, nelse) } as n')) ->
      (* We rewrite 'while' loops that suspiciously like 'for' ones.
         However, we make sure that the loop terminates sometimes,
         as removing unterminating loops is not a good idea *)
      (* YYY For complex loops with more than one break, write something
         more general, that visits all the nodes below [a] (until the cycle)
         and check that they all go to the same node. Beware of incoming
         nodes *)
      if CfgNode.equal nthen a &&
         nelse.cfgn_kind <> NDead &&
         not (CfgNode.must_be_in_cfg ~keep n') &&
         List.length n'.cfgn_preds = 1
      then
        (* remove_also is [n'] because [n'] is also a pred of [a] *)
        replace_by_succ ~remove_also:[n'] nelse
      else
        a

    | NWhile (_, succ) when List.length a.cfgn_preds = 1 ->
      (* False while/for loop, in which we never do a full iteration *)
      replace_by_succ  succ

    | NCall (stmt, (lkf , (_ :: _ as ln) )) ->
      let succ = ref None in
      let aux_node acc (f, n) =
        let remove_n succ' =
          (match !succ with
           | None -> succ := Some succ'
           | Some succ -> assert (CfgNode.equal succ succ')
          );
          remove_from_preds ~n:succ' ~remove:[n];
        in
        match CfgNode.must_be_in_cfg ~keep n, n.cfgn_kind with
        | false, NJump (JReturn _, succ) ->
          (* This can happen because the call originally contained
             accesses to vars, which have been later detected as
             non-concurrent and removed *)
          assert (List.length n.cfgn_preds = 1);
          remove_n succ;
          acc

        | false, NWholeCall (_, _, s, succ) ->
          (* WholeCall that contains only accesses to variables we
             are not interested in *)
          assert (List.length n.cfgn_preds = 1);
          let only_var_access = Mt_types.EventsSet.for_all
              (function VarAccess _ -> true | _ -> false) s in
          if only_var_access then
            (remove_n succ; acc)
          else
            (f, n) :: acc

        | true, _ | false, _ -> (f, n) :: acc
      in
      let l' = List.fold_left aux_node [] (List.combine lkf ln) in
      if l' = [] then
        (* All subcalls are been recursively removed: remove [a] too *)
        match !succ with
        | None -> Mt_self.fatal "Impossible case in cfg NCall removal"
        | Some succ -> replace_by_succ succ
      else
        (a.cfgn_kind <- NCall (stmt, List.split l');
         a)

    | NSwitch (_, _ , l ) ->
      let l' = List.filter (fun n' -> not (CfgNode.(equal n' dead))) l in
      begin
        match l' with
        | [] -> replace_by_succ CfgNode.dead (* all succs are dead *)
        | n :: q ->
          if List.for_all (CfgNode.equal n) q then
            replace_by_succ n
          else
            a
      end

    | _ -> a (* Not simplification on this node *)
  else
    a


(* Remove nodes that do not contain multithread content *)
let remove_superfluous_nodes ~keep start =
  let start = ref start in
  CfgNode.iter ~f_after:(fun n ->
      let n' = remove_node ~keep n in
      if CfgNode.equal n !start then
        start := n')
    !start;
  !start

let make_cfg th =
  let events = th.th_amap in
  let init_call = th.th_fun, Kglobal in
  let th_id = Thread.id th.th_eva_thread in
  let callstack = Callstack.init ~thread:th_id ~entry_point:th.th_fun in
  let tg = CfgNode.new_node callstack in (* Originally an empty stack, is that important ? *)
  tg.cfgn_kind <- NEOP;
  let subtrace = Trace.subtrace_at_call events init_call in
  let tg = make_cfg_aux ~eop:tg ~subtrace ~caller_succ:tg callstack in
  let start = CfgNode.new_node callstack in
  start.cfgn_kind <- NStart (th.th_fun, tg);
  start.cfgn_value_state <- {
    state_before = th.th_init_state;
    state_after = th.th_init_state;
  };
  tg.cfgn_preds <- [start];
  if not (Mt_options.FullCfg.get ())
  then remove_superfluous_nodes ~keep:NotReallySharedVar start
  else start


let dot_fprint_graph fmt start_tg link_stmt =
  let module OcamlgraphCfg = struct

    type t = CfgNode.t

    module V = struct
      type t = CfgNode.t
    end

    type edge_direction = Forward | Backward

    type edge_type =
      | IfThen | IfElse
      | DefaultEdge
      | Return
      | Fun of string * bool (* The bool indicated that the edge targets a
                                WholeCall node *)

    let edge_type_to_string = function
      | IfThen -> "then"
      | IfElse -> "else"
      | Fun (s, _) -> s
      | DefaultEdge | Return -> ""

    module E = struct

      type edge_annot = edge_direction * edge_type

      type t = V.t * V.t * edge_annot

      let src (v, _, _ : t) = v
      let dst (_ , v, _ : t) = v
    end


    let red =         0xff0000ffl
    and black =       0x000000ffl
    and white =       0xffffffffl
    and light_blue =  0xccccffffl
    and light_green = 0xccffccffl
    and light_grey =  0xaaaaaaffl
    ;;

    let graph_attributes (_ : t) =
      [ `Fontsize 9;
        `Center true; ]

    let get_subgraph (_ : V.t) = None

    let vertex_name (v : V.t)  = Format.sprintf "v%d" v.cfgn_id

    let pretty_vertex vlabel =
      match vlabel.cfgn_kind with
      | NInstr (s, _) -> Format.asprintf "%a" Printer.pp_stmt s
      | NIf (s, _, _) ->
        (match s.skind with
         | If (e, _, _, _) -> Format.asprintf
                                "if (%a)" Printer.pp_exp e
         | _ -> assert false)
      | NWhile (_, _) -> "while(1)"
      | NSwitch (_, e, _) ->
        Format.asprintf "switch (%a)" Printer.pp_exp e
      | NCall(s, _) ->
        begin
          match s.skind with
          | Instr i ->
            Format.asprintf "Call %a" Printer.pp_instr i
          | _ -> assert false
        end
      | NEOP -> ""
      | NMT (_, evts, _) | NWholeCall (_, _, evts, _) ->
        Format.asprintf "@[<v 0>%a@]" (EventsSet.pretty ()) evts
      | NStart (kf, _) ->
        Format.asprintf "Start: %s" (Kernel_function.get_name kf)
      | NJump _ | NDead ->
        Format.asprintf "%a" CfgNode.pretty_kind vlabel.cfgn_kind

    ;;

    let default_vertex_attributes (_ : t) = []

    let vertex_attributes (v : V.t) : Graph.Graphviz.DotAttributes.vertex list =
      let label =
        if CfgNode.has_concur_accesses v then
          let s = pretty_vertex v in
          let sep = if s <> ""
            then ("@." : (_, _, _, _, _, _) format6)
            else "" (* Can happen with [WholeCall} nodes *)
          in
          Format.asprintf "%s%(%)%a"
            (pretty_vertex v)
            sep
            SetZoneAccess.pretty v.cfgn_var_access.concur_accesses
        else pretty_vertex v
      and shape =
        match v.cfgn_kind with
        | NEOP -> `Plaintext
        | NWholeCall _ -> `Diamond
        | _ -> `Box
      and color = match v.cfgn_kind with
        | NStart _ -> 0x55l
        | NMT _ -> red
        | NWholeCall (_, _, evts, _) ->
          if EventsSet.is_empty evts then black else red
        | NJump (JReturn _, _) | NCall _ -> light_grey
        | _ -> black
      and fillcolor =
        match v.cfgn_kind, v.cfgn_var_access.var_access_kind with
        | NStart _, _ -> 0l
        | _, NotReallySharedVar -> white
        | _, SharedVarNonConcurrentAccess -> light_green
        | _, ConcurrentAccess -> light_blue
      and url = match CfgNode.node_stmt v with
        | [stmt] -> link_stmt stmt
        | _ -> "#"
      and style = match v.cfgn_kind with
        | NEOP -> `Invis
        | _ -> `Filled
      in [
        `Label (String.utf8_escaped label);
        `ColorWithTransparency color;
        `Shape shape;
        `Style style;
        `FillcolorWithTransparency fillcolor;
        `Url url
      ]
    ;;

    let iter_vertex f (root : t) =
      let f v = match v.cfgn_kind with
        | NDead -> ()
        | _ -> f v
      in
      CfgNode.iter ~f_before:f root


    exception NoCaller

    (* Find the [NCall] node that corresponds to a given [JReturn]. The
       stack is supposed to be obtained by a depth-first search, and is
       thus in reversed order. When encountering a [JReturn] or [NWholeCall]
       (the latter have no return node), we increase the stack depth,
       and decrease it when when encounter a [NCall] node *)
    let rec caller_in_stack depth = function
      | { cfgn_kind = (NJump (JReturn _, _) | NWholeCall _ )} :: q ->
        caller_in_stack (depth+1) q
      | ({ cfgn_kind = NCall _ } as n) :: q ->
        if depth = 0 then n
        else caller_in_stack (depth-1) q
      | _ :: q -> caller_in_stack depth q
      | [] -> raise NoCaller

    let iter_edges_e f (root : t) =
      let f ~prevs v =
        let do_edge ?(etype=DefaultEdge) dst =
          let dir, src', dst' =
            if List.exists (fun v' -> CfgNode.equal dst v') prevs
            then Backward, dst, v
            else Forward,  v,   dst
          in
          match dst.cfgn_kind with
          | NDead -> ()
          | _ ->
            let e = (src', dst', (dir, etype)) in
            f e
        in
        match v.cfgn_kind with
        | NDead | NEOP -> ()

        | NJump (JReturn _, a) ->
          do_edge a;
          (if Mt_options.ShowReturnEdges.get () then
             try
               let caller = caller_in_stack 0 prevs in
               do_edge ~etype:Return caller
             with NoCaller ->
               Mt_self.error "Strange stack in cfg, please report@.%a"
                 CfgNode.pretty_kinds_node_list prevs
          )

        | NInstr (_, a) | NJump (_, a) | NMT (_, _, a) | NWhile (_, a)
        | NWholeCall (_, _, _, a) | NStart (_, a) -> do_edge a

        | NSwitch (_, _, l) -> List.iter do_edge l

        | NCall (s, (ln, l)) ->
          let fun_name = match s.skind with
            | Instr (Call (_, Var _, _, _)
                    |Local_init(_,ConsInit _,_)) ->
              (fun _ -> "")
            | _ -> (fun kf -> Kernel_function.get_name kf)
          in
          let do_edge name n =
            let fun_name = fun_name name in
            let etype = match n.cfgn_kind with
              | NWholeCall _ ->  Fun (fun_name, true)
              | _ -> Fun (fun_name, false)
            in
            do_edge ~etype n
          in
          List.iter2 do_edge ln l

        | NIf (_, a1, a2) ->
          do_edge ~etype:IfThen a1;
          do_edge ~etype:IfElse a2;
      in
      CfgNode.iter_with_prevs ~f_before:f root


    let default_edge_attributes (_ : t) = []

    let edge_attributes (_, _, (dir, etype) : E.t) =
      let label = match edge_type_to_string etype with
        | "" -> []
        | s -> [`Label s]
      and dir = match dir, etype with
        | _, (Return | Fun (_, true)) -> `Dir `None
        | Forward, _ -> `Dir `Forward
        | Backward, _ -> `Dir `Back
      and style = match etype with
        | Return -> [`Style `Dotted; `Weight 1]
        | Fun (_, _) -> [`Weight 200] (* ZZZ Find a way to shorten short edges *)
        | _ -> []
      in
      dir :: label @ style
  end in
  let module DotAutomata = Graph.Graphviz.Dot(OcamlgraphCfg) in
  DotAutomata.fprint_graph fmt start_tg






(* Shared vars accesses inside cfgs *)

let cfg_accesses th node =
  let r = ref AccessesByZoneNode.empty_map in
  let do_node n =
    SetZoneAccess.iter
      (fun (rw, z) ->
         let m = AccessesByZoneNode.Map !r in
         let v = SetNodeIdAccess.inject_singleton (rw, n, th) in
         let m' = AccessesByZoneNode.add_binding m ~exact:false z v in
         match m' with
         | AccessesByZoneNode.Map m -> r := m
         | AccessesByZoneNode.Bottom ->
           assert false (* Impossible because m is not Bottom *)
         | AccessesByZoneNode.Top ->
           assert false (* We never store a write to a Top zone *)
      ) n.cfgn_var_access.concur_accesses
  in
  CfgNode.iter ~f_before:do_node node;
  !r


let compute_node_context th mutexes iter state node =
  let extract ~default v = match v with
    | Ok v -> v
    | Error error ->
      Mt_self.warning "%a: %s" CfgNode.pretty_with_stmts node error;
      default
  in
  let mutexes =
    Mutex.Set.fold
      (fun m acc ->
         let mutex_presence = NodeValueState.mutex_presence m state in
         let p = extract ~default:NotPresent mutex_presence in
         MutexPresence.add m p acc
      ) mutexes MutexPresence.empty
  and threads =
    let presence = ref ThreadPresence.empty in
    let save th v = presence := ThreadPresence.add th v !presence in
    iter
      (fun th' started ->
         if Thread.equal th th' then `NotStarted
         else
           let presence = NodeValueState.threads_presence started th' state in
           let r = extract ~default:MaybePresent presence in
           save th' r;
           match started, r with
           | `Prior, _ -> `Prior
           | `Started, _ -> `Started
           | `MaybeStarted, _ -> `Started
           | `NotStarted, Present -> `Started
           | `NotStarted, MaybePresent -> `MaybeStarted
           | `NotStarted, NotPresent -> `NotStarted
      ) `Prior;
    !presence
  in {
    started_threads = threads;
    locked_mutexes = mutexes;
  }

let update_cfg_contexts analysis th =
  let mutexes = analysis.all_mutexes
  and iter = OrderedThreads.ordered_iter analysis
  in
  let compute_context = compute_node_context th.th_eva_thread mutexes iter in
  CfgNode.iter ~f_before:
    (fun node ->
       let state_after = node.cfgn_value_state.state_after in
       if Cvalue.Model.is_reachable state_after then
         node.cfgn_context <- compute_context state_after node
       else
         (* Useful to deal with memory accesses that fail and cause the
            value analysis to crash, but that occur on shared zones *)
         let state_before = node.cfgn_value_state.state_before in
         if Cvalue.Model.is_reachable state_before then
           node.cfgn_context <- compute_context state_before node
    ) th.th_cfg
