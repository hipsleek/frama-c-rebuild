(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Parameters
open Eva_annotations
open Cil_types

let is_return s = match s.skind with Return _ -> true | _ -> false
let is_loop s =   match s.skind with Loop _ -> true | _ -> false

let warn ?(current = true) =
  Kernel.warning ~wkey:Kernel.wkey_annot_error ~once:true ~current

module Make (Kf : sig val kf: kernel_function end) =
struct
  let kf = Kf.kf
  let automaton = Eva_automata.get_automaton kf

  let widening_delay = WideningDelay.get ()
  let widening_period = WideningPeriod.get ()

  let interpreter_mode = InterpreterMode.get ()

  let slevel stmt =
    if is_return stmt || interpreter_mode then
      max_int
    else match Per_stmt_slevel.local kf with
      | Per_stmt_slevel.Global i -> i
      | Per_stmt_slevel.PerStmt f -> f stmt

  let merge_after_loop = SlevelMergeAfterLoop.mem kf

  let merge stmt =
    is_loop stmt && merge_after_loop
    ||
    match Per_stmt_slevel.merge kf with
    | Per_stmt_slevel.NoMerge -> false
    | Per_stmt_slevel.Merge f -> f stmt

  let min_loop_unroll = MinLoopUnroll.get ()
  let auto_loop_unroll = AutoLoopUnroll.get ()
  let default_loop_unroll = DefaultLoopUnroll.get ()

  let warn_no_loop_unroll stmt =
    let is_attribute a = Ast_attributes.contains a stmt.sattr in
    match List.filter is_attribute ["for" ; "while" ; "dowhile"] with
    | [] -> ()
    | loop_kind :: _ ->
      let wkey =
        if loop_kind = "for"
        then Self.wkey_missing_loop_unroll_for
        else Self.wkey_missing_loop_unroll
      in
      Self.warning
        ~wkey ~source:(fst (Cil_datatype.Stmt.loc stmt)) ~once:true
        "%s loop without unroll annotation" loop_kind

  let unroll loop =
    let automatic_unrolling i =
      if i > min_loop_unroll
      then Partition.AutoUnroll (loop, min_loop_unroll, i)
      else Partition.IntLimit min_loop_unroll
    in
    let default = automatic_unrolling auto_loop_unroll in
    match get_unroll_annot loop.stmt with
    | [] -> warn_no_loop_unroll loop.stmt; default
    | [UnrollFull] -> Partition.IntLimit default_loop_unroll
    | [UnrollAuto i] -> automatic_unrolling i
    | [UnrollAmount t] -> begin
        (* Inlines the value of const variables in [t]. *)
        let global_init vi =
          try (Globals.Vars.find vi).init with Not_found -> None
        in
        let t =
          Cil.visitCilTerm (new Logic_utils.simplify_const_lval global_init) t
        in
        try
          match Logic_utils.constFoldTermToInt t with
          | Some n -> Partition.IntLimit (Z.to_int n)
          | None   -> Partition.ExpLimit (Logic_to_c.term_to_exp t)
        with Z.Overflow | Logic_to_c.No_conversion ->
          warn "invalid loop unrolling parameter; ignoring";
          default
      end
    | _ :: _ :: _ ->
      warn "more than one loop unroll annotation; ignoring";
      default

  let history_size =
    try HistoryPartitioningFunction.find kf
    with Not_found -> HistoryPartitioning.get ()

  let split_limit = SplitLimit.get ()

  let universal_splits =
    let add name l =
      try
        let vi = Globals.Vars.find_from_astinfo name Global in
        let limit = split_limit
        and term = Partition.Expression (Eva_ast.Build.var_exp vi)
        and kind = Partition.Dynamic
        and loc = Fileloc.unknown in
        let monitor = Partition.new_monitor ~limit ~term ~kind ~loc in
        Partition.Split monitor :: l
      with Not_found ->
        warn ~current:false "cannot find the global variable %s for value \
                             partitioning; ignoring" name;
        l
    in
    ValuePartitioning.fold add []

  let translate_split_term = function
    | Term term ->
      let exp = Logic_to_c.term_to_exp ?result:None term in
      Partition.Expression (Eva_ast.translate_exp exp), exp.eloc
    | Predicate pred ->
      Partition.Predicate pred, pred.pred_loc
    | ConditionalCases ->
      assert false

  let translate_flow_annotation vertex annotation =
    try
      match annotation with
      | FlowSplit (ConditionalCases, _) ->
        let do_branch (src,edge,dest) =
          let source = src.Eva_automata.vertex_key in
          let branch = edge.Eva_automata.edge_key in
          (dest, Partition.SyntacticSplit (source, branch))
        in
        (* Find first vertex with several successors. *)
        let rec find_next_branches vertex =
          match Eva_automata.G.succ_e automaton.graph vertex with
          | [] -> [] (* No successor, should probably emit a warning? *)
          | [_, _, v] -> find_next_branches v (* only one successor *)
          | l -> List.map do_branch l
        in
        find_next_branches vertex
      | FlowMerge (ConditionalCases) ->
        [vertex, (Partition.MergeSyntacticSplits)]
      | FlowSplit (term, kind) ->
        let term, loc = translate_split_term term in
        let split_monitor =
          Partition.new_monitor ~limit:split_limit ~kind ~term ~loc
        in
        [vertex, Partition.Split split_monitor]
      | FlowMerge term ->
        let term, _loc = translate_split_term term in
        [vertex, Partition.Merge term]
    with
    | Logic_to_c.No_conversion ->
      warn "split/merge expressions must be valid expressions; ignoring";
      []

  module VertexTable = Eva_automata.Vertex.Hashtbl

  let flow_annotations_table =
    let table = VertexTable.create (Eva_automata.G.nb_vertex automaton.graph) in
    let add_action (vertex, action) =
      action :: VertexTable.find_default ~default:[] table vertex
      |> VertexTable.replace table vertex
    in
    let add_annotations vertex =
      let stmt = Eva_automata.Vertex.stmt vertex in
      let annotations = Option.fold ~none:[]~some:get_flow_annot stmt in
      annotations
      |> List.concat_map (translate_flow_annotation vertex)
      |> List.iter add_action
    in
    Eva_automata.G.iter_vertex add_annotations automaton.graph;
    table

  let split_return_action =
    match Split_return.kf_strategy kf, Eva_utils.find_return_var kf with
    | SplitAuto, _ ->
      assert false (* SplitAuto already transformed into SplitEqList. *)
    | FullSplit, _ ->
      Partition.Ration (Partition.new_rationing ~limit:max_int ~merge:false)
    | SplitEqList i, Some return_vi
      when Ast_types.is_integral_or_pointer return_vi.vtype ->
      let return_exp = Eva_ast.Build.var_exp return_vi in
      Partition.Restrict (return_exp, i)
    | (NoSplit | SplitEqList _), _ ->
      Partition.Ration (Partition.new_rationing ~limit:0 ~merge:false)

  let flow_actions vertex =
    let flow_actions =
      VertexTable.find_default ~default:[] flow_annotations_table vertex
    in
    let store_results, rationing_action =
      match Eva_automata.Vertex.stmt vertex with
      | None when vertex == automaton.return_point ->
        true, split_return_action
      | Some stmt when not (Cil.is_skip stmt.skind && flow_actions <> []) ->
        (* A skip statement is created on each split annotation: do not ration
           states on them to avoid meddling in successive split directives. *)
        let limit = slevel stmt and merge = merge stmt in
        true, Ration (Partition.new_rationing ~limit ~merge)
      | _ ->
        (* No rationing. *)
        false, Ration (Partition.new_rationing ~limit:max_int ~merge:false)
    in
    let flow_actions =
      rationing_action :: Update_dynamic_splits :: flow_actions
    in
    flow_actions, store_results

  let call_return_policy =
    Partition.{
      callee_splits = Parameters.InterproceduralSplits.get ();
      callee_history = Parameters.InterproceduralHistory.get ();
      caller_history = true;
      history_size = history_size;
    }
end
