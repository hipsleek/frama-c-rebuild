(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Partition

let dkey = Self.dkey_partition

module Make
    (Abstract: Engine_abstractions_sig.S)
    (Kf : sig val kf: kernel_function end) =
struct
  module Partition_parameters = Partitioning_parameters.Make (Kf)

  open Kf
  open Partition_parameters

  module Domain = Abstract.Dom

  module Index = Partitioning_index.Make (Domain)
  module Flow = Partition.MakeFlow (Abstract)

  type state = Domain.t

  type store = {
    flow_actions : action list; (* partitioning actions to be applied *)
    rationing: bool; (* Is there a slevel rationing in above actions? *)
    store_stmt : stmt option;
    store_is_loop_head : bool;
    store_index : Index.t; (* Index of all states stored: used to quickly remove
                              new states that have already been propagated. *)
    mutable store_partition : state partition; (* partition of states *)
    mutable incoming_states : int; (* number of incoming states. *)
  }

  type flow = Flow.t

  type tank = {
    mutable tank_states : state partition;
  }

  type widening_state = {
    mutable widened_state : state option;
    mutable previous_state : state;
    mutable widening_counter : int;
    mutable widening_steps : int; (* count the number of successive widenings *)
  }

  type widening = {
    widening_stmt : stmt;
    mutable widening_partition : widening_state partition;
  }

  (* Constructors *)

  let empty_store (v : Eva_automata.vertex) : store =
    let flow_actions, rationing = flow_actions v in
    {
      flow_actions;
      rationing;
      store_stmt = Eva_automata.Vertex.stmt v;
      store_is_loop_head = Eva_automata.Vertex.is_loop_head v;
      store_index = Index.empty ();
      store_partition = Partition.empty;
      incoming_states = 0;
    }

  let empty_flow : flow = Flow.empty

  let empty_tank () : tank =
    { tank_states = Partition.empty }

  let empty_widening (vertex : Eva_automata.vertex) : widening =
    let stmt = Eva_automata.Vertex.stmt vertex in
    {
      widening_stmt = Option.value ~default:Cil.invalidStmt stmt;
      widening_partition = Partition.empty;
    }

  let initial_tank (states : state list) : tank =
    let flow = Flow.initial states in
    (* Split the initial partition according to the global split settings *)
    let states = List.fold_left Flow.transfer_keys flow universal_splits in
    { tank_states = Flow.to_partition states }


  (* Pretty printing *)

  let pretty_store (fmt : Format.formatter) (s : store) : unit =
    Partition.iter (fun _key state -> Domain.pretty fmt state) s.store_partition

  let pretty_flow (fmt : Format.formatter) (flow : flow) =
    Flow.iter (fun _ -> Domain.pretty fmt) flow


  (* Accessors *)

  let expanded (s : store) : (key * state) list =
    Partition.to_list s.store_partition

  let smashed (s : store) : state Lattice_bounds.or_bottom =
    match expanded s with
    | [] -> `Bottom
    | (_k, v1) :: l ->
      let f acc (_k, v) = Domain.join acc v in
      `Value (List.fold_left f v1 l)

  let contents (flow : flow) : (key * state) list =
    Flow.to_list flow

  let is_empty_store (s : store) : bool =
    Partition.is_empty s.store_partition

  let is_empty_flow (flow : flow) : bool =
    Flow.is_empty flow

  let is_empty_tank (t : tank) : bool =
    Partition.is_empty t.tank_states

  let store_size (s : store) : int =
    Partition.size s.store_partition

  let flow_size (flow : flow) : int =
    Flow.size flow

  let tank_size (t : tank) : int =
    Partition.size t.tank_states


  (* Partition transfer functions *)

  let add_disjunction_keys stmt key states =
    let add_key =
      if List.compare_length_with states 1 <= 0 then
        fun _ s -> key, s
      else
        fun i s ->
          let branch = Disjunction_case (stmt, i) in
          Partition.Key.add_branch ~history_size branch key, s
    in
    List.mapi add_key states

  let enter_loop (flow : flow) (loop : Eva_automata.loop) : flow =
    Flow.transfer_keys flow (Enter_loop (unroll loop, loop))

  let leave_loop (flow : flow) (_loop : Eva_automata.loop) : flow =
    Flow.transfer_keys flow Leave_loop

  let next_loop_iteration (flow : flow) (_loop : stmt) : flow =
    Flow.transfer_keys flow Incr_loop

  let call_return ~caller kind result =
    let policy = call_return_policy in
    let callee_history =
      policy.callee_history || kind = `Spec || kind = `Builtin
    in
    let policy = { policy with callee_history } in
    let combine = Partition.Key.combine ~policy in
    List.map (fun (k, s) -> combine ~caller ~callee:k, s) result


  (* Reset state (for hierarchical convergence) *)

  let reset_store (s : store) : unit =
    let is_eternal key _state = not (Key.exceed_rationing key) in
    s.store_partition <- Partition.filter is_eternal s.store_partition

  let reset_tank (t : tank) : unit =
    t.tank_states <- Partition.empty

  let reset_widening (w : widening) : unit =
    w.widening_partition <- Partition.empty

  let reset_widening_counter (w : widening) : unit =
    let reset w =
      { w with widening_counter = max w.widening_counter (widening_period - 1) }
    in
    w.widening_partition <- Partition.map reset w.widening_partition


  (* Operators *)

  let drain (t : tank) : flow =
    let flow = Flow.of_partition t.tank_states in
    t.tank_states <- Partition.empty;
    flow

  let fill ~(into : tank) (flow : flow) : unit =
    let new_states = Flow.to_partition flow in
    let join _key dest src = match dest, src with
      | Some dest, Some src -> Some (Domain.join dest src)
      | Some v, None | None, Some v -> Some v
      | None, None -> None
    in
    into.tank_states <- Partition.merge join into.tank_states new_states

  let transfer = Flow.transfer

  let output_slevel : int -> unit =
    let slevel_display_step = Parameters.ShowSlevel.get () in
    let max_displayed = ref 0 in
    fun x ->
      if x >= !max_displayed + slevel_display_step
      then
        let rounded = x / slevel_display_step * slevel_display_step in
        Self.feedback ~dkey ~once:true ~current:true
          "Trace partitioning superposing up to %d states"
          rounded;
        max_displayed := rounded

  let partitioning_feedback dest flow stmt =
    output_slevel dest.incoming_states;
    (* Debug information. *)
    Self.debug ~dkey:Self.dkey_iterator ~current:true
      "reached statement %d with %d incoming states, %d to propagate"
      stmt.sid dest.incoming_states (flow_size flow)

  let join (sources : (int*flow) list) (dest : store) : flow =
    (* Get every source flow *)
    let sources_states =
      (* Is there more than one non-empty incoming flow? *)
      match sources with
      | [(_,flow)] -> [flow]
      | sources ->
        (* Several branches -> partition according to the incoming branch *)
        let get (b,flow) =
          Flow.transfer_keys flow (Add_branch (b,history_size))
        in
        List.map get sources
    in
    (* Merge incoming flows *)
    let flow_states =
      List.fold_left Flow.union Flow.empty sources_states
    in
    (* Execute actions *)
    let flow_states =
      List.fold_left Flow.transfer_keys flow_states dest.flow_actions
    in
    (* Add the propagated state to the store *)
    let add_state key s =
      dest.store_partition <- Partition.replace key s dest.store_partition;
    in
    if not (dest.rationing) then begin
      Flow.iter add_state flow_states;
      flow_states
    end else begin
      (* Handle ration stamps *)
      dest.incoming_states <- dest.incoming_states + Flow.size flow_states;
      (* Add states to the store but filter out already propagated states *)
      let update key current_state =
        (* Inclusion test *)
        let state =
          try
            let previous_state = Partition.find key dest.store_partition in
            if Domain.is_included current_state previous_state then
              (* The current state is included in the previous; stop *)
              None
            else begin
              (* Propagate the join of the two states *)
              if dest.store_is_loop_head then
                Self.feedback ~dkey ~once:true ~current:true
                  "starting to merge loop iterations";
              Some (Domain.join previous_state current_state)
            end
          with
          (* There is no previous state, propagate normally *)
            Not_found -> Some current_state
        in
        Option.iter (add_state key) state;
        (* Filter out already propagated states. *)
        Option.filter (fun s -> Index.add s dest.store_index) state
      in
      let flow = Flow.join_duplicate_keys flow_states in
      let flow = Flow.filter_map update flow in
      Option.iter (partitioning_feedback dest flow) dest.store_stmt;
      flow
    end

  let widen (w : widening) (flow : flow) : flow =
    let stmt = w.widening_stmt in
    (* Apply widening to each leaf *)
    let widen_one key curr =
      try
        (* Search for an already existing widening state *)
        let w = Partition.find key w.widening_partition in
        let previous_state = w.previous_state in
        (* Update the widening state *)
        w.previous_state <- curr;
        w.widening_counter <- w.widening_counter - 1;
        (* Propagated state decreases, stop propagating *)
        if Domain.is_included curr previous_state then
          None
          (* Widening is delayed *)
        else if w.widening_counter >= 0 then
          Some curr
          (* Apply widening *)
        else begin
          Self.feedback ~once:true ~current:true ~dkey:Self.dkey_widening
            "applying a widening at this point";
          (* We join the previous widening state with the previous iteration
             state so as to allow the intermediate(s) iteration(s) (between
             two widenings) to stabilize at least a part of the state. *)
          let prev = match w.widened_state with
            | Some v -> Domain.join previous_state v
            | None -> previous_state
          in
          let next = Domain.widen kf stmt prev (Domain.join prev curr) in
          w.previous_state <- next;
          w.widened_state <- Some next;
          w.widening_counter <- widening_period - 1;
          w.widening_steps <- w.widening_steps + 1;
          Statistics.(grow max_widenings) stmt w.widening_steps;
          Some next
        end
      with Not_found ->
        (* The key is not in the widening state; add the state if slevel is
           exceeded. *)
        if Key.exceed_rationing key then begin
          let ws =
            { widened_state = None;
              previous_state = curr;
              widening_counter = widening_delay - 1;
              widening_steps = 0
            }
          in
          w.widening_partition <- Partition.replace key ws w.widening_partition
        end;
        Some curr
    in
    let flow = Flow.join_duplicate_keys flow in
    Flow.filter_map widen_one flow
end
