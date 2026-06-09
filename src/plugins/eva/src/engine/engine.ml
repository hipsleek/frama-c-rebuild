(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

module type S = Engine_sig.S_with_results


module Make_Domain (Abstract: Abstractions.S) = struct

  include Abstract.Dom

  (* Adds functions to access the cvalue component of the abstract domain. *)
  include Cvalue_domain.Getters (Abstract.Dom)

  (* Store used by the engine, built over Domain_store.S:
     - [register_state] joins the previous registered state (if any) at the
       given control point with the new computed state.
     - during the analysis, states are registered by callstack. At the end,
       for each function/statement, consolidated states are computed as the join
       of states registered for each callstack.
     - [get_state_by_callstack] uses multiple calls to [get_state] to return
       an associative list (callstack, state). *)
  module Store = struct

    let register_state callstack control_point state =
      match control_point with
      | Domain_store.Initial ->
        (* Only one possible initial state, which is also the consolidated state.
           Register it, as it is used to evaluate ACSL \init label. *)
        Store.set_state Initial state;
        Store.set_state ~callstack Initial state
      | _ ->
        (* Only register [state] for the given callstack; the consolidated state
           is computed afterward by [post_analysis]. *)
        let set = Store.set_state ~callstack control_point in
        match Store.get_state ~callstack control_point with
        | `Value previous_state -> set (join previous_state state)
        | `Bottom | `Top -> set state (* No state previously registered. *)

    (* Computes and registers consolidated state as the join of states
       registered for all possible callstacks at the given [control_point]. *)
    let compute_consolidated_state control_point =
      let open Lattice_bounds.TopBottom.Operators in
      let* callstacks = Store.callstacks control_point in
      let join a b = `Value (join a b) in
      let get_state callstack = Store.get_state ~callstack control_point in
      let aux acc callstack = TopBottom.join join (get_state callstack) acc in
      let+ joined_state = List.fold_left aux `Bottom callstacks in
      Store.set_state control_point joined_state;
      joined_state

    (* Consolidated states are not registered during the analysis, so computes
       it if no callstack. *)
    let get_state ?callstack control_point =
      match callstack, Store.get_state ?callstack control_point with
      | None, `Bottom -> compute_consolidated_state control_point
      | _, result -> result

    let get_state_by_callstack control_point =
      let open Lattice_bounds.TopBottom.Operators in
      let* callstacks = Store.callstacks control_point in
      let get_state callstack =
        match Store.get_state ~callstack control_point with
        | `Bottom -> None
        | `Top -> Some (callstack, Abstract.Dom.top)
        | `Value state -> Some (callstack, state)
      in
      let list = List.filter_map get_state callstacks in
      if list = [] then `Bottom else `Value list

    let compute_consolidated_states () =
      let compute_initial_state kf =
        ignore (compute_consolidated_state (Start kf));
      in
      let compute_stmt_state stmt =
        ignore (compute_consolidated_state (Before stmt));
        ignore (compute_consolidated_state (After stmt));
      in
      let compute_kf_states kf =
        match Function_calls.analysis_status kf with
        | Unreachable | Analyzed NoResults | Analyzed Partial -> ()
        | SpecUsed | Builtin _ -> compute_initial_state kf
        | Analyzed Complete ->
          compute_initial_state kf;
          let fundec = Kernel_function.get_definition kf in
          List.iter compute_stmt_state fundec.sallstmts
      in
      Globals.Functions.iter compute_kf_states
  end

  (* Computes consolidated states at the end of the analysis. *)
  let post_analysis final_state =
    if Parameters.JoinResults.get () then Store.compute_consolidated_states ();
    post_analysis final_state
end


module Make (Abstract: Abstractions.S) = struct

  module Eval' =
    Evaluation.Make (Abstract.Ctx) (Abstract.Val) (Abstract.Loc) (Abstract.Dom)

  module rec Transfer_inout' : Engine_sig.Transfer_inout =
    Transfer_inout.Make (Engine)

  and Transfer_stmt' : Engine_sig.Transfer_stmt =
    Transfer_stmt.Make (Engine)

  and Transfer_logic' : Engine_sig.Transfer_logic =
    Transfer_logic.Make (Engine.Dom)

  and Transfer_specification' : Engine_sig.Transfer_specification =
    Transfer_specification.Make (Engine)

  and Initialization' : Engine_sig.Initialization = Initialization.Make (Engine)
  and Iterator' : Engine_sig.Iterator  = Iterator.Make (Engine)
  and Compute' : Engine_sig.Compute = Compute_functions.Make (Engine)
  and Interference' : Engine_sig.Interferences = Interferences.Make (Engine)

  and Engine : Engine_sig.S_with_results
    with type Ctx.t = Abstract.Ctx.t
     and type Val.t = Abstract.Val.t
     and type Loc.location = Abstract.Loc.location
     and type Dom.state = Abstract.Dom.state =
  struct

    module Ctx = Abstract.Ctx
    module Val = Abstract.Val
    module Loc = Abstract.Loc

    module Dom = Make_Domain (Abstract)

    module Eval = Eval'
    module Transfer_inout = Transfer_inout'
    module Transfer_stmt = Transfer_stmt'
    module Transfer_logic = Transfer_logic'
    module Transfer_specification = Transfer_specification'
    module Initialization = Initialization'
    module Iterator = Iterator'
    module Compute = Compute'
    module Interferences = Interference'


    let find get (control_point: Domain_store.control_point) =
      if Self.is_computed ()
      then
        match control_point with
        | Initial -> get control_point
        | Start kf ->
          if Function_calls.is_called kf
          then get control_point
          else `Bottom
        | Before stmt | After stmt ->
          let kf = Kernel_function.find_englobing_kf stmt in
          match Function_calls.analysis_status kf with
          | Unreachable | SpecUsed | Builtin _ -> `Bottom
          | Analyzed NoResults -> `Top
          | Analyzed (Complete | Partial) -> get control_point
      else `Top

    let get_state ?callstack =
      find (Dom.Store.get_state ?callstack)

    let get_state_by_callstack control_point =
      find (Dom.Store.get_state_by_callstack) control_point

    let callstacks = Abstract.Dom.Store.callstacks

    let eval_expr state expr = Eval.evaluate state expr >>=: snd

    let copy_lvalue state expr = Eval.copy_lvalue state expr >>=: snd

    let eval_lval_to_loc state lv =
      let get_loc (_, loc) = loc in
      let for_writing = false in
      Eval.lvaluate ~for_writing state lv >>=: get_loc

    let eval_function state ?args lv =
      let list, alarms = Eval.eval_function lv ?args state in
      Lattice_bounds.TopBottom.map (List.map fst) list, alarms

    let assume_cond ~pos state cond positive =
      fst (Eval.reduce state cond positive) >>- fun valuation ->
      let dval = Eval.to_domain_valuation valuation in
      Dom.assume ~pos cond positive dval state
  end

  include Engine
end


let default = Abstractions.Config.of_list [Cvalue_domain.registered, None]
module DefaultAbstractions = (val Abstractions.make default)
module Default : Engine_sig.S_with_results = Make (DefaultAbstractions)


(* Reference to the current configuration (built by Abstractions.configure from
   the parameters of Eva regarding the abstractions used in the analysis) and
   the current Engine module. *)
let ref_engine = ref (default, (module Default : S))

(* Returns the current Engine module. *)
let current () = (module (val (snd !ref_engine)): S)

(* Set of hooks called whenever the current Engine module is changed.
   Useful for the GUI parts that depend on it. *)
module Engine_Hook = Hook.Build (struct type t = (module S) end)

(* Register a new hook. *)
let register_hook = Engine_Hook.extend

(* Sets the current Engine module for a given configuration.
   Calls the hooks above. *)
let set_current config (engine: (module S)) =
  Engine_Hook.apply (module (val engine): S);
  ref_engine := (config, engine)

(* Builds the Engine module corresponding to a given configuration *)
let make config =
  if Abstractions.Config.(equal config default) then (module Default : S)
  else
    let module Abstract = (val Abstractions.make config) in
    let module Engine = Make (Abstract) in
    (module Engine)

(* Builds the engine reference according to the parameters of Eva if
   necessary and sets it as the current engine. *)
let reset () =
  let config = Abstractions.Config.configure () in
  (* If the configuration has not changed, do not reset the engine but uses
     the reference instead. *)
  if Abstractions.Config.equal config (fst !ref_engine)
  then snd !ref_engine
  else
    let engine = make config in
    set_current config engine;
    engine

(* Resets the Engine reference when the current project is changed. *)
let () =
  let reset () = ignore @@ reset () in
  Project.register_after_set_current_hook ~user_only:true (fun _ -> reset ());
  Project.register_after_global_load_hook reset
