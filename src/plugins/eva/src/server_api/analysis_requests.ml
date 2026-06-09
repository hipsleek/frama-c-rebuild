(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Server
open Cil_types

let package =
  let title = "Eva Analysis" in
  Package.package ~plugin:"eva" ~name:"analysis" ~title ()


(* ----- Computation state -------------------------------------------------- *)

module ComputationState = struct
  type t = Self.computation_state
  let jtype =
    Data.declare ~package
      ~name:"computationStateType"
      ~descr:(Markdown.plain "State of the computation of Eva Analysis.")
      Package.(Junion [
          Jtag "not_computed" ;
          Jtag "computing" ;
          Jtag "computed" ;
          Jtag "aborted" ])
  let to_json = function
    | Self.NotComputed -> `String "not_computed"
    | Computing -> `String "computing"
    | Computed -> `String "computed"
    | Aborted -> `String "aborted"
end

let computation_signal =
  States.register_framac_value ~package
    ~name:"computationState"
    ~descr:(Markdown.plain "The current computation state of the analysis.")
    ~output:(module ComputationState)
    (module Self.ComputationState)

let () = Request.register ~package
    ~kind:`EXEC
    ~name:"compute"
    ~descr:(Markdown.plain "run eva analysis")
    ~input:(module Data.Junit)
    ~output:(module Data.Junit)
    Analysis.compute

let () = Request.register ~package
    ~kind:`GET (* able to interrupt the EXEC compute request *)
    ~name:"abort"
    ~descr:(Markdown.plain "abort eva analysis")
    ~input:(module Data.Junit)
    ~output:(module Data.Junit)
    Analysis.abort

let register_computation_hook f =
  Self.ComputationState.add_hook_on_change (fun _ -> f ());
  Self.ComputationState.add_hook_on_update (fun _ -> f ())

let clear () =
  if Self.ComputationState.get () <> Computing then
    begin
      Self.clear_results ();
      Emitter.clear Eva_utils.emitter;
      Emitter.clear Eva_utils.export_emitter;
    end

let () = Request.register ~package
    ~kind:`SET
    ~name:"clear"
    ~descr:(Markdown.plain "removes all results from previous Eva analyses, \
                            including emitted alarms, annotations and statuses")
    ~input:(module Data.Junit)
    ~output:(module Data.Junit)
    clear


(* ----- Domains states ----------------------------------------------------- *)

let compute_lval_deps request lval =
  let zone = Results.lval_deps lval request in
  Memory_zone.get_bases zone

let compute_expr_deps request expr =
  let zone = Results.expr_deps expr request in
  Memory_zone.get_bases zone

let compute_instr_deps request = function
  | Set (lval, expr, _) ->
    Base.SetLattice.join
      (compute_lval_deps request lval)
      (compute_expr_deps request expr)
  | Local_init (vi, AssignInit (SingleInit expr), _) ->
    Base.SetLattice.join
      (Base.SetLattice.inject_singleton (Base.of_varinfo vi))
      (compute_expr_deps request expr)
  | _ -> Base.SetLattice.empty

let compute_stmt_deps request stmt =
  match stmt.skind with
  | Instr (instr) -> compute_instr_deps request instr
  | If (expr, _, _, _) -> compute_expr_deps request expr
  | _ -> Base.SetLattice.empty

let compute_marker_deps request = function
  | Printer_tag.PStmt (_, stmt)
  | PStmtStart (_, stmt) -> compute_stmt_deps request stmt
  | PLval (_, _, lval) -> compute_lval_deps request lval
  | PExp (_, _, expr) -> compute_expr_deps request expr
  | PVDecl (_, _, vi) -> Base.SetLattice.inject_singleton (Base.of_varinfo vi)
  | _ -> Base.SetLattice.empty

let get_filtered_state request marker =
  let bases = compute_marker_deps request marker in
  match bases with
  | Base.SetLattice.Top -> Results.print_states request
  | Base.SetLattice.Set bases ->
    if Base.Hptset.is_empty bases
    then []
    else Results.print_states ~filter:bases request

let get_state filter request marker =
  if filter
  then get_filtered_state request marker
  else Results.print_states request

let get_states (marker, filter) =
  let kinstr = Printer_tag.ki_of_localizable marker in
  match kinstr with
  | Kglobal -> []
  | Kstmt stmt ->
    let states_before = get_state filter (Results.before stmt) marker in
    let states_after = get_state filter (Results.after stmt) marker in
    match states_before, states_after with
    | [], _ -> List.map (fun (name, after) -> name, "", after) states_after
    | _, [] -> List.map (fun (name, before) -> name, before, "") states_before
    | _, _ ->
      let join (name, before) (name', after) =
        assert (name = name');
        name, before, after
      in
      List.rev_map2 join states_before states_after

let () = Request.register ~package
    ~kind:`GET ~name:"getStates"
    ~descr:(Markdown.plain "Get the domain states about the given marker")
    ~input:(module Data.Jpair (Kernel_ast.Marker) (Data.Jbool))
    ~output:(module Data.Jlist
          (Data.Jtriple (Data.Jstring) (Data.Jstring) (Data.Jstring)))
    ~signals:[computation_signal]
    get_states
