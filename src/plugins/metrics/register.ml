(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Metrics_parameters

let syntactic () =
  let libc = Libc.get () in
  begin
    match AstType.get () with
    | "cil" -> Metrics_cilast.compute_on_cilast ~libc
    (* Cabs metrics are experimental. unregistered *)
    | "cabs" -> Metrics_cabs.compute_on_cabs ()
    | "acsl" -> Metrics_acsl.dump()
    | _ -> assert false (* the possible values are checked by the kernel*)
  end;
  SyntacticallyReachable.iter
    (fun kf ->
       let reachable = Metrics_coverage.compute_syntactic ~libc kf in
       let cov_printer = new Metrics_coverage.syntactic_printer ~libc reachable in
       Metrics_parameters.result "%a"
         cov_printer#pp_reached_from_function kf)

let syntactic_deps =
  [ Ast.self; AstType.self; OutputFile.self; SyntacticallyReachable.self;
    Libc.self ]

let syntactic_once, _ =
  State_builder.apply_once "Metrics.syntactic" syntactic_deps syntactic


let dkey_eva_coverage =
  Metrics_parameters.register_category "eva:coverage" ~default:true
    ~help:"print messages about Eva coverage"

let dkey_eva_unreached =
  Metrics_parameters.register_category "eva:unreached" ~default:true
    ~help:"print messages about function calls not reached by Eva"

let dkey_eva_reached_stmts =
  Metrics_parameters.register_category "eva:reached-stmts" ~default:true
    ~help:"print messages about statements reached by Eva"

let eva () =
  Eva.Analysis.compute ();
  if Eva.Analysis.is_computed () then begin
    let libc = Libc.get () in
    let cov_metrics = Metrics_coverage.compute ~libc in
    let cov_printer = new Metrics_coverage.semantic_printer ~libc cov_metrics in
    Metrics_parameters.result ~dkey:dkey_eva_coverage
      "%t" cov_printer#pp_value_coverage;
    Metrics_parameters.result ~dkey:dkey_eva_unreached
      "%t" cov_printer#pp_unreached_calls;
    Metrics_parameters.result ~dkey:dkey_eva_reached_stmts
      "%t" cov_printer#pp_stmts_reached_by_function;
  end

let eva_deps = [Eva.Analysis.self; Libc.self]
let eva_once, _ = State_builder.apply_once "Metrics.eva" eva_deps eva

let main () =
  if Enabled.get () then syntactic_once ();
  if ValueCoverage.get () then eva_once ();
  if LocalsSize.is_set () then begin
    Ast.compute ();
    Metrics_parameters.result "function\tlocals_size_no_temps\t\
                               locals_size_with_temps\t\
                               max_call_size_no_temps\t\
                               max_call_size_with_temps";
    LocalsSize.iter (fun kf -> Metrics_cilast.compute_locals_size kf);
  end;
  if UsedFiles.get () then begin
    let used_files = Metrics_cilast.used_files () in
    Metrics_cilast.pretty_used_files used_files
  end
;;

(* Register main entry points *)
let () = Boot.Main.extend main
