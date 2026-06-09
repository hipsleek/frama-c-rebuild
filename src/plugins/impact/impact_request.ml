(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Server

let package = Package.package ~plugin:"impact" ~name:"impact" ~title:"Impact" ()

(* Convert the result of the impact analysis into a list of localizables. *)
let impact_to_localizable_list impact =
  let add_kf_nodes kf nodes acc =
    let stmts = Compute_impact.nodes_to_stmts nodes in
    let to_localizable stmt = Printer_tag.PStmtStart (kf, stmt) in
    (* Try to list statements in their order in the source code, as it is
       more natural for the user. *)
    List.map to_localizable stmts @ acc
  in
  Kernel_function.Map.fold add_kf_nodes impact []

let impact_statement stmt =
  let kf = Kernel_function.find_englobing_kf stmt in
  let skip = Compute_impact.skip () in
  let reason = Options.Reason.get () in
  let restrict = Memory_zone.top in
  let impact, _initial, _reason =
    Compute_impact.nodes_impacted_by_stmts ~skip ~restrict ~reason kf [stmt]
  in
  impact_to_localizable_list impact

let () =
  Request.register ~package
    ~kind:`GET ~name:"impactStatement"
    ~descr:(Markdown.plain "Compute the impact of a statement")
    ~input:(module Kernel_ast.Stmt)
    ~output:(module Data.Jlist (Kernel_ast.Marker))
    impact_statement
