(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

[@@@ warning "-32" ]

open Pdg_types

type t = PdgTypes.Pdg.t

type t_nodes_and_undef =
  ((PdgTypes.Node.t * Memory_zone.t option) list * Memory_zone.t option)

exception Top = PdgTypes.Pdg.Top
exception Bottom = PdgTypes.Pdg.Bottom

(**************************************************************************)

let self = Pdg_tbl.self
let get = Pdg_tbl.get

let node_key = PdgTypes.Node.elem_key

let from_same_fun pdg1 pdg2 =
  let kf1 =  PdgTypes.Pdg.get_kf pdg1 in
  let kf2 =  PdgTypes.Pdg.get_kf pdg2 in
  Kernel_function.equal kf1 kf2

(**************************************************************************)
let find_decl_var_node = Sets.find_decl_var_node
let find_entry_point_node = Sets.find_entry_point_node
let find_top_input_node = Sets.find_top_input_node
let find_simple_stmt_nodes = Sets.find_simple_stmt_nodes
let find_stmt_and_blocks_nodes = Sets.find_stmt_and_blocks_nodes
let find_stmt_node = Sets.find_stmt_node
let find_label_node = Sets.find_label_node
let find_location_nodes_at_stmt = Sets.find_location_nodes_at_stmt
let find_location_nodes_at_begin = Sets.find_location_nodes_at_begin
let find_location_nodes_at_end = Sets.find_location_nodes_at_end
let find_call_ctrl_node = Sets.find_call_ctrl_node
let find_call_input_node = Sets.find_call_num_input_node
let find_call_output_node = Sets.find_call_output_node
let find_input_node = Sets.find_input_node
let find_ret_output_node = Sets.find_output_node
let find_output_nodes = Sets.find_output_nodes
let find_all_inputs_nodes = Sets.find_all_input_nodes

let find_call_stmts = Sets.find_call_stmts

let find_code_annot_nodes = Annot.find_code_annot_nodes
let find_fun_precond_nodes = Annot.find_fun_precond_nodes
let find_fun_postcond_nodes = Annot.find_fun_postcond_nodes
let find_fun_variant_nodes = Annot.find_fun_variant_nodes

let find_call_out_nodes_to_select = Sets.find_call_out_nodes_to_select
let find_in_nodes_to_select_for_this_call =
  Sets.find_in_nodes_to_select_for_this_call

(**************************************************************************)

let direct_dpds = Sets.direct_dpds
let direct_ctrl_dpds = Sets.direct_ctrl_dpds
let direct_data_dpds = Sets.direct_data_dpds
let direct_addr_dpds = Sets.direct_addr_dpds

let all_dpds = Sets.find_nodes_all_dpds
let all_ctrl_dpds = Sets.find_nodes_all_ctrl_dpds
let all_data_dpds = Sets.find_nodes_all_data_dpds
let all_addr_dpds = Sets.find_nodes_all_addr_dpds

let direct_uses = Sets.direct_uses
let direct_ctrl_uses = Sets.direct_ctrl_uses
let direct_data_uses = Sets.direct_data_uses
let direct_addr_uses = Sets.direct_addr_uses

let all_uses = Sets.all_uses

let custom_related_nodes = Sets.custom_related_nodes

let iter_nodes = PdgTypes.Pdg.iter_nodes

(**************************************************************************)


let extract = Pdg_tbl.print_dot

let pretty_node = Pdg_tbl.pretty_node
let pretty_key = Pdg_tbl.pretty_key
let pretty = Pdg_tbl.pretty

(**************************************************************************)

module Marks = Marks

(**************************************************************************)
