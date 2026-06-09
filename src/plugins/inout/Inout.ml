(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let expr_inputs = Inputs.expr
let stmt_inputs = Inputs.statement
let kf_inputs = Inputs.get_internal
let kf_external_inputs = Inputs.get_external

let stmt_outputs = Outputs.statement
let kf_outputs = Outputs.get_internal
let kf_external_outputs = Outputs.get_external

let get_precise_inout = Operational_inputs.get_internal_precise

let states = [ Inputs.self; Outputs.self ]
let proxy = State_builder.Proxy.(create "inout" Both states)
let self = State_builder.Proxy.get proxy
