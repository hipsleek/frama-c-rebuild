(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let self = Functionwise.self
let compute = Functionwise.compute
let compute_all = Functionwise.compute_all
let is_computed = Functionwise.is_computed
let get = Functionwise.get
let pretty = Functionwise.pretty

let access zone mem = Eva.Assigns.Memory.find mem zone

let display fmt = From_register.display (Some fmt)

let compute_all_calldeps = Callwise.compute_all_calldeps
module Callwise = struct
  let iter = Callwise.iter
  let find = Callwise.find
end
