(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let is_computed = Eva.Analysis.is_computed
let get_cvalue_state kinstr = Eva.Results.(before_kinstr kinstr |> get_cvalue_model)
