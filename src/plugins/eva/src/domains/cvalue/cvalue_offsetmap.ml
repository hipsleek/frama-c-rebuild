(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

let offsetmap_of_v ~typ v =
  let size = Z.of_int (Cil.bitsSizeOf typ) in
  let v = Cvalue.V.anisotropic_cast ~size v in
  let v = Cvalue.V_Or_Uninitialized.initialized v in
  Cvalue.V_Offsetmap.create ~size v ~size_v:size

let offsetmap_of_assignment state expr = function
  | Copy (lv, _value) ->
    Bottom.non_bottom (Eval_op.offsetmap_of_loc lv.lloc state)
  | Assign value ->
    offsetmap_of_v ~typ:expr.Eva_ast.typ value
