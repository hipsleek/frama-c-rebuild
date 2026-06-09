(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Lattice_bounds.Top.Make_Datatype (Z)

let top = `Top
let of_int i = `Value (Z.of_int i)

let is_zero = equal (`Value Z.zero)
let is_top = Lattice_bounds.Top.is_top

let inject i = `Value i
let project = function
  | `Top -> raise Abstract_interp.Error_Top
  | `Value i -> i
