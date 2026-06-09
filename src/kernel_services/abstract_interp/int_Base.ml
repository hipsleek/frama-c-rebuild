(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type i = Z_or_top.t

include Z_or_top

let zero = `Value Z.zero
let one = `Value Z.one
let minus_one = `Value Z.minus_one
let neg = Lattice_bounds.Top.map Z.neg
let inject z = `Value z
let cardinal_zero_or_one z = not (is_top z)
