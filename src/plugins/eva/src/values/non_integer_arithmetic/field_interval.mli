(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type Name = sig val name : string end

module Make (K : Field.S) (Computation : IEEE754.Computation) :
  IEEE754.Abstraction
  with module Scalar = K
   and module Computation = Computation
   and type t = K.t Field.bounds
