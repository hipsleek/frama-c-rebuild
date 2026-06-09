(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Main numeric values of Eva that can be used by abstract domains. *)

(** Main abstract values built over Cvalue.V, used by most domains. *)
module CVal: Abstract_value.Leaf
  with type t = Cvalue.V.t and type context = unit
val cval: CVal.t Abstract_value.dependencies

(** Dummy intervals: no forward nor backward propagations,
    only used as a reduced product with CVal above. [None] is top. *)
module Interval: Abstract_value.Leaf
  with type t = Ival.t option and type context = unit
val ival: Interval.t Abstract_value.dependencies

(** Simple sign values, used by the sign domain. *)
module Sign: Abstract_value.Leaf
  with type t = Sign_value.t and type context = unit
val sign: Sign.t Abstract_value.dependencies
