(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type Conversion = sig
  type extended
  type internal
  val restrict : extended -> internal
end

module Make
    (Val: Abstract_value.Leaf)
    (Convert : Conversion with type internal := Val.context)
  : Abstract.Value.Internal with type t = Val.t
                             and type context = Convert.extended
