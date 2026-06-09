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
  val extend : internal -> extended
  val replace : internal -> extended -> extended
  val restrict : extended -> internal
end

module Make
    (Loc: Abstract_location.Leaf)
    (Convert : Conversion with type internal := Loc.value)
  : Abstract.Location.Internal with type location = Loc.location
                                and type offset = Loc.offset
                                and type value = Convert.extended
