(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module type Input_Domain = sig
  include Abstract_domain.S
  val key: t Structure.Key_Domain.key
end

module type Conversion = sig
  type extended
  type internal
  val extend: internal -> extended
  val restrict: extended -> internal
end

module Make
    (Domain: Input_Domain)
    (Ctx: Conversion with type internal := Domain.context)
    (Val: Conversion with type internal := Domain.value)
    (Loc: Conversion with type internal := Domain.location)
  : Abstract.Domain.Internal with type state = Domain.state
                              and type context = Ctx.extended
                              and type value = Val.extended
                              and type location = Loc.extended
                              and type origin = Domain.origin
