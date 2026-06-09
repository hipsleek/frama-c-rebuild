(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Helper module to register read and written memory zones to {!Inout_access}
    in {!Transfer_stmt} and {!Transfer_specification} *)

module Make (Engine : Engine_abstractions_sig.S) :
  Engine_sig.Transfer_inout with type location = Engine.Loc.location
                             and type value = Engine.Val.t
                             and type valuation = Engine.Eval.Valuation.t
