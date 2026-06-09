(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Interpretation of function specification. *)

(** Subset of [Engine_sig.S] required by this functor. *)
module type Engine_Subset = sig
  include Engine_abstractions_sig.S

  (* Used to register read and written zones. *)
  module Transfer_inout : Engine_sig.Transfer_inout
    with type location = Loc.location
     and type value = Val.t
     and type valuation = Eval.Valuation.t

  (* Used to inject interferences in concurrent programs. *)
  module Interferences : Engine_sig.Interferences with type state = Dom.t

  (* Interpretation of pre- and post-conditions. *)
  module Transfer_logic : Engine_sig.Transfer_logic with type state = Dom.t
end

module Make (Engine: Engine_Subset) :
  Engine_sig.Transfer_specification with type state = Engine.Dom.t
                                     and type value = Engine.Val.t
                                     and type location = Engine.Loc.location
