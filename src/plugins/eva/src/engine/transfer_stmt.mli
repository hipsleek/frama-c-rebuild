(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val current_kf_inout: unit -> Inout_type.t option

(** Subset of [Engine_sig.S] required for this functor. *)
module type Engine_Subset = sig
  include Engine_abstractions_sig.S

  (* Used to register read and written zones. *)
  module Transfer_inout : Engine_sig.Transfer_inout
    with type location = Loc.location
     and type value = Val.t
     and type valuation = Eval.Valuation.t

  (* Used to inject interferences in concurrent programs. *)
  module Interferences : Engine_sig.Interferences with type state = Dom.t

  (** Used to interpret function calls. *)
  module Compute : Engine_sig.Compute with type state = Dom.t
                                       and type value = Val.t
                                       and type loc = Loc.location
end

module Make (Engine: Engine_Subset) :
  Engine_sig.Transfer_stmt with type state = Engine.Dom.t
