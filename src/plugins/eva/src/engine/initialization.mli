(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Creation of the initial state of abstract domain. *)

(** Subset of [Engine_sig.S] required for this functor. *)
module type Engine_Subset = sig
  include Engine_abstractions_sig.S
  module Transfer_stmt : Engine_sig.Transfer_stmt with type state = Dom.t
end

module Make (Engine: Engine_Subset) :
  Engine_sig.Initialization with type state = Engine.Dom.t
