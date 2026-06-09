(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type thread_id = int

(** Subset of [Engine_sig.S] required by this functor. *)
module type Engine_Subset = sig
  include Engine_abstractions_sig.S
  include Engine_sig.Results with type state := Dom.state
                              and type value := Val.t
                              and type location := Loc.location
end

module Make (Engine : Engine_Subset) :
  Engine_sig.Interferences with
  type state = Engine.Dom.t
