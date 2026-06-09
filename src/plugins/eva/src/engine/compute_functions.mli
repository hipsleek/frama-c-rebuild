(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Value analysis of entire functions, using Eva engine. *)

module Make (Engine : Engine_sig.S) :
  Engine_sig.Compute with type state = Engine.Dom.t
                      and type value = Engine.Val.t
                      and type loc = Engine.Loc.location
