(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Sets of intervals with a lattice structure. Consecutive intervals are
    automatically fused. *)

(* For compilation reasons, the type of this module is in
   {!Int_Intervals_sig}, and the implementation is in
   {!Offsetmap.Int_Intervals}. *)

include Int_Intervals_sig.S
  with type t = Offsetmap.Int_Intervals.t
