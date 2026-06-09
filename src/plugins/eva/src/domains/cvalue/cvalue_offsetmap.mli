(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary functions on cvalue offsetmaps, used by the cvalue domain. *)

open Cvalue

(** Computes the offsetmap for an assignment:
    - in case of a copy, extracts the offsetmap from the state;
    - otherwise, translates the value assigned into an offsetmap. *)
val offsetmap_of_assignment:
  Model.t -> Eva_ast.exp -> (Precise_locs.precise_location, V.t) Eval.assigned ->
  V_Offsetmap.t
