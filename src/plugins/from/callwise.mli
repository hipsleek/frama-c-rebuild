(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Computation of callwise functional dependencies. The results are computed
    while the value analysis runs, and the results are usually much more
    precise than the functionwise results. *)

val compute_all_calldeps : unit -> unit
val iter : (Cil_types.kinstr -> Eva.Assigns.t -> unit) -> unit
val find : Cil_types.kinstr -> Eva.Assigns.t
val self : State.t
