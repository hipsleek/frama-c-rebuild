(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

open Pdg_types

val compute_annots: (unit -> stmt list)
(** Compute the impact analysis from the impact annotations in the program.
    Print and slice the results according to the parameters -impact-print
    and -impact-slice.
    @return the impacted statements *)

val from_stmt: (stmt -> stmt list)
(** Compute the impact analysis of the given statement.
    @return the impacted statements *)

val from_nodes:
  (kernel_function -> PdgTypes.Node.t list -> PdgTypes.NodeSet.t)
(** Compute the impact analysis of the given set of PDG nodes,
    that come from the given function.
    @return the impacted nodes *)

val slice: stmt list -> Project.t
