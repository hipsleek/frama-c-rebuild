(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Implementation of domain queries for the cvalue domain. *)
include Abstract_domain.Queries
  with type state = Cvalue.Model.t
   and type context = unit
   and type value = Main_values.CVal.t
   and type location = Main_locations.PLoc.location
   and type origin = Main_values.CVal.t

(** Evaluation engine specific to the cvalue domain. *)
include Evaluation_sig.S with type state := state
                          and type context := unit
                          and type value := value
                          and type loc := location
                          and type origin := origin

(** Evaluates the location of a lvalue in a given cvalue state. *)
val lval_to_loc: state -> Eva_ast.lval -> Locations.t
