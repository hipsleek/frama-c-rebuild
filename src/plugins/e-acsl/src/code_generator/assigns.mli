(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

exception NoAssigns

val get_assigns_from :
  loc:Cil_types.location ->
  Env.t ->
  Cil_types.logic_var list ->
  Cil_types.logic_var ->
  Cil_types.exp list
(* @returns the list of expressions that are allowed to be used to assign the
   the result of a logic function *)

val get_assigned_var :
  loc:Cil_types.location -> is_gmp:bool -> Cil_types.varinfo -> Cil_types.term
(* @returns the expression that gets assigned when the result of the function is
   passed as an additional argument *)
