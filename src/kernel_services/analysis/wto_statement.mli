(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Specialization of WTO for the CIL statement graph. See the Wto module
    for more details *)

open Cil_types

(** A weak topological ordering where nodes are Cil statements *)
type wto = stmt Wto.partition

(** The datatype for statement WTOs *)
module WTO : Datatype.S with type t = wto

(** @return the computed wto for the given function *)
val wto_of_kf : kernel_function -> wto


(** the position of a statement in a wto given as the list of
    component heads *)
type wto_index = stmt list

(** Datatype for  wto_index *)
module WTOIndex : Datatype.S with type t = wto_index

(** @return the wto_index for a statement *)
val wto_index_of_stmt : stmt -> wto_index

(** @return the components left and the components entered when going from
    one index to another *)
val wto_index_diff : wto_index -> wto_index -> stmt list * stmt list

(** @return the components left and the components entered when going from
    one stmt to another *)
val wto_index_diff_of_stmt : stmt -> stmt -> stmt list * stmt list
