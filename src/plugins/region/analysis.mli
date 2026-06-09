(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** @raises Not_found *)
val find : kernel_function -> Code.domain

(** Memoized *)
val get : kernel_function -> Code.domain

(** Memoized *)
val compute : kernel_function -> unit

(** Hook on update *)
val add_hook : (unit -> unit) -> unit
