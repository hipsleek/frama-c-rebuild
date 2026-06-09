(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

module Loop_Max_Iteration: State_builder.Hashtbl with type key = stmt
                                                  and type data = int

val analyze: Kernel_function.t -> unit

val get_bounds: stmt -> int option

val fold_bounds: (stmt -> int -> 'a -> 'a) -> 'a -> 'a

val display_results: unit -> unit
