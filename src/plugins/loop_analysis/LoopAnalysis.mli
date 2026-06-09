(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Loop Analysis plugin. *)

open Cil_types

(** [Loop] exports functions related to the estimation of loop iteration bounds. *)
module Loop_analysis : sig
  val analyze: Kernel_function.t -> unit
  val get_bounds: stmt -> int option
  val fold_bounds: (stmt -> int -> 'a -> 'a) -> 'a -> 'a
end
