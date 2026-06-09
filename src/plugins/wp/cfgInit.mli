(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* Compute Init WP *)

module Make(W : Mcfg.S) :
sig

  val process_global_init : W.t_env -> kernel_function -> W.t_prop -> W.t_prop

end
