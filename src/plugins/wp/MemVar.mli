(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- No-Aliasing Memory Model                                           --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types

module type VarUsage =
sig
  val datatype : string
  val param : varinfo -> MemoryContext.param
  val iter: ?kf:kernel_function -> init:bool -> (varinfo -> unit) -> unit

end

(** VarUsage naive instance.
    It reports a by-value access for all variables. *)
module Raw : VarUsage

(** VarUsage that uses only Cil-Static infos. *)
module Static : VarUsage

(** Create a mixed Hoare Memory Model from VarUsage instance. *)
module Make(_ : VarUsage)(_ : Memory.Model) : Memory.Model
