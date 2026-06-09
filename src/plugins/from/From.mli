(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** {3 Analysis} *)

val is_computed : kernel_function -> bool
val compute : kernel_function -> unit
val compute_all : unit -> unit

val get : Cil_types.kernel_function -> Eva.Assigns.t
val access : Memory_zone.t -> Eva.Assigns.Memory.t -> Memory_zone.t

val self : State.t

(** {3 Pretty-printing} *)

val pretty : Format.formatter -> kernel_function -> unit
val display : Format.formatter -> unit

(** {3 Callsite-wise analysis} *)

val compute_all_calldeps : unit -> unit
module Callwise : sig
  val iter : (Cil_types.kinstr -> Eva.Assigns.t -> unit) -> unit
  val find : Cil_types.kinstr -> Eva.Assigns.t
end
