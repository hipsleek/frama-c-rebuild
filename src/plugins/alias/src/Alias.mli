(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Interface for the Alias plug-in. *)

module Analysis: sig
  (** see file analysis.mli for documentation *)
  val compute : unit -> unit
  val clear : unit -> unit
end


module API = API
