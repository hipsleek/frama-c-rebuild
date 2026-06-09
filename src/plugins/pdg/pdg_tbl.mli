(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Pdg_types

type t = PdgTypes.Pdg.t

val self : State.t

val get : Cil_types.kernel_function -> t
(** Get the PDG of a function. Build it if it doesn't exist yet. *)

(** {3 Pretty printing} *)

val pretty_node : bool -> Format.formatter -> PdgTypes.Node.t -> unit
(** Pretty print information on a node : with [short=true], only the id
      of the node is printed.. *)

val pretty_key : Format.formatter -> PdgIndex.Key.t -> unit
(** Pretty print information on a node key *)

val pretty : ?bw:bool -> Format.formatter -> t -> unit
(** For debugging... Pretty print pdg information.
    Print codependencies rather than dependencies if [bw=true]. *)

val print_dot : t -> string -> unit
(** Pretty print pdg into a dot file. *)
