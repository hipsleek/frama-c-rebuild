(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Built-in Instance Tactical (auto-registered) *)

open Lang.F
open Tactical
open Strategy

val tactical : Tactical.t
val fields : selection field list
val params : parameter list
val filter : tau -> term -> bool

type bindings = (var * selection) list

val complexity : bindings -> Z.t
val cardinal : int -> bindings -> int option
(** less than limit *)

val instance_goal : ?title:string -> bindings -> pred -> Tactical.process
val instance_have : ?title:string -> ?at:int -> bindings -> pred -> Tactical.process
val wrap : selection field list -> selection list -> argument list

(** {2 Strategies} *)

val strategy : ?priority:float -> selection -> selection list -> strategy

(**************************************************************************)
