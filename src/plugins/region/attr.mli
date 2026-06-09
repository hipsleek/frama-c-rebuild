(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type flags
type attr = [
  | `Nullable (** Might be null *)
  | `Allocated  (** Might be dynamically allocated *)
  | `Garbage  (** Might be non-initialized *)
  | `Readonly (** Contains only readonly memory *)
]

val empty : flags
val add : attr -> flags -> flags
val mem : attr -> flags -> bool
val subset : flags -> flags -> bool
val union : flags -> flags -> flags (** Union of attributes *)

val bottom : flags
(** Neutral for merge *)

val merge : flags -> flags -> flags
(** Combine flags:
    - [`Nullable] if {i either} is readonly
    - [`Allocated] if {i either} is allocated
    - [`Garbage] if {i either} is garbage
    - [`Readonly] if {i both} are readonly
*)

val iter : (attr -> unit) -> flags -> unit

val pp_attr : Format.formatter -> attr -> unit
val pretty : Format.formatter -> flags -> unit

open Cil_types

val cvar : garbage:bool -> varinfo -> flags
val is_local : varinfo -> bool
val is_const : varinfo -> bool
val is_initialized : garbage:bool -> varinfo -> bool

val readable :
  loc:location -> ?label:logic_label ->
  from:flags -> term -> predicate
(** Whether the address is readable wrt. [~from] attributes *)

val writable :
  loc:location -> ?label:logic_label ->
  from:flags -> term -> predicate
(** Whether the address is writable wrt. [~from] attributes *)

val requires :
  loc:location -> ?label:logic_label ->
  ?readonly:bool -> from:flags -> target:flags -> term -> predicate
(** Whether the address satisfying [~from] attributes
    satisfies [~target] attributes. *)
