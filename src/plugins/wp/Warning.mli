(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Contextual Errors *)

exception Error of string * string (** Source, Reason *)

val error : ?source:string -> ('a,Format.formatter,unit,'b) format4 -> 'a

(** Warning Manager *)

type t = {
  loc : Filepos.t ;
  severe : bool ;
  source : string ;
  reason : string ;
  fallback : string ;
}

val compare : t -> t -> int
val pretty : Format.formatter -> t -> unit

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

val severe : Set.t -> bool

type context

val context : ?source:string -> unit -> context
val flush : context -> Set.t
val add : t -> unit

val create : ?log:bool -> ?severe:bool -> ?source:string -> fallback:string ->
  ('a,Format.formatter,unit,t) format4 -> 'a

val emit : ?severe:bool -> ?source:string -> fallback:string ->
  ('a,Format.formatter,unit) format -> 'a
(** Emit a warning in current context.
    Defaults: [severe=true], [source="wp"]. *)

val handle : ?severe:bool -> fallback:string -> handler:('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
(** Handle the error and emit a warning with specified severity and fallback
    if a context has been set.
    Otherwise, a WP-fatal error is raised instead.
    Default for [severe] is false.
    @before 30.0-Zinc [fallback] was [effect]
*)

type 'a outcome =
  | Result of Set.t * 'a
  | Failed of Set.t

val catch : ?source:string -> ?severe:bool -> fallback:string -> ('a -> 'b) -> 'a -> 'b outcome
(** Set up a context for the job. If non-handled errors are raised,
    then a warning is emitted with specified severity and fallback.
    Default for [severe] is [true]. *)
