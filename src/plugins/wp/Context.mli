(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Contextual Values *)

type 'a value

val create : ?default:'a -> string -> 'a value
(** Creates a new context with name *)

val defined : 'a value -> bool
(** The current value is defined. *)

val get : 'a value -> 'a
(** Retrieves the current value of the context.
    Raise an exception if not bound. *)

val get_opt : 'a value -> 'a option
(** Retrieves the current value of the context.
    Return [None] if not bound. *)

val set : 'a value -> 'a -> unit
(** Define the current value. Previous one is lost *)

val update : 'a value -> ('a -> 'a) -> unit
(** Modification of the current value *)

val bind : 'a value -> 'a -> ('b -> 'c) -> 'b -> 'c
(** Performs the job with local context bound to local value. *)

val free : 'a value -> ('b -> 'c) -> 'b -> 'c
(** Performs the job with local context cleared. *)

val clear : 'a value -> unit
(** Clear the current value. *)

val push : 'a value -> 'a -> 'a option
val pop : 'a value -> 'a option -> unit

val name : 'a value -> string

val register : (unit -> unit) -> unit
(** Register a global configure, to be executed once per project/ast. *)

val configure : unit -> unit
(** Apply global configure hooks, once per project/ast. *)
