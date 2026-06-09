(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Format
open Cil_types
open Cil_datatype

type 'a t = private
  | Pure
  | Dvar   of string
  | Ptr    of 'a
  | Array  of 'a t
  | Record of 'a t Fieldinfo.Map.t
  | Logic  of logic_type_info * 'a t list
  | Arrow  of 'a t list * 'a t

val is_pure : 'a t -> bool
val pretty : (formatter -> 'a -> unit) -> formatter -> 'a t -> unit

val pure : 'a t
val ptr : 'a -> 'a t
val scalar : 'a option -> 'a t
val array : 'a t -> 'a t
val field : fieldinfo -> 'a t -> 'a t
val record : 'a t Fieldinfo.Map.t -> 'a t
val logic : logic_type_info -> 'a t list -> 'a t
val arrow : 'a t list -> 'a t -> 'a t

val merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

(** Flattens and merge all pointed regions in the domain *)
val pointed : ('a -> 'a -> 'a) -> 'a t -> 'a option

val get_field : ('a -> 'a -> 'a) -> 'a t -> fieldinfo -> 'a t
val get_index : ('a -> 'a -> 'a) -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit

(** Polymorphic context *)
type 'a context

val empty : 'a context
val make : (string * 'a t) list -> 'a context

val of_ltype : (unit -> 'a) -> logic_type -> 'a t
val of_typ : (unit -> 'a) -> typ -> 'a t

type 'a sigma = 'a context ref
val unify : ('a -> 'a -> 'a) -> 'a sigma -> 'a t -> 'a t -> unit
val subst : 'a context -> 'a t -> 'a t
val getvar : ?default:'a t -> 'a context -> string -> 'a t
