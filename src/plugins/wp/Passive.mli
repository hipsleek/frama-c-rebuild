(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang.F

(** Passive Forms *)

type t

val empty : t
val is_empty : t -> bool
val union : t -> t -> t
val bind : fresh:var -> bound:var -> t -> t
val join : var -> var -> t -> t
val conditions : t -> (var -> bool) -> pred list
val apply : t -> pred -> pred

type binding =
  | Bind of var * var (* fresh , bound *)
  | Join of var * var (* left, right *)

val iter : (binding -> unit) -> t -> unit
val pretty : Format.formatter -> t -> unit
