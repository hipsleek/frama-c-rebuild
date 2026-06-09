(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Utilities function for aggregate types. *)

val get_array_typ_opt:
  typ -> (typ * exp option * attributes) option
(** @return the content of the array type if [ty] is an array, or None
    otherwise. *)

(** Represent the different types of aggregations. *)
type t =
  | StructOrUnion
  | Array
  | NotAggregate

val get_t: typ -> t
(** [get_t ty] returns [Array] if [ty] is an array type,
    [StructOrUnion] if [ty] is a struct or an union type and [NotAggregate]
    otherwise. *)
