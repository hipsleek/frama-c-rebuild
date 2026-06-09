(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Merging Set Functor *)
(* -------------------------------------------------------------------------- *)

module type Elt =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(E : Elt) :
sig

  type elt = E.t

  type t = elt list Intmap.t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val is_empty : t -> bool
  val empty : t

  (* good sharing *)
  val add : elt -> t -> t
  val singleton : elt -> t
  val elements : t -> elt list

  (* good sharing *)
  val remove : elt -> t -> t

  val mem : elt -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val iter_sorted : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_sorted: (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool

  val union : t -> t -> t
  val inter : t -> t -> t
  val diff  : t -> t -> t
  val subset : t -> t -> bool
  val intersect : t -> t -> bool

  val of_list : elt list -> t
end
