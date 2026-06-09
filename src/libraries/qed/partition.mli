(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Union-find based partitions *)

module type Elt =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module type Set =
sig
  type t
  type elt
  val singleton : elt -> t
  val iter : (elt -> unit) -> t -> unit
  val union : t -> t -> t
  val inter : t -> t -> t
end

module type Map =
sig
  type 'a t
  type key
  val empty : 'a t
  val is_empty : 'a t -> bool
  val find : key -> 'a t -> 'a
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end


module Make(E : Elt)
    (S : Set with type elt = E.t)
    (_ : Map with type key = E.t) :
sig
  type t
  type elt = E.t
  type set = S.t

  val empty : t
  val equal : t -> elt -> elt -> bool
  val merge : t -> elt -> elt -> t
  val merge_list : t -> elt list -> t
  val merge_set : t -> set -> t
  val lookup : t -> elt -> elt
  val members : t -> elt -> set
  val iter : (elt -> set -> unit) -> t -> unit
  val unstable_iter : (elt -> elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val is_empty : t -> bool
end
