(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Very low-level abstract functorial caches. Do not use them unless
    you understand what happens in this module, and do not forget that
    those caches are not aware of projects. *)

(** [set_cache_size n] sets the size cache to 2^(8+n).
    Does not resize already created caches. *)
val set_cache_size: int -> unit

module type Cacheable = sig
  type t
  val hash : t -> int
  val sentinel : t
  val equal : t -> t -> bool
end

module type Result = sig
  type t
  val sentinel : t
end

module Symmetric_Binary(H : Cacheable)(R : Result): sig
  val clear : unit -> unit
  val merge : (H.t -> H.t -> R.t) -> H.t -> H.t -> R.t
end


module Binary_Predicate(H0 : Cacheable)(H1 : Cacheable): sig
  val clear : unit -> unit
  val merge : (H0.t -> H1.t -> bool) -> H0.t -> H1.t -> bool
end

module Symmetric_Binary_Predicate(H0 : Cacheable): sig
  val clear : unit -> unit
  val merge : (H0.t -> H0.t -> bool) -> H0.t -> H0.t -> bool
end


module Arity_One(H : Cacheable)(R : Result): sig
  val clear : unit -> unit
  val merge : (H.t -> R.t) -> H.t -> R.t
end

module Arity_Two(H0 : Cacheable)(H1 : Cacheable)(R : Result): sig
  val clear : unit -> unit
  val merge : (H0.t -> H1.t -> R.t) -> H0.t -> H1.t -> R.t
end

module Arity_Three(H0 : Cacheable)(H1 : Cacheable)(H2 : Cacheable)(R : Result):
sig
  val clear : unit -> unit
  val merge : (H0.t -> H1.t -> H2.t -> R.t) -> H0.t -> H1.t -> H2.t -> R.t
end
