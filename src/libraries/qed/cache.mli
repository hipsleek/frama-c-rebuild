(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Simple Caches                                                      --- *)
(* -------------------------------------------------------------------------- *)

module type S =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

module type Cache =
sig
  type 'a value
  type 'a cache
  val create : size:int -> 'a cache
  val clear : 'a cache -> unit
  val compute : 'a cache -> 'a value -> 'a value
end

module Unary(A : S) : Cache with type 'a value = A.t -> 'a
module Binary(A : S) : Cache with type 'a value = A.t -> A.t -> 'a
