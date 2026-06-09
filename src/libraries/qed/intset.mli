(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Set of integers using Patricia Trees.

    From the paper of Chris Okasaki and Andrew Gill:
    'Fast Mergeable Integer Maps'.
*)

type t

val compare : t -> t -> int
val equal : t -> t -> bool

val empty : t
val singleton : int -> t

val is_empty : t -> bool
val cardinal : t -> int
val elements : t -> int list

val mem : int -> t -> bool
val add : int -> t -> t
val remove :int -> t -> t
val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val subset : t -> t -> bool

val iter : (int -> unit) -> t -> unit
val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (int -> bool) -> t -> bool
val exists : (int -> bool) -> t -> bool
val filter : (int -> bool) -> t -> t
val partition : (int -> bool) -> t -> t * t

val intersect : t -> t -> bool
