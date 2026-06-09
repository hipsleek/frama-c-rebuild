(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module provides hash functions.
    @since 33.0-Arsenic *)

(** [hash_iter iter hash x] hashes a collection [x] given an [iter] function on
    this collection and a [hash] function on its elements.
    @param limit is the maximum number of elements used for the hash; if
    collections bigger than this size are given, the remaining elements are
    ignored. Defaults to 16. *)
val hash_iter :
  ?limit:int ->
  (('a -> unit) -> 'b -> unit) -> ('a -> int) -> 'b -> int
