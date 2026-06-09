(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** List with constant-time concat operation.
    @since Carbon-20101201
*)

type 'a t

val empty : 'a t
val elt : 'a -> 'a t
val add : 'a -> 'a t -> 'a t
val append : 'a t -> 'a -> 'a t
val list : 'a list -> 'a t
val ulist : 'a t list -> 'a t
val concat : 'a t -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
val umap : ('a -> 'b t) -> 'a t -> 'b t
val umap_list : ('a -> 'b t) -> 'a list -> 'b t

val iter : ('a -> unit) -> 'a t -> unit
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val find : ('a -> bool) -> 'a t -> 'a
val find_opt : ('a -> bool) -> 'a t -> 'a option
val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f b]: finds the first element [x] of the bag such
    that [f x] returns [Some y], and returns [Some y]. If no such
    element exists, returns None.
*)

val exists : ('a -> bool) -> 'a t -> bool
val for_all : ('a -> bool) -> 'a t -> bool

val filter : ('a -> bool) -> 'a t -> 'a t
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t

val length : 'a t -> int

val is_empty : 'a t -> bool
val singleton : 'a t -> 'a option

val elements : 'a t -> 'a list
(** Might have [n^2] complexity in worst cases.
    It might be better to use a Vector to reach linear complexity. *)

val sort : ('a -> 'a -> int) -> 'a t -> 'a list
(** The returned list preserves duplicates and order of equals elements.
    Uses Merge Sort (from standard List module),
    but might have [n^2] complexity in worst cases.
    It might be better to use a Vector to reach linear complexity. *)
