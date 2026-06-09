(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Extension of OCaml's {!Stdlib.List} module.
    @see https://frama-c.com/download/frama-c-plugin-development-guide.pdf
    @since 31.0-Gallium
*)

(** {2 Monad } *)

include Monad.S_with_product with type 'a t = 'a list
include module type of Stdlib.List

(** Make iterators to handle lists of monadic elements and monadic lists.
    @since 33.0-Arsenic
*)
module Make_monadic_iterators (M : Monad.S) : Monad.Iterators
  with type 'a iterable = 'a list
   and type 'a monad = 'a M.t

(** {2 Datatype functions } *)

(** Compute a hash for the list given a hash for the elements.
    @since 33.0-Arsenic *)
val hash : ('a -> int) -> 'a t -> int

(** Pretty prints a list given a printer for the elements.
    @param format defaults to "[ %t ]"
    @param item defaults to "%a"
    @param sep defaults to ";@ "
    @param last defaults to [sep]
    @param empty defaults to "[]"
    @since 33.0-Arsenic *)
val pretty :
  ?format:(Pretty.tformatter -> unit) Pretty.format ->
  ?item:('a Pretty.aformatter -> 'a -> unit) Pretty.format ->
  ?sep:unit Pretty.format ->
  ?last:unit Pretty.format ->
  ?empty:unit Pretty.format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit

(** Pretty prints the list as a user readable text.
    @param format defaults to "%t"
    @param item defaults to "%a"
    @param sep defaults to ",@ "
    @param last defaults to "@ and@ "
    @param empty defaults to "<empty>"
    @since 33.0-Arsenic *)
val pretty_text:
  ?format:(Pretty.tformatter -> unit) Pretty.format ->
  ?item:('a Pretty.aformatter -> 'a -> unit) Pretty.format ->
  ?sep:unit Pretty.format ->
  ?last:unit Pretty.format ->
  ?empty:unit Pretty.format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit


(** {2 Iterators } *)

(** Returns the index (starting at 0) of the first element verifying the
    condition.
    Appears in Ocaml 5.1.
    @since 33.0-Arsenic *)
val find_index: ('a -> bool) -> 'a list -> int option

(** Same as {!Stdlib.List.map2} but gives the index of the current element to
    [f]
    @since 33.0-Arsenic *)
val mapi2 : (int -> 'a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** Same as {!Stdlib.List.map} but avoid creating a copy of the list's tail if
    the mapped function returns its argument (tested through physical equality).
    @since 33.0-Arsenic *)
val map_no_copy: ('a -> 'a) -> 'a list -> 'a list

(** Same as {!Stdlib.List.concat_map} but avoid creating a copy of the list's
    tail if the mapped function returns a singleton list with its argument
    (tested through physical equality).
    @since 33.0-Arsenic *)
val concat_map_no_copy: ('a -> 'a list) -> 'a list -> 'a list

(** {2 Accessors } *)

(** returns the unique element of a singleton list.
    @raise Invalid_argument on a non singleton list.
    @since 33.0-Arsenic *)
val as_singleton: 'a list -> 'a

(** returns the last element of a list.
    @raise Invalid_argument on an empty list
    @since 33.0-Arsenic *)
val last: 'a list -> 'a

(** [take n l] returns the first [n] elements of the list. Tail
    recursive.
    It returns an empty list if [n] is nonpositive and the whole list if [n] is
    greater than [List.length l].
    This function is introduced in OCaml 5.3 and is made available here until
    OCaml 5.4 is the minimal supported version. (The 5.3 version is raising
    exceptions on negative n values)
    It is equivalent to [slice ~last:n l].
    @since 33.0-Arsenic *)
val take : int -> 'a list -> 'a list

(** [drop n l] returns the list without the first [n] elements.
    It returns the whole list if [n] is nonpositive and an empty list if [n] is
    greater than [List.length l].
    This function is introduced in OCaml 5.3 and is made available here until
    OCaml 5.4 is the minimal supported version. (The 5.3 version is raising
    exceptions on negative n values)
    It is equivalent to [slice ~first:n l].
    @since 33.0-Arsenic *)
val drop : int -> 'a list -> 'a list

(** [break n l] returns a couple of the list of the first n elements and the
    list of the remaining elements. If n is smaller than 0 (resp. greater than
    the list length) then [([], l)] is returned (resp. [(l, [])]).
    It is equivalent to [(take n l, drop n l)].
    @since 33.0-Arsenic *)
val break : int -> 'a list -> ('a list * 'a list)

(** [slice ?first ?last l] is equivalent to Python's slice operator
    (l[first:last]): returns the range of the list between [first] (inclusive)
    and [last] (exclusive), starting from 0.
    If omitted, [first] defaults to 0 and [last] to [List.length l].
    Negative indices are allowed, and count from the end of the list.
    [slice] never raises exceptions: out-of-bounds arguments are clipped,
    and inverted ranges result in empty lists.
    @since 33.0-Arsenic *)
val slice: ?first:int -> ?last:int -> 'a list -> 'a list

(** {2 Mutators } *)

(** [replace cmp x l] replaces the first element [y] of [l] such that
    [cmp x y] is true by [x]. If no such element exists, [x] is added
    at the tail of [l].
    @since 33.0-Arsenic *)
val replace: ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list

(** {2 Product of lists } *)

(** [product_map f l1 l2] applies [f] to all the pairs of an elt of [l1] and
    an element of [l2].
    @since 33.0-Arsenic *)
val product_map: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(** [product_fold f acc l1 l2] is similar to [fold_left f acc l12] with l12 the
    list of all pairs of an elt of [l1] and an elt of [l2]
    @since 33.0-Arsenic *)
val product_fold: ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a

(** {2 Conversion } *)

(** converts a list with 0 or 1 element into an option.
    @raise Invalid_argument on lists with more than one argument
    @since 33.0-Arsenic *)
val to_option: 'a list -> 'a option

(** {2 Combinations } *)

(** [combinations k l] computes the combinations of [k] elements from list [l].
    E.g. [combinations 2 [1;2;3;4] = [[1;2];[1;3];[1;4];[2;3];[2;4];[3;4]]].
    This function preserves the order of the elements in [l] when
    computing the sublists. [l] should not contain duplicates.
    @since 33.0-Arsenic *)
val combinations: int -> 'a list -> 'a list list
