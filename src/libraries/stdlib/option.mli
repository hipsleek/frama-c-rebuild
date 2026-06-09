(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Extension of OCaml's {!Stdlib.Option} module. Be wary that the parameters
    order of the [bind] function are reversed compared to the standard library
    and that [get] takes an optional [exn] argument.
    @see https://frama-c.com/download/frama-c-plugin-development-guide.pdf
    @since 31.0-Gallium *)

include Monad.S_with_product with type 'a t = 'a option
include module type of Stdlib.Option

(** Make iterators to handle options of monadic elements and monadic options.
    @since 33.0-Arsenic
*)
module Make_monadic_iterators (M : Monad.S) : Monad.Iterators
  with type 'a iterable = 'a option
   and type 'a monad = 'a M.t

(** The call [opt <? default] is equivalent to [value ~default opt].
    @since 33.0-Arsenic
*)
val ( <? ) : 'a t -> 'a -> 'a

(** Reverse {!Stdlib.Option.bind} parameters for monad compatibility.
    [bind f o] is [f v] if [o] is [Some v] and [None] if [o] is [None].
*)
val bind: ('a -> 'b t) -> 'a t -> 'b t

(** Redefines {!Stdlib.Option.get} with a [exn] parameter.
    @raise Exn if the value is [None] and [exn] is specified.
    @raise Invalid_argument if the value is [None] and [exn] is not specified.
    @return v if the value is [Some v].
    @since 33.0-Arsenic
*)
val get: ?exn:exn -> 'a option -> 'a

(** [value_or_else ~none o] is similar to {!value} but uses a function to
    compute the default value.
    @since 33.0-Arsenic
*)
val value_or_else : none:(unit -> 'a) -> 'a option -> 'a

(** Compute a hash for the option given a hash for the element.
    @since 33.0-Arsenic *)
val hash: ('a -> int) -> 'a option -> int

(** Merges two options such that
    - [merge None None = None]
    - [merge (Some a) None = Some a]
    - [merge None (Some b) = Some b]
    - [merge (Some a) (Some b) = Some (f a b)]
      See also {!product} and {!map2} for other ways to combine options.
      @since 33.0-Arsenic *)
val merge: ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option

(** Maps two options such that
    - [map2 None None = None]
    - [map2 (Some a) None = None]
    - [map2 None (Some b) = None]
    - [map2 (Some a) (Some b) = Some (f a b)]
      See also {!product} and {!merge} for other ways to combine options.
      @since 33.0-Arsenic *)
val map2: ('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option

(** Same as {!Stdlib.Option.map} but avoid creating a copy of the option if the
    mapped function returns its argument (tested through physical equality).
    @since 33.0-Arsenic *)
val map_no_copy: ('a -> 'a) -> 'a option -> 'a option

(** [filter f (Some a)] applies [f] to [a] and returns [Some a] if [f a] is true
    or [None] if [f a] is false. [filter f None] always returns [None].
    @since 33.0-Arsenic *)
val filter: ('a -> bool) -> 'a option -> 'a option
