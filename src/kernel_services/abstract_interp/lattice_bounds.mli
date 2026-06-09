(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Types, monads and utility functions for lattices in which the bottom
    and/or the top are managed separately from other values. *)

type 'a or_bottom = [ `Value of 'a | `Bottom ]
type 'a or_top = [ `Value of 'a | `Top ]
type 'a or_top_bottom = [ `Value of 'a | `Bottom | `Top ]


module Bottom : sig

  include Monad.S_with_product with type 'a t = 'a or_bottom

  (** Datatype constructor *)
  module Make_Datatype (X: Datatype.S) :
    Datatype.S_with_collections with type t = X.t or_bottom

  (** Bounds a semi-lattice *)
  module Bound_Lattice (Lattice: Lattice_type.Join_Semi_Lattice) : sig
    include Datatype.S_with_collections with type t = Lattice.t or_bottom
    include Lattice_type.Bottom_Bounded_Join_Semi_Lattice with type t := t
  end

  (** Access *)
  val is_bottom : 'a t -> bool
  val non_bottom : 'a t -> 'a
  val value : bottom:'a -> 'a t -> 'a

  (** Datatype *)
  val hash : ('a -> int) -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int)  -> 'a t -> 'a t -> int

  (** Pretty-printing *)
  val pretty_bottom : Format.formatter -> unit (* for %t specifier *)
  val pretty : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** Lattice operators *)
  val is_included : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val join : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val narrow : ('a -> 'a -> 'a t) -> 'a t -> 'a t -> 'a t
  val join_list : ('a -> 'a -> 'a) -> 'a t list -> 'a t

  (** Iterator. *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** In a lattice where the elements are lists of non-bottom values,
      the empty list is the bottom case. *)

  (**  Conversion from Bottom to None. *)
  val to_option : 'a t -> 'a option
  val of_option : 'a option -> 'a t

  (** Joins all elements of a list. Returns Bottom if the list is empty. *)
  val of_list : join:('a -> 'a -> 'a) -> 'a list -> 'a t

  (** Returns an empty list on Bottom, and a list of one element otherwise. *)
  val to_list : 'a t -> 'a list

  (** Filters the given list to remove Bottom elements. *)
  val list_values : 'a t list -> 'a list

  (** [list_filter_map f l] maps the function [f] to the list [l] and does not
      keep bottoms. *)
  val list_filter_map : ('a -> 'b t) -> 'a list -> 'b list

  (** [add_to_list elt list] adds [elt] to the [list] if it is not bottom. *)
  val add_to_list : 'a t -> 'a list -> 'a list

end


module Top : sig

  include Monad.S_with_product with type 'a t = 'a or_top

  (** Datatype constructor *)
  module Make_Datatype (X: Datatype.S) :
    Datatype.S_with_collections with type t = X.t or_top

  (** Bounds a semi-lattice *)
  module Bound_Lattice (Lattice: Lattice_type.Join_Semi_Lattice) : sig
    include Datatype.S_with_collections with type t = Lattice.t or_top
    include Lattice_type.Top_Bounded_Join_Semi_Lattice with type t := t
  end

  (** Access *)
  val is_top : 'a t -> bool
  val non_top : 'a t -> 'a
  val value : top:'a -> 'a t -> 'a

  (** Datatype *)
  val hash : ('a -> int) -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int)  -> 'a t -> 'a t -> int

  (** Pretty-printing *)
  val pretty_top : Format.formatter -> unit (* for %t specifier *)
  val pretty :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit

  (** Lattice operators *)
  val is_included : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val join : ('a -> 'a -> 'a t) -> 'a t -> 'a t -> 'a t
  val narrow : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** Iterator *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** Conversion *)
  val to_option : 'a t -> 'a option
  val of_option : 'a option -> 'a t

end


module TopBottom: sig

  type 'a t = 'a or_top_bottom
  val return : 'a -> 'a t
  val product : [< 'a t] -> [< 'b t] -> [> ('a * 'b) t]
  val bind : ('a -> ([> 'b t] as 'c)) -> [< 'a t] -> 'c
  val map : ('a -> 'b) -> [< 'a t] -> [> 'b t]
  val flatten : [< 'a t t] -> [> 'a t]

  (** Datatype constructor *)
  module Make_Datatype (X: Datatype.S) :
    Datatype.S_with_collections with type t = X.t or_top_bottom

  (** Operators. In presence of simultaneous `Bottom and `Top in and+ / and*,
      everything narrows down to `Bottom. Every operators is redefined to
      ensure subtyping properties. *)
  module Operators : sig

    (* Binding operators, applicative syntax *)
    val (>>-:) : [< 'a t] -> ('a -> 'b) -> [> 'b t]
    val (let+) : [< 'a t] -> ('a -> 'b) -> [> 'b t]
    val (and+) : [< 'a t] -> [< 'b t] -> [> ('a * 'b) t]

    (* Binding operators, monad syntax *)
    val (>>- ) : [< 'a t] -> ('a -> ([> 'b t] as 'c)) -> 'c
    val (let*) : [< 'a t] -> ('a -> ([> 'b t] as 'c)) -> 'c
    val (and*) : [< 'a t] -> [< 'b t] -> [> ('a * 'b) t]

  end

  (** Datatype *)
  val hash : ('a -> int) -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int)  -> 'a t -> 'a t -> int

  (** Pretty-printing *)
  val pretty :
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a t -> unit

  (** Lattice operators *)
  val is_included : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val join : ('a -> 'a -> [< 'a t]) -> 'a t -> 'a t -> 'a t
  val narrow : ('a -> 'a -> [< 'a t]) -> 'a t -> 'a t -> 'a t

end
