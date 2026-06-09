(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Monad signature with let-bindings}

    This signature provides all the usual monadic operators along with
    let-bindings definitions used to simplify codes relying on monads.
    The provided operators are as follows:
    - [return] embeds a value [x] in the monad.
    - [bind] encodes the idea of "sequence" in the monadic world, i.e the
      call [bind f m] comes down to performing the computation [m] before
      applying the next computation step, represented by the function [f],
      to the resulting value.
    - [map] applies a function through the monad. One can exemplify it by
      making a parallel with the list [map] operator.
    - [flatten] is used to handle nested applications of the monad.

    The provided let-bindings operators can be used to write simpler and
    cleaner code. For example, one can write [let+ v = compute x in v + 1]
    instead of [map (fun v -> v + 1) (compute x)]. The more monadic steps,
    the simpler the code will get when written using those operators.
    In this module, [>>-] and [let*] always correspond to the [bind]
    operator while [>>-:] and [let+] always correspond to the [map].
    All those operators are provided in an [Operators] module to avoid
    namespace conflicts. Indeed, one can use the expression
    [let open MyMonad.Operators in] to use all the let-bindings without
    risking conflicts by including the other definitions, which have
    rather common names. This idiom also helps indicate which monad is
    currently used in a code.
    @since 31.0-Gallium *)
module type S = sig

  type 'a t
  val return : 'a -> 'a t
  val flatten : 'a t t -> 'a t
  val map  : ('a -> 'b  ) -> 'a t -> 'b t
  val bind : ('a -> 'b t) -> 'a t -> 'b t

  module Operators : sig
    val ( >>-  ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>-: ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end

end



(** {2 Monad signature with product}

    In a computational point of view, a monad is an abstraction of a sequence
    of operations. But sometimes, one may need to specify that two operations
    can be performed in *any* order, for instance when dealing with concurrency
    or generic errors handling using the [option] type. To do so, one needs
    a *product* on monadic values, i.e a way to combine two monads into a new
    one. Thus a second signature is provided, including a product operator
    and the two let-bindings [and*] and [and+].
    @since 31.0-Gallium *)
module type S_with_product = sig

  type 'a t
  val return : 'a -> 'a t
  val flatten : 'a t t -> 'a t
  val map  : ('a -> 'b  ) -> 'a t -> 'b t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t

  module Operators : sig
    val ( >>-  ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    val ( >>-: ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

end



(** {2 Building monads from minimal signatures}

    Below are functors for creating monads (with or without product). Both
    gives a strictly equivalent implementation but differ from the primitives
    given as argument. For an example of monad implementation based on this
    module, see  {!Option}. *)


(** {3 Minimal signature based on bind}

    This is the usual minimal definition of a monadic type constructor, also
    called a Kleisli triple. The [return] function embeds a value {m x} in
    the monad. The [bind] function, also called the "combinator", corresponds
    to the idea of "sequence" in the monadic world, i.e the call [bind f m]
    comes down to performing the computation [m] before applying the next
    computation step, represented by the function [f], to the resulting value.

    To fully qualify as a monad, these three parts must respect the three
    following laws :

    1. [return] is a left-identity for [bind] :
       ∀f:('a -> 'b t), ∀x:'a, [bind f (return x) ≣ return (f x)]

    2. [return] is a right-identity for [bind] :
       ∀m:'a t, [bind return m ≣ m]

    3. [bind] is associative :
       ∀m:'a t, ∀f:('a -> 'b t), ∀g:('b -> 'c t),
       [bind (fun x -> bind g (f x)) m ≣ bind g (bind f m)]

    As there is no way in the OCaml type system to enforce those properties,
    users have to trust the implemented monad when using it, and developers
    have to manually check that they are respected.
    @since 31.0-Gallium *)
module type Based_on_bind = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
end


(** {3 Minimal signature based on bind with product}

    This signature simply extends the previous one with a product operator.
    @since 31.0-Gallium *)
module type Based_on_bind_with_product = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
  val product : 'a t -> 'b t -> ('a * 'b) t
end


(** {3 Minimal definition based on map}

    In a computer science context, this is a rarer definition of a monadic
    type constructor. It is however equivalent to the Kleisli triple one,
    and may be simpler to use for certain monads, such as the [list] one.

    This approach requires three monadic operations. The first one, as in
    the Kleisli triple approach, is a [return] function, allowing to plundge
    values into the monad. The second one is a [map], that can be understand
    as a parallel of the [map] on list, i.e a way to apply a function through
    the monad. Finally, the third one is a [flatten] function, which allows
    to handle nested applications of the monad.

    As in the Kleisli triple approach, all those functions must satisfy some
    laws to fully qualify as a monad :

    1. ∀x:'a, ∀f:('a -> 'b), [return (f x) ≣ map f (return x)]

    2. ∀m:('a t t t), [flatten (map flatten m) ≣ flatten (flatten m)]

    3. ∀m:('a t), [flatten (map return m) ≣ flatten (return m) ≣ m]

    4. ∀m:('a t t), ∀f: ('a -> 'b), [flatten (map (map f) m) ≣ map f (flatten m)]

    As there is no way in the OCaml type system to enforce those properties,
    users have to trust the implemented monad when using it, and developers
    have to manually check that they are respected.

    More explanations on this approach on monads and its deep roots in the
    category theory can be found at the end of this file.
    @since 31.0-Gallium *)
module type Based_on_map = sig
  type 'a t
  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val flatten : 'a t t -> 'a t
end


(** {3 Minimal signature based on map with product}

    This signature simply extends the previous one with a product operator.
    @since 31.0-Gallium *)
module type Based_on_map_with_product = sig
  type 'a t
  val return : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val flatten : 'a t t -> 'a t
  val product : 'a t -> 'b t -> ('a * 'b) t
end


(** {3 Functors extending minimal signatures}

    Those functors provide a way to extend a minimal signature into a full
    monad that satisfies the signatures defined above. This is possible
    because one can define operations from one monadic definition
    using the operations required by the others. Indeed :

    1. ∀m:('a t), ∀f:('a -> 'b), [map f m ≣ bind (fun x -> return (f x)) m]

    2. ∀m:('a t t), [flatten m ≣ bind identity m]

    3. ∀m:('a t), ∀f:('a -> 'b t), [bind f m ≣ flatten (map f m)]

    All required laws expressed in both minimal signatures are respected
    using those definitions. *)

(** Extend a minimal monad based on bind.
    @since 31.0-Gallium *)
module Make_based_on_bind (M : Based_on_bind) :
  S with type 'a t = 'a M.t

(** Extend a minimal monad based on map.
    @since 31.0-Gallium *)
module Make_based_on_map (M : Based_on_map) :
  S with type 'a t = 'a M.t

(** Extend a minimal monad based on bind with product.
    @since 31.0-Gallium *)
module Make_based_on_bind_with_product (M : Based_on_bind_with_product) :
  S_with_product with type 'a t = 'a M.t

(** Extend a minimal monad based on map with product.
    @since 31.0-Gallium *)
module Make_based_on_map_with_product (M : Based_on_map_with_product) :
  S_with_product with type 'a t = 'a M.t


(** {3 Monadic iterators signature}

    Handling the interaction between monads and iterable data structures
    like lists can be difficult. On the other hand, all iterators
    concerning a monad over an iterable have the same signature, thus
    we provide a generic signature for all three standard iterators.
    @since 33.0-Arsenic *)
module type Iterators = sig
  type 'a iterable
  type 'a monad
  val fold : ('a -> 'b -> 'a monad) -> 'a -> 'b iterable -> 'a monad
  val map  : ('a -> 'b monad) -> 'a iterable -> 'b iterable monad
  val iter : ('a -> unit monad) -> 'a iterable -> unit monad
end


(** {3 Detailed explanations and category theory}

    To be pedantic, the map based approach defines a monad as a categoric
    functor equipped with two natural transformations. This does sound
    frightening but this breaks down to rather simple concepts.

    Let's start at the beginning. A category is just a collection of objects
    and arrows (or morphisms) between those objects that satisfies two
    properties: there exists a morphism from all objects to themselves, i.e
    an identity, and if there is a morphism between objects [a] and [b] and
    a morphism between objects [b] and [c], then there must be a morphism
    between [a] and [c], i.e morphisms are associative.

    There is a strong parallel between categories and type systems. Indeed,
    if one uses the collection of all types as objects, then for all types
    ['a] and ['b], the function [f : 'a -> 'b] can be seen as a morphism
    between the objects ['a] and ['b]. As functions are naturally associative
    and, for any type ['a], one can trivially defines the identity function
    ['a -> 'a], one can conclude that types along with all functions of
    arity one forms a category.

    Next, there is the idea of functors. In the category theory, a functor
    is a mapping between categories. That means that, given two categories
    [A] and [B], a functor maps all objects of [A] to an object of [B] and
    maps any morphism of [A] into a morphism of [B]. But, not all mappings
    are functors. Indeed, to be a valid functor, one has to preserve the
    identity morphisms and the composition of morphims.

    The idea of functors can also be seen in a type systems. At least, the
    more restricted but enough here of an endofunctor, a functor from a
    category to itself. Indeed, for any type [v] and for any parametric
    type ['a t], the type [v t] can be seen as a mapping from values of
    type [v] to values of type [v t]. Thus the type ['a t] encodes the first
    part of what a functor is, a mapping from objects of the Type category
    to objects of the Type category. For the second part, we need a way to
    transform morphisms of the Type category, i.e functions of type ['a -> 'b],
    in a way that preserves categoric properties. Expressed as a type, this
    transformation can be seen as a higher-order function [map] with the
    type [map : ('a -> 'b) -> ('a t -> 'b t)].

    Finally, this definition of a monad requires two natural transformations.
    Simply put, a natural transformation is a mapping between functors that
    preserves the structures of all the underlying categories (mathematicians
    and naming conventions...). That may be quite complicated to grasp, but in
    this case, the two required transformations are quite simple and intuitive.

    The first one is the [return] function that embeds values into the monad,
    which can be seen as a natural transformation from the identity functor
    to the monad functor. The second one is the [flatten] function, which
    is a transformation from two applications of the monad functor to the
    monad functor. With this two transformations, any number of application
    of the monad functor can be taken down to a unique application of the
    monad. *)
