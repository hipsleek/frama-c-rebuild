(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module exposes two functors that, given a monad T called the
    "interior monad" and a monad S called the "exterior monad", build
    a monad of type ['a T.t S.t]. To be able to do so, one has to provide
    a [swap] function that, simply put, swap the exterior monad out of
    the interior one. In other word, this function allows fixing
    "badly ordered" monads compositions, in the sens that they are
    applied in the opposite order as the desired one.

    For example, one may want to combine the State monad and the Option
    monad to represent a stateful computation that may fail. To do so,
    one can either rewrite all the needed monadic operations, which may
    prove difficult, or use the provided functors of this module. Using
    the Option monad as the interior and the State monad as the exterior,
    one can trivially provide the following swap function:
    {[
      let swap (m : 'a State.t Option.t) : 'a Option.t State.t =
        match m with
        | None -> State.return None
        | Some s -> State.map Option.return s
    ]}
    Note here that trying to reverse the order of the Option and State
    monads makes the [swap] function way harder to write. Moreover, the
    resulting function does not actually satisfy the required axioms.

    Indeed, all [swap] functions will not result in a valid composed monad.
    To produce such a monad, the given [swap] function must verify the
    following equations:
    1. ∀t: 'a T.t, [swap (T.map S.return t) ≣ S.return t]
    2. ∀s: 'a S.t, [swap (T.return s) ≣ S.map T.return s]
    3. ∀x: 'a S.t S.t T.t, [swap (T.map S.flatten x) ≣ S.flatten (S.map swap (swap x))]
    4. ∀x: 'a S.t T.t T.t, [swap (T.flatten x) ≣ S.map T.flatten (swap (T.map swap x))]
    More details on this at the end of this file.
    @since 31.0-Gallium *)

module type Axiom = sig
  type 'a interior and 'a exterior
  val swap : 'a exterior interior -> 'a interior exterior
end

module Make
    (Interior : Monad.S)
    (Exterior : Monad.S)
    (_ : Axiom with type 'a interior = 'a Interior.t
                and type 'a exterior = 'a Exterior.t)
  : Monad.S with type 'a t = 'a Interior.t Exterior.t

module Make_with_product
    (Interior : Monad.S_with_product)
    (Exterior : Monad.S_with_product)
    (_ : Axiom with type 'a interior = 'a Interior.t
                and type 'a exterior = 'a Exterior.t)
  : Monad.S_with_product with type 'a t = 'a Interior.t Exterior.t


(** {3 Notes}

    Monads composition is a notoriously difficult topic, and no general
    approach exists. The one provided in this module is, in theory,
    quite restrictive as the [swap] function, also called a distributive
    law, has to satisfy the four presented axioms to guarantee that a
    valid monad can be built. Roughly speaking, those axioms enforce the
    idea that the distributive law must preserve all structures in the
    two monads.

    Distributive laws, their application to monads composition and weakenings
    of their axioms are a broad topic with profound implications in category
    theory. Even if none of this formal knowledge is required to use this
    module, one can check the following references to satisfy their curiosity.

    @see "Distributive laws" by Jon Beck for more details on this topic.
    @see "On the compositionality of monads via weak distributive laws" by
    Alexandre Goy thesis  for details on how to relax some of those axioms. *)
