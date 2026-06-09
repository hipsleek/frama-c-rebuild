(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** The goal of this module is to provide a way to build a sound
    overapproximation of the semantics of IEEE-754 correctly rounded
    operations for floating-point formats supported by the C language.

    To do so, set of floating-point numbers are soundly abstracted by a
    triplet composed of:
    - an abstraction {m \mathbb{R}(x)} of the theoretical computation of {m x}
      using real numbers;
    - an abstraction {m \varepsilon_a^f(x)} of the absolute errors committed when
      computing {m x} using floating-point numbers in the format {m f};
    - an abstraction {m \varepsilon_r^f(x)} of the relative errors committed when
      computing {m x} using floating-point numbers in fhe format {m f}.

    Two sound abstract overapproximations of sets of real numbers are required
    to build a sound overapproximation of IEEE-754 semantics. The first one,
    called {m \mathbb{A}}, is used to abstract the real numbers and the absolute
    floating-point errors. As absolute error semantics is mostly linear, it is
    preferable to provide an abstraction as precise as possible on linear
    operations, like affine forms for example. The second required abstraction,
    called {m \mathbb{M}}, is used to abstract the relative errors. As their
    semantics heavily relies on multiplications and divisions, it is preferable
    to provide an abstraction as precise as possible on those operations, like
    the relative forms described {{:https://theses.hal.science/tel-03566701}
    here}. Moreover, the relative error semantics for additions and subtractions
    is defined based on an expression of the form {m (ax \pm by) / (x \pm y)}.
    This expression can be precisely computed even in intervals, as described
    {{:https://theses.hal.science/tel-01094485v1} here}, and is optimal if
    there is no relation between {m a}, {m b}, {m x} and {m y}. Thus, an
    implementation of this computation is also required.

    Another key part of the precision of the built semantics is the relations
    between absolute and relative errors. Indeed, each one can be derived from
    the other as follows:
    - {m \varepsilon_a^f(x) = \mathbb{R}(x) \varepsilon_r^f(x)}
    - {m \varepsilon_r^f(x) = \varepsilon_a^f(x) / \mathbb{R}(x)}

    Thus, a sound abstraction of those computations are required to define a
    correct reduced product between the two error abstractions. As one may need
    to partially or totally disable this reduced product, for experimentation
    purposes for instance, two functions are required to build the semantics,
    returning true if the absolute (resp. relative) error should be reduced using
    the other.

    Finally, as relational abstractions may need to keep track of each source of
    floating-point errors (i.e. each elementary error), one is also asked to provide
    a constructor for absolute and relative abstractions, that returns a symmetric
    and tracked if needed abstraction based on a given positive bound.

    All the required components form together an abstract {b Model}, as described
    by the [Modeling] signature described in {!IEEE754_sig}. Note that both in
    this file and in {!IEEE754_sig}, the types use to represent the exact and
    the absolute errors semantics are required to be the same. It is, in theory,
    not mandatory. However, in practice it complexifies quite a lot both the
    signatures and the implementation without providing much use. *)

open Lattice_bounds
open Typed_float

(** All necessary types and signatures are declared in a separated file to avoid
    duplications, and included here to simplify their use. *)
include module type of IEEE754_sig



(** {2 Functor building the IEEE-754 abstract semantic.} *)

module Make (Model : Modeling) : sig

  (** The field over which the abstraction is defined. *)
  module Scalar : Field.S
    with type t = Model.Scalar.t

  (** The abstract context used by the resulting value. *)
  module Context : Abstract_context.Leaf
    with type t = Model.Context.t

  (** The monad used to encode context dependent computations. *)
  module Computation : Computation
    with type context = Context.t
     and type 'a t = 'a Model.Computation.t

  (** Abstraction used for the exact semantic. *)
  module Exact : Abstraction
    with type t = Model.Exact.t
     and module Scalar = Scalar
     and module Computation = Computation

  (** Abstraction used for the absolute error semantic. *)
  module Absolute : Abstraction
    with type t = Model.Absolute.t
     and module Scalar = Scalar
     and module Computation = Computation

  (** Abstraction used for the exact semantic. *)
  module Relative : Abstraction
    with type t = Model.Relative.t
     and module Scalar = Scalar
     and module Computation = Computation

  (** The resulting abstraction is an Eva abstract value. *)
  include Abstract_value.Leaf
    with type context = Context.t


  (** {3 Type aliases.} *)

  type scalar    = Scalar.t
  type exact     = Exact.t
  type absolute  = Absolute.t
  type relative  = Relative.t
  type 'a computation = 'a Computation.t


  (** {3 Useful functions on abstract values. }*)

  (** Returns the exact abstraction of a value. *)
  val exact    : t -> exact

  (** Returns the absolute error abstraction of a value. *)
  val absolute : t -> absolute

  (** Returns the relative error abstraction of a value. *)
  val relative : t -> relative

  (** Returns the floating-point format of a value, or [Top] if the value
      is not bounded. *)
  val format   : t -> resulting_format or_top

  (** Abstract value constructor. *)
  val make : exact -> absolute -> relative -> 'f format -> t

  (** Convert a [fkind] value to a typed format. For the long double format, a
      warning is emitted and the double format is instead used. *)
  val format_of_fkind : Cil_types.fkind -> resulting_format


  (** {3 Effectful arithmetic operators.} *)

  val neg   : t computation -> t computation
  val sqrt  : t computation -> t computation
  val ( + ) : t computation -> t computation -> t computation
  val ( - ) : t computation -> t computation -> t computation
  val ( * ) : t computation -> t computation -> t computation
  val ( / ) : t computation -> t computation -> t computation

end
