(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Functors for generic lattices implementations.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

val feedback_approximation: ('a, Format.formatter, unit) format -> 'a

exception Error_Top
(** Raised by some functions when encountering a top value. *)

exception Error_Bottom
(** Raised by Lattice_Base.project. *)

exception Not_less_than
(** Raised by {!Lattice.cardinal_less_than}. *)

exception Can_not_subdiv
(** Used by other modules e.g. {!Fval.subdiv_float_interval}. *)

type truth = True | False | Unknown
(** Truth values with a possibility for 'Unknown' *)

val inv_truth: truth -> truth

(** Signatures for comparison operators [==, !=, <, >, <=, >=]. *)
module Comp: sig
  type t = Lt | Gt | Le | Ge | Eq | Ne (** comparison operators *)

  type result = truth = True | False | Unknown (** result of a comparison *)

  val pretty_comp: t Pretty_utils.formatter

  val inv: t -> t
  (** Inverse relation: [a op b <==> ! (a (inv op) b)].  *)

  val sym: t -> t
  (** Opposite relation: [a op b <==> b (sym op) a]. *)

end


module Int : sig
  include module type of Integer with type t = Integer.t
    [@@alert "-deprecated"]

  include Datatype.S_with_collections with type t := t

  val fold : (t -> 'a -> 'a) -> inf:t -> sup:t -> step:t -> 'a -> 'a
  (** Fold the function on the value between [inf] and [sup] at every
      step. If [step] is positive the first value is [inf] and values
      go increasing, if [step] is negative the first value is [sup]
      and values go decreasing *)
  [@@deprecated "Use Int_interval.fold_int instead"]
end
[@@deprecated "Use Z module instead. You can use OCamlmig (see \
               Frama-C Plugin Development Guide) or integer.mli for migration \
               hints"]

(** "Relative" integers. They are subtraction between two absolute integers *)
module Rel : sig
  type t

  val pretty: t Pretty_utils.formatter

  val equal: t -> t -> bool
  val compare: t -> t -> int
  val hash: t -> int

  val zero: t
  val is_zero: t -> bool

  val sub : t -> t -> t
  val add_abs : Z.t -> t -> Z.t
  val add : t -> t -> t
  val sub_abs : Z.t -> Z.t -> t
  val erem: t -> Z.t -> t

  val e_rem: t -> Z.t -> t
  [@@deprecated "Use erem instead."]
  [@@migrate { repl = Rel.erem } ]

  val check: rem:t -> modu:Z.t -> bool
end

module Make_Lattice_Set
    (V : Datatype.S)
    (Set: Lattice_type.Hptset with type elt = V.t)
  : Lattice_type.Lattice_Set with module O = Set

module Make_Hashconsed_Lattice_Set
    (V: Hptmap.Id_Datatype)
    (Set: Hptset.S with type elt = V.t)
  : Lattice_type.Lattice_Set with module O = Set
(** See e.g. base.ml and locations.ml to see how this functor should be
    applied. The [O] module passed as argument is the same as [O] in the
    result. It is passed here to avoid having multiple modules calling
    [Hptset.Make] on the same argument (which is forbidden by the datatype
    library, and would cause hashconsing problems) *)
