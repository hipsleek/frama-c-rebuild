(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Lattice signatures using the Bottom type:
    these lattices do not include a bottom element, and return `Bottom instead
    when needed. Except that, they are identical to the module signatures in
    {!Lattice_type}. *)

open Lattice_bounds

module type Join_Semi_Lattice = Lattice_type.Join_Semi_Lattice
module type With_Top = Lattice_type.With_Top
module type With_Intersects = Lattice_type.With_Intersects
module type With_Enumeration = Lattice_type.With_Enumeration
module type With_Cardinal_One = Lattice_type.With_Cardinal_One

module type With_Narrow = sig
  type t
  val narrow: t -> t -> t or_bottom (** over-approximation of intersection *)
end

module type With_Under_Approximation = sig
  type t

  val link: t -> t -> t
  (** under-approximation of union *)

  val meet: t -> t -> t or_bottom
  (** under-approximation of intersection *)
end

module type With_Diff = sig
  type t
  val diff : t -> t -> t or_bottom
  (** [diff t1 t2] is an over-approximation of [t1-t2]. [t2] must
      be an under-approximation or exact. *)
end

module type With_Diff_One = sig
  type t
  val diff_if_one : t -> t -> t or_bottom
  (** [diff_if_one t1 t2] is an over-approximation of [t1-t2].
      @return [t1] if [t2] is not a singleton. *)
end

(** {2 Common signatures} *)

(** Signature shared by some functors of module {!Abstract_interp}. *)
module type AI_Lattice_with_cardinal_one = sig
  include Join_Semi_Lattice
  include With_Top with type t:= t
  include With_Cardinal_One with type t := t
  include With_Narrow with type t := t
  include With_Under_Approximation with type t := t
  include With_Intersects with type t := t
end

(** Most complete lattices: all operations plus widening, notion of cardinal
    (including enumeration) and difference. *)
module type Full_AI_Lattice_with_cardinality = sig
  include AI_Lattice_with_cardinal_one
  include With_Diff with type t := t
  include With_Diff_One with type t := t
  include With_Enumeration with type t := t
end
