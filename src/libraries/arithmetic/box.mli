(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module provides a representation of closed boxes in a classical
    vector space over a field 𝕂. *)
module Make (K : Field.S) : sig

  open Nat
  open Linear.Space (K)

  (** Boxes are represented as a center and a radius. The radius components
      are always all positives. *)
  type 'n t = { center : 'n vector ; radius : 'n vector }

  (** The call [make center radius] returns a box of center [center] and
      of radius [abs radius], with [abs] the componentwise absolute value
      on vectors as defined in {!Linear}. *)
  val make : 'n succ vector -> 'n succ vector -> 'n succ t

  (** The call [point center] returns a box of radius zero and of
      center [center], i.e a point in the vector space. *)
  val point : 'n succ vector -> 'n succ t

  (** The call [zero n] returns the point zero in a [n] dimensional
      vector space. *)
  val zero : 'n succ nat -> 'n succ t

  (** Pretty printer. *)
  val pretty : 'n succ t Pretty_utils.formatter

  (** Boxes can also be seen as a collection of intervals. The call [bounds b]
      returns thus the bounds in each dimension of the closed space defined
      by [b]. The [lower b] (resp. [upper b]) function returns only the lower
      bounds (resp. upper bounds). *)
  val bounds : 'n t -> 'n vector * 'n vector
  val lower : 'n t -> 'n vector
  val upper : 'n t -> 'n vector

  (** The call [is_included l r] returns true if and only if all points in
      the box [l] are also in the box [r]. *)
  val is_included : 'n t -> 'n t -> bool

  (** Minkowsky sum of two boxes, i.e {m l + r} is the box {m z} such as
      {m \forall x \in l, y \in r, x + y \in z}. *)
  val ( + ) : 'n t -> 'n t -> 'n t

end
