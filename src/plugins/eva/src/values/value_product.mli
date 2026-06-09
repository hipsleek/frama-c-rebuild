(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Cartesian product of two value abstractions. *)

type 'v truth := 'v Abstract_value.truth

(** [narrow_truth (v1, t1) (v2, t2)] intersects the truth values [t1] and [t2]
    resulting from [assume_] functions for abstract values [v1] and [v2]
    (that may be reduced by the assumption). *)
val narrow_truth: 'a * 'a truth -> 'b * 'b truth -> ('a * 'b) truth

(** Same as narrow_truth for truth values involving pairs of abstract values. *)
val narrow_truth_pair:
  ('a * 'a) * ('a * 'a) truth -> ('b * 'b) * ('b * 'b) truth ->
  (('a * 'b)  * ('a * 'b)) truth

module Make
    (Context : Abstract_context.S)
    (Left  : Abstract.Value.Internal with type context = Context.t)
    (Right : Abstract.Value.Internal with type context = Context.t)
  : Abstract.Value.Internal
    with type t = Left.t * Right.t
     and type context = Context.t
