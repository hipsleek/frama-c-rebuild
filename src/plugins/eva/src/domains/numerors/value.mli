(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Numerors' abstract value, which computes a sound overapproximation of the
    floating-point expressions semantic. It is represented as a triplet
    containing a sound overapproximation of the real semantic along with sound
    overapproximations for the absolute and relative errors. Those
    overapproximations also performs a reduced product between the two errors.
    For more details, one can look at M. Jacquemin's thesis. *)

type ('context, 'value) builtin =
  'context -> 'value list -> 'value Eval.or_bottom

module Make (Model : IEEE754.Modeling) : sig
  include Abstract_value.Leaf with type context = Model.Context.t
  val track_variable : Cil_types.varinfo -> bool
  val of_scalars : Cil_types.fkind -> Model.scalar -> Model.scalar -> t
  val widen : Addresses.Bytes.widen_hint -> t -> t -> t
  val builtins : (string * (context, t) builtin) list
end
