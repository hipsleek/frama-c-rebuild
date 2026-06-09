(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Widening hints for the Value Analysis datastructures. *)

include Datatype.S

(** An empty set of hints *)
val empty : t

(** A default set of hints *)
val default : unit -> t

val join: t -> t -> t

(**  Pretty-prints a set of hints (for debug purposes only).
     @since Silicon-20161101 *)
val pretty : Format.formatter -> t -> unit

(** Define numeric hints for one or all variables ([None]),
    for a certain stmt or for all statements ([None]).  *)
val num_hints:
  Cil_types.stmt option -> Base.t option -> Int_val.widen_hint -> t

(** Define floating hints for one or all variables ([None]),
    for a certain stmt or for all statements ([None]).  *)
val float_hints:
  Cil_types.stmt option -> Base.t option -> Fval.widen_hint -> t

(** Define a set of bases to widen in priority for a given statement. *)
val var_hints : Cil_types.stmt -> Base.Set.t -> t

(** Widen hints for a given statement, suitable for function
    {!Cvalue.Model.widen}. *)
val hints_from_keys :
  Cil_types.stmt -> t ->
  Base.Set.t * (Base.t -> Addresses.Bytes.widen_hint)
