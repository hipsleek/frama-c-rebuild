(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Heuristic for automatic loop unrolling. *)

module Make (Abstract: Engine_abstractions_sig.S) : sig

  val compute:
    max_unroll:int -> Abstract.Dom.t -> Eva_automata.loop -> int option

end
