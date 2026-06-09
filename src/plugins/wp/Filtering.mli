(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Sequent Cleaning *)
(* -------------------------------------------------------------------------- *)

open Lang

(**
   Erase parts of a predicate that do not satisfies the condition.
   The erased parts are replaced by:
   - [true] when [~polarity:false] (for hypotheses)
   - [false] when [~polarity:true] (for goals)

   Hence, we have:
   - [filter ~polarity:true f p ==> p]
   - [p ==> filter ~polarity:false f p]

   See [theory/filtering.why] for proofs.
*)

val filter : polarity:bool -> (F.pred -> bool) -> F.pred -> F.pred

open Conditions

val compute : ?anti:bool -> sequent -> sequent




(* -------------------------------------------------------------------------- *)
