(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Subdivision of the evaluation on non-linear expressions:
    for expressions in which some l-values appear multiple times, proceed
    by disjunction on their abstract value, in order to gain precision. *)

open Eval

module type Forward_Evaluation = sig
  type value
  type valuation
  type environment
  val evaluate: subdivided:bool -> environment -> valuation ->
    exp -> (valuation * value) evaluated
end

module Make
    (Value : Abstract.Value.External)
    (Loc: Abstract_location.S with type value = Value.t)
    (Valuation: Valuation with type value = Value.t
                           and type loc = Loc.location)
    (Eva: Forward_Evaluation with type value := Value.t
                              and type valuation := Valuation.t)
  : sig

    val evaluate:
      Eva.environment -> Valuation.t -> subdivnb:int ->
      exp -> (Valuation.t * Value.t) evaluated

    val reduce_by_enumeration:
      Eva.environment -> Valuation.t -> exp -> bool -> Valuation.t or_bottom
  end
