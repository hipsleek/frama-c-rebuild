(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eval

(* Marks all behaviors of the list as inactive. *)
val process_inactive_behaviors:
  kinstr -> kernel_function -> behavior list -> unit

(* Checks "calls" annotations at the given statement according to the inferred
   list of functions at this point. Reduces the given list to the functions
   referred to by "calls" annotations. *)
val check_calls_annotations:
  stmt -> (kernel_function * 'a) list -> (kernel_function * 'a) list


module type LogicDomain = sig
  type t
  val top: t
  val equal: t -> t -> bool
  val evaluate_predicate:
    t Abstract_domain.logic_environment -> t -> predicate -> Alarmset.status
  val reduce_by_predicate:
    t Abstract_domain.logic_environment -> t -> predicate -> bool -> t or_bottom
  val interpret_acsl_extension:
    acsl_extension -> t Abstract_domain.logic_environment -> t -> t
end

module Make (Domain: LogicDomain) :
  Engine_sig.Transfer_logic with type state = Domain.t
