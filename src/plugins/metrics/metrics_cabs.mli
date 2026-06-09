(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Metrics computing on Cabs

    Syntactic metrics usually makes more sense on Cabs as they
    reference the original program.

    However, one loses CIL facilities for this purpose. Thus, working
    on Cabs is less developer-friendly.
*)


(** Main entry point to compute various metrics on Cabs AST
    instead of CIL AST.
*)
val compute_on_cabs: unit -> unit ;;

module Halstead : sig
  type halstead_metrics = {
    distinct_operators : float;
    total_operators : float;
    distinct_operands : float;
    total_operands : float;
    program_length : float;
    program_volume : float;
    program_level : float;
    vocabulary_size : float;
    difficulty_level : float;
    effort_to_implement : float;
    time_to_implement : float;
    bugs_delivered : float;
  }
  ;;

  val get_metrics : unit -> halstead_metrics ;;
  val to_list : halstead_metrics -> string list list ;;
end
