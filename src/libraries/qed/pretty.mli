(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(**    Pretty Printer for Qed Output.                                         *)
(* -------------------------------------------------------------------------- *)

open Logic
open Format

module Make(T : Term) :
sig
  open T

  type env (** environment for pretty printing *)

  val empty : env
  val marks : env -> marks
  val known : env -> Vars.t -> env
  val fresh : env -> term -> string * env
  val bind : string -> term -> env -> env

  val pp_tau : formatter -> tau -> unit

  (** print with the given environment without modifying it *)
  val pp_term : env -> formatter -> term -> unit
  val pp_def : env -> formatter -> term -> unit

  (** print with the given environment and update it *)
  val pp_term_env : env -> formatter -> term -> unit
  val pp_def_env : env -> formatter -> term -> unit

end
