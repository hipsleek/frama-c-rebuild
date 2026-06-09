(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Plugin Registration                                                --- *)
(* -------------------------------------------------------------------------- *)

include Plugin.S

(** Module activation *)
module Enabled : Parameter_sig.Bool

(** Displays the table [function -> summary] at the end of the analysis *)
module ShowFunctionTable : Parameter_sig.Bool

(** Displays the table [statement -> state] at the end of the analysis *)
module ShowStmtTable : Parameter_sig.Bool

(** Switch to debug mode when displaying tables *)
module DebugTable : Parameter_sig.Bool

(** Displays the final abstract state in Dot File <f> *)
module Dot_output : Parameter_sig.String

module Warn : sig
  val no_return_stmt : warn_category
  val undefined_function : warn_category
  val unsupported_address : warn_category
  val unsupported_asm : warn_category
  val unsupported_function : warn_category
  val unsafe_cast : warn_category
  val incoherent : warn_category
end

module DebugKeys : sig
  val show_libc_vars : category
  val show_internal_state : category
end
