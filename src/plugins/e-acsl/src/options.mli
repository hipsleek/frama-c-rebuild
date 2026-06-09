(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S (** implementation of Log.S for E-ACSL *)

module Run: Parameter_sig.Bool
module Valid: Parameter_sig.Bool
module Gmp_only: Parameter_sig.Bool
module Full_mtracking: Parameter_sig.Bool
module Project_name: Parameter_sig.String
module Builtins: Parameter_sig.String_set
module Temporal_validity: Parameter_sig.Bool
module Validate_format_strings: Parameter_sig.Bool
module Replace_libc_functions: Parameter_sig.Bool
module Assert_print_data: Parameter_sig.Bool
module Concurrency: Parameter_sig.Bool
module Interlang: Parameter_sig.Bool
module Interlang_force: Parameter_sig.Bool
module O : Parameter_sig.Int

module Functions: Parameter_sig.Kernel_function_set
module Instrument: Parameter_sig.Kernel_function_set

module Widening_arguments_base: Parameter_sig.Int
module Widening_arguments: Parameter_sig.Map
  with type key = string and type value = int
module Widening_output_base: Parameter_sig.Int
module Widening_output: Parameter_sig.Map
  with type key = string and type value = int

val parameter_states: State.t list
val emitter: Emitter.t

val must_visit: unit -> bool

module Dkey: sig
  val prepare: category
  val logic_normalizer: category
  val bound_variables: category
  val rte: category
  val inductive: category
  val interval: category
  val mtracking: category
  val typing: category
  val labels: category
  val translation: category
  val env: category
  val interlang_translation: category
  val interlang_not_covered: category
  val interlang_print_opt: category
end

val setup: ?rtl:bool -> unit -> unit
(** Verify and initialize the options of the current project according to the
    options set by the user.
    If [rtl] is true, then the project being modified is the RTL project. *)
