(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

val plugin_name: string

(** {1 Messages and warning categories} *)

module Keys: sig
  val dkey_binding: category
  val dkey_binding_table: category
  val dkey_volatile_table: category
  val dkey_transformation_action: category
  val dkey_transformation_visit: category

  val wkey_invalid_binding_function: warn_category
  val wkey_unsupported_volatile_clause: warn_category
  val wkey_volatile_cast: warn_category
  val wkey_cast_insertion: warn_category
  val wkey_duplicated_access_function: warn_category
  val wkey_transformed_access_lvalue_volatile: warn_category
  val wkey_transformed_access_lvalue_partially_volatile: warn_category
  val wkey_untransformed_access_lvalue_volatile: warn_category
  val wkey_untransformed_access_lvalue_partially_volatile: warn_category
  val wkey_untransformed_call: warn_category
  val wkey_untransformed_call_function_not_found: warn_category
  val wkey_transformed_call: warn_category
  val wkey_transformed_call_skipped_parameters: warn_category
  val wkey_transformed_call_missing_parameters: warn_category
end

(** {1 Options} *)

module Enabled : Parameter_sig.Bool
module Process : Parameter_sig.Kernel_function_set
module CallPtr : Parameter_sig.Kernel_function_set
module Binding : Parameter_sig.Kernel_function_set
module BindingAuto : Parameter_sig.Bool
module BindingCall : Parameter_sig.Bool
module Base : Parameter_sig.Bool
module BindingPrefix : Parameter_sig.String
