(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

let empty_string = ""

let cmdline_stage_ref = ref Cmdline.Configuring
let set_cmdline_stage s = cmdline_stage_ref := s

let negative_option_name_ref = ref None
let set_negative_option_name s = negative_option_name_ref := Some s

let negative_option_help_ref = ref empty_string
let set_negative_option_help s = negative_option_help_ref := s

let unset_option_name_ref = ref empty_string
let set_unset_option_name s = unset_option_name_ref := s

let unset_option_help_ref = ref empty_string
let set_unset_option_help s = unset_option_help_ref := s

let must_save_ref = ref true
let do_not_save () = must_save_ref := false

let reset_on_copy_ref = ref true
let do_not_reset_on_copy () = reset_on_copy_ref := false

let projectify_ref = ref true
let do_not_projectify () =
  projectify_ref := false;
  do_not_save ();
  do_not_reset_on_copy ()

let empty_format = ("": (unit, Format.formatter, unit) format)
let optional_help_ref = ref empty_format

let module_name_ref = ref empty_string
let set_module_name s = module_name_ref := s

let argument_is_function_name_ref = ref false
let argument_is_function_name () = argument_is_function_name_ref := true

let argument_may_be_fundecl_ref = ref false
let argument_may_be_fundecl () = argument_may_be_fundecl_ref := true

let argument_must_be_fundecl_ref = ref false
let argument_must_be_fundecl () = argument_must_be_fundecl_ref := true

let argument_must_be_existing_fun_ref = ref false
let argument_must_be_existing_fun () =
  argument_must_be_existing_fun_ref := true

let group_ref = ref Cmdline.Group.default
let set_group s = group_ref := s

let is_reconfigurable_ref = ref None
let is_reconfigurable () = is_reconfigurable_ref := Some true
let is_not_reconfigurable () = is_reconfigurable_ref := Some false

let is_visible_ref = ref true
let is_invisible () =
  is_visible_ref := false;
  is_not_reconfigurable ()

let is_safe_ref = ref true
let is_unsafe () =
  is_safe_ref := false

let use_category_ref = ref true
let no_category () = use_category_ref := false

let find_kf_by_name: (string -> Cil_types.kernel_function) ref =
  Extlib.mk_fun "Parameter_customize.find_kf_by_name"

let plain_fct_finder s =
  try
    Cil_datatype.Kf.Set.singleton (!find_kf_by_name s)
  with Not_found -> Cil_datatype.Kf.Set.empty

let mangling_functions = ref [plain_fct_finder]

let get_c_ified_functions s =
  List.fold_left
    (fun acc f -> Cil_datatype.Kf.Set.union (f s) acc)
    Cil_datatype.Kf.Set.empty
    !mangling_functions

let add_function_name_transformation f =
  mangling_functions := f :: !mangling_functions

let reset () =
  cmdline_stage_ref := Cmdline.Configuring;
  negative_option_name_ref := None;
  negative_option_help_ref := empty_string;
  unset_option_name_ref:= empty_string;
  unset_option_help_ref:= empty_string;
  optional_help_ref := empty_format;
  projectify_ref := true;
  must_save_ref := true;
  module_name_ref := empty_string;
  group_ref := Cmdline.Group.default;
  is_reconfigurable_ref := None;
  is_visible_ref := true;
  is_safe_ref := true;
  argument_is_function_name_ref := false;
  argument_may_be_fundecl_ref := false;
  argument_must_be_fundecl_ref := false;
  argument_must_be_existing_fun_ref := false;
  reset_on_copy_ref := true;
  use_category_ref := true
