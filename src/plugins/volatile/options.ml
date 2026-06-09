(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Plug-in states and options. *)

let plugin_name="Volatile"

include Plugin.Register
    (struct
      let name = plugin_name
      let shortname = "volatile"
      let help = "support for volatile accesses and calls through function pointers"
    end)

(** {1 Messages and warning categories} *)

module Keys = struct

  let dkey_binding =
    register_category
      ~help:"Prints debug messages related to volatile operations"
      "binding"

  let dkey_binding_table =
    register_category
      ~help:"Prints debug messages related to volatile operations on internal tables"
      "binding-table"

  let dkey_volatile_table =
    register_category
      ~help:"Prints Volatile internal tables"
      "volatile-table"

  let dkey_transformation_action =
    register_category
      ~help:"Prints information on generated code"
      "transformation-action"

  let dkey_transformation_visit =
    register_category
      ~help:"Prints visitor information during the transformation"
      "transformation-visit"

  let wkey name status =
    let wkey = register_warn_category name in
    set_warn_status wkey status;
    wkey

  let wkey_invalid_binding_function =
    wkey "invalid-binding-function" Log.Werror
  let wkey_unsupported_volatile_clause =
    wkey "unsupported:volatile-clause" Log.Werror
  let wkey_duplicated_access_function =
    wkey "duplicated-access-function" Log.Werror

  let wkey_volatile_cast = wkey "cast:volatile" Log.Wfeedback
  let wkey_cast_insertion =
    wkey "cast:insertion" Log.Wfeedback

  let wkey_transformed_access_lvalue_volatile =
    wkey "transformed-access:lvalue-volatile" Log.Wfeedback
  let wkey_transformed_access_lvalue_partially_volatile =
    wkey "transformed-access:lvalue-partially-volatile" Log.Wfeedback

  let wkey_untransformed_access_lvalue_volatile =
    wkey "untransformed-access:lvalue-volatile" Log.Wactive
  let wkey_untransformed_access_lvalue_partially_volatile =
    wkey "untransformed-access:lvalue-partially-volatile" Log.Wactive

  let wkey_untransformed_call = wkey "untransformed-call" Log.Wactive
  let wkey_untransformed_call_function_not_found =
    wkey "untransformed-call:function_not_found" Log.Wactive

  let wkey_transformed_call = wkey "transformed-call" Log.Wfeedback
  let wkey_transformed_call_skipped_parameters =
    wkey "transformed-call:skipped-parameters" Log.Wactive
  let wkey_transformed_call_missing_parameters =
    wkey "transformed-call:missing-parameters" Log.Wactive

end

(** {1 Plug-in options.} *)

module Enabled =
  False (struct
    let option_name = "-volatile"
    let help =
      "builds a new project (named \"" ^ plugin_name
      ^"\") where volatile accesses are simulated by function calls"
  end)

let () = Parameter_customize.argument_may_be_fundecl ()
module Process =
  Kernel_function_set (struct
    let option_name = "-volatile-fct"
    let arg_name = "f,..."
    let help = "Only process the given function(s)"
  end)

let () = Parameter_customize.argument_may_be_fundecl ()
module CallPtr =
  Kernel_function_set (struct
    let option_name = "-volatile-call-pointer"
    let arg_name = "f,..."
    let help = "stub call to pointer functions to the provided functions \
                (indexed by type)"
  end)
let () = Parameter_customize.argument_must_be_fundecl ()

module Base =
  False (struct
    let option_name = "-volatile-basetype"
    let help = "use base-type for int, float and enums for the instrumentation \
                related to -volatile-binding option"
  end)

let () = Parameter_customize.argument_may_be_fundecl ()
module Binding =
  Kernel_function_set (struct
    let option_name = "-volatile-binding"
    let arg_name = "f,..."
    let help = "allows binding of volatile accesses to functions <f,...>"
  end)
let () = Parameter_customize.argument_must_be_fundecl ()

module BindingAuto =
  False (struct
    let option_name = "-volatile-binding-auto"
    let help = "allows automatic binding of volatiles accesses to functions: \
                <prefix>Rd_<typename> and <prefix>Wr_<typename>"
  end)

module BindingCall =
  False (struct
    let option_name = "-volatile-binding-call-pointer"
    let help = "replaces calls through function pointers by direct calls to \
                functions: <prefix>Call_<result-type>_<param-types>"
  end)

module BindingPrefix =
  String (struct
    let option_name = "-volatile-binding-prefix"
    let arg_name = "str"
    let default = "c2fc2_"
    let help = "adds <str> as prefix to function names for automatic binding"
  end)
let () = BindingPrefix.add_set_hook
    (fun _t -> function
       | "" -> error "empty string cannot be used as binding prefix@."
       | str ->  let rx = Str.regexp "^[a-zA-Z_][a-zA-Z0-9_$]*$" in
         if (not (Str.string_match rx str 0)) then
           error "binding prefix %S does not match C identifier regexp@."str;
    )
