(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** The purpose of this module global definitions when it is needed by
    instantiation modules.
*)

(** [get_variable name f] searches for an existing variable [name]. If this
    variable does not exists, it is created using [f].

    The obtained varinfo does not need to be registered, nor [f] needs to
    perform the registration, it will be done by the transformation.
*)
val get_variable: string -> (unit -> varinfo) -> varinfo

(** [get_logic_function name f] searches for an existing logic function [name].
    If this function does not exists, it is created using [f]. If the logic
    function must be part of an axiomatic block **DO NOT** use this function,
    use [get_logic_function_in_axiomatic].

    Note that function overloading is not supported.
*)
val get_logic_function: string -> (unit -> logic_info) -> logic_info

(** [get_logic_function_in_axiomatic name f] searches for an existing logic
    function [name]. If this function does not exists, an axiomatic definition
    is created using [f].

    [f] must return:
    - the axiomatic in a form [name, list of the definitions (incl. functions)]
    - all functions that are part of the axiomatic definition

    Note that function overloading is not supported.
*)
val get_logic_function_in_axiomatic:
  string ->
  (unit -> (string * global_annotation list) * logic_info list) ->
  logic_info

(** Clears internal tables *)
val clear: unit -> unit

(** Creates a list of global for the elements that have been created *)
val globals: location -> global list
