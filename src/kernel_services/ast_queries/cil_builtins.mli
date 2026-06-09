(***************************************************************************)
(*                                                                         *)
(*  SPDX-License-Identifier BSD-3-Clause                                   *)
(*  Copyright (C) 2001-2003                                                *)
(*  George C. Necula    <necula@cs.berkeley.edu>                           *)
(*  Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*  Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*  Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                   *)
(*  File modified by                                                       *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   *)
(*  INRIA (Institut National de Recherche en Informatique et Automatique)  *)
(*                                                                         *)
(***************************************************************************)

open Cil_types

(* ************************************************************************* *)
(** {2 Builtins management} *)
(* ************************************************************************* *)

(** This module associates the name of a built-in function that might be used
    during elaboration with the corresponding varinfo.  This is done when
    parsing [${FRAMAC_SHARE}/libc/__fc_builtins.h], which is always performed
    before processing the actual list of files provided on the command line (see
    {!File.init_from_c_files}).  Actual list of such built-ins is managed in
    {!Cabs2cil}. *)
module Frama_c_builtins:
  State_builder.Hashtbl with type key = string and type data = varinfo

val is_builtin: varinfo -> bool
(** @return true if {!has_fc_builtin_attr} or {!is_special_builtin} are true.
    @before 29.0-Copper Only check for {!has_fc_builtin_attr}.
    @since Fluorine-20130401 *)

val has_fc_builtin_attr: varinfo -> bool
(** @return true if the given variable has a FC_BUILTIN attribute
    @since 29.0-Copper *)

val is_unused_builtin: varinfo -> bool
(** @return true if the given variable refers to a Frama-C builtin that
    is not used in the current program. Plugins may (and in fact should)
    hide this builtin from their outputs *)

val is_special_builtin: string -> bool
(** @return [true] if the given name refers to a special built-in function.
    A special built-in function can have any number of arguments. It is up to
    the plug-ins to know what to do with it.
    @since Carbon-20101201 *)

(** register a new special built-in function *)
val add_special_builtin: string -> unit

(** register a new family of special built-in functions.
    @since Carbon-20101201
*)
val add_special_builtin_family: (string -> bool) -> unit

(** A list of the built-in functions for the current compiler (GCC or
    MSVC, depending on [!Machine.msvcMode]).  Maps the name to the
    result and argument types, and whether it is vararg.
    Initialized by {!Machine.init}. Do not add builtins directly, use
    {! add_custom_builtin } below for that.

    This map replaces [gccBuiltins] and [msvcBuiltins] in previous
    versions of CIL.*)
module Builtin_functions :
  State_builder.Hashtbl with type key = string
                         and type data = typ * typ list * bool

type compiler = GCC | MSVC | Not_MSVC

val string_of_compiler : compiler -> string

type builtin_template = {
  name: string;
  compiler: compiler option;
  rettype: string;
  args: string list;
  types: string list option;
  variadic: bool;
}

module Builtin_templates :
  State_builder.Hashtbl with type key = string
                         and type data = builtin_template

module Gcc_builtin_templates_loaded : State_builder.Ref with type data = bool

val init_gcc_builtin_templates : unit -> unit

(** Register a new builtin. The function will be called after setting
    the machdep and initializing machine-dependent builtins. Hence, types
    such {!Cil.uint16_t} might be used if needed.

    @since 23.0-Vanadium
*)
val add_custom_builtin: (unit -> (string * typ * typ list * bool)) -> unit

(** This is used as the location of the prototypes of builtin functions. *)
val builtinLoc: location
