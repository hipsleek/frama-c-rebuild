(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Functors for implementing new command line options. *)

(* ************************************************************************* *)
(** {2 Kernel use only} *)
(* ************************************************************************* *)

module Make
    (_: sig
       val shortname: string
       val parameters: (string, Typed_parameter.t list) Hashtbl.t
       module L: sig
         val abort: ('a,'b) Log.pretty_aborter
         val warning: 'a Log.pretty_printer
       end
     end):
  Parameter_sig.Builder

(* ************************************************************************* *)
(** {2 Internal use only} *)
(* ************************************************************************* *)

open Cil_types

val find_kf_by_name: (string -> kernel_function) ref
val find_kf_def_by_name: (string -> kernel_function) ref
val find_kf_decl_by_name: (string -> kernel_function) ref
val kf_category: (unit -> kernel_function Parameter_category.t) ref
val kf_def_category: (unit -> kernel_function Parameter_category.t) ref
val kf_decl_category: (unit -> kernel_function Parameter_category.t) ref
val kf_string_category: (unit -> string Parameter_category.t) ref
val fundec_category: (unit -> fundec Parameter_category.t) ref
val force_ast_compute: (unit -> unit) ref
val ast_dependencies: State.t list ref
