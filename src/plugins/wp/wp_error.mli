(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val name : string list -> string

(* ------------------------------------------------------------------------ *)
(* ---  Exception Handling in WP                                        --- *)
(* ------------------------------------------------------------------------ *)

open Cil_types

exception Error of string * string
(** To be raised a feature of C/ACSL cannot be supported by a memory model
    or is not implemented, or ... *)

val set_model : string -> unit

val unsupported : ?model:string -> ('a,Format.formatter,unit,'b) format4 -> 'a
val not_yet_implemented : ?model:string -> ('a,Format.formatter,unit,'b) format4 -> 'a

val pp_logic_label : Format.formatter -> logic_label -> unit

val pp_assigns :
  Format.formatter -> Cil_types.assigns -> unit

val pp_string_list : ?sep:Pretty_utils.sformat -> empty:string ->
  Format.formatter -> string list -> unit
