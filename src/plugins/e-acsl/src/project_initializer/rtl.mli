(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module links the E-ACSL's RTL to the user source code. *)

val link: Project.t -> unit
(** [link prj] links the RTL's AST contained in [prj] to the AST of the current
    project. *)

(** Tables that contain RTL's symbols. Useful to know whether some symbols is
    part of the RTL. *)
module Symbols: sig
  open Cil_types

  val mem_global: global -> bool
  val mem_kf: kernel_function -> bool

  val mem_vi: string -> bool
  exception Unregistered of string
  val find_vi: string -> varinfo
  (** @raise Unregistered if the given name is not part of the RTL. *)

  val replacement: get_name:(string -> string) -> varinfo -> varinfo
  (** Given the varinfo of a C function with an RTL replacement, return
      the varinfo of the RTL function that replaces it. The function
      [get_name] is used to find the name of the RTL replacement. *)
end
