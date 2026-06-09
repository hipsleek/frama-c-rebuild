(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This file performs various consistency checks over a cil file.
    Code may vary depending on current development of the kernel and/or
    identified bugs. *)

val check_ast: ?is_normalized:bool -> ?ast:Cil_types.file -> string -> unit
(** Visits the given AST (defaults to the AST of the current project)
    to check whether it is consistent. Use a non-default [ast] argument
    at your own risks.

    Note that the check is only partial.
    @since Aluminium-20160501
*)

module type Extensible_checker =
sig
  class check: ?is_normalized:bool -> string -> Visitor.frama_c_visitor
end

(** Allows to register an extension to current checks. The function
    will be given as input the current state of the checker.

    @since Phosphorus-20170501-beta1
*)
val extend_checker:
  ((module Extensible_checker) -> (module Extensible_checker)) -> unit
