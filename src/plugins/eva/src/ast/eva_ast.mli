(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Eva Abstract Syntax Tree. *)

include module type of Eva_ast_types
include module type of Eva_ast_typing
include module type of Eva_ast_printer
include module type of Eva_ast_datatype
include module type of Eva_ast_builder
include module type of Eva_ast_deps
include module type of Eva_ast_utils
include module type of Eva_ast_visitor
