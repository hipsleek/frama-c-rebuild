(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(**
   demote a generated variable holding a string literal into a standard
   char (or wide char) array. This can imply a formal
   AST change if said variable is used to initialize a _local_ array.
   Do nothing if the variable is not a string literal as per
   {!Ast_info.is_string_literal}

   @since 32.0-Germanium
*)
val demote : Cil_types.varinfo -> unit
