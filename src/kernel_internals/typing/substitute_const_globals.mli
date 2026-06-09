(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A visitor that substitutes globals, defined with the attribute 'const', with
    respective initializers. *)
val constGlobSubstVisitor: Cil.cilVisitor
