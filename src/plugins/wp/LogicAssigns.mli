(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Make
    ( M : Memory.Model )
    ( L : Memory.LogicSemantics with module M = M ) :
  Memory.LogicAssigns with module M = M and module L = L
