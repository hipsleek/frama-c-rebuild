(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Experimental module *)

(** Returns a clone of a kernel function and
    adds it into the AST next to the old one *)
val clone_defined_kernel_function: kernel_function -> kernel_function
