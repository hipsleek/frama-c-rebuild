(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Syntactic loop unfolding.
    Uses code transformation hook mechanism (after-cleanup phase)
    of {!File} and exports nothing.

    Name of the transformation is "loop unfolding"
*)

val transform: File.code_transformation_category
