(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Export a CIL application from a slicing project *)

val default_slice_names:(Cil_types.kernel_function -> bool  -> int -> string)

(** Apply the actions still waiting in the project
    and transform the program (CIL AST) using slicing results
    Can optionally specify how to name the sliced functions using [f_slice_names].
    (see db.mli)
*)
val extract :
  f_slice_names:(Cil_types.kernel_function -> bool  -> int -> string)
  -> string -> Project.t
