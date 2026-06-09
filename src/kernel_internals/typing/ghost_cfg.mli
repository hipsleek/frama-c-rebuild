(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Checks that normal cfg is not altered by ghost statements.
    Registered as a transformation category (that doesn't transform
    anything but will complain if ghost cfg is ill-formed)
*)
val transform_category: File.code_transformation_category
