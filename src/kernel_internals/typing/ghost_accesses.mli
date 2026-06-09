(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Checks that memory accesses are well-typed in the sense of the [\ghost]
    qualifier. That is, only ghost code can declare ghost memory, and ghost
    code can modify only ghost memory.
    Registered as a transformation category (that doesn't transform
    anything but will complain if there are bad accesses).
*)
val transform_category: File.code_transformation_category
