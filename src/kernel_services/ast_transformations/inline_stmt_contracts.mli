(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** transforms requires and ensures of statement contracts into assert.
    This transformation is done after cleanup
*)

val emitter : Emitter.t
val category : File.code_transformation_category
