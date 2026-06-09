(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Code transformation for inferring contracts from information given
    in GNU's extended assembly syntax. Registered as a transformation
    occurring after annotations table are filled. *)

(** the corresponding code transformation category. *)
val category: File.code_transformation_category

(** emitter for the generated annotations. *)
val emitter: Emitter.t
