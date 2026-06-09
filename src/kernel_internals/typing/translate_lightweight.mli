(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

(** Annotate files interpreting lightweight annotations. *)

(** Code transformation that interprets __declspec annotations.
    Done after cleanup (see {! File.add_code_transformation_after_cleanup}).
    Name of the transformation is "lightweight spec"
*)
val lightweight_transform: File.code_transformation_category
