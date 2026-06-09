(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Transformations required for the importer (tagging loops and putting unroll
    pragmas). Everything is done through hooks in the normalization phase
    when building the AST. Nothing is exported directly. *)
