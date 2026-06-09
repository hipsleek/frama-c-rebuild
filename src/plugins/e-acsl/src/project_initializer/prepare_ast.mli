(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Prepare AST for E-ACSL generation.

    More precisely, this module performs the following tasks:
    - generating a new definition for functions with contract;
    - removing term sharing;
    - in case of temporal validity checks, adding the attribute "aligned" to
      variables that are not sufficiently aligned;
    - create a block around a labeled statement to hold the labels so that the
      code generation does not need to change the statement holding the label.
*)

open Cil_types

val prepare: unit -> unit
(** Prepare the AST *)

val sound_verdict: unit -> varinfo
(** @return the [varinfo] representing the E-ACSL global variable that indicates
    whether the verdict emitted by E-ACSL is sound. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

val is_libc_writing_memory_ref: (varinfo -> bool) ref
