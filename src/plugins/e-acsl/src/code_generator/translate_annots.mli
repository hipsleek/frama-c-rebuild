(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Functions that translate a given ACSL annotation into the corresponding C
    statements (if any) for runtime assertion checking. These C statements are
    part of the resulting environment. *)

val pre_funspec: kernel_function -> Env.t -> funspec -> Env.t
(** Translate the preconditions of the given function contract in the
    environment. The contract is attached to the kernel_function.

    The function contract is pushed in the environment, some care should be
    taken to call {!post_funspec} at the right time to pop the right
    contract. *)

val post_funspec: kernel_function -> Env.t -> Env.t
(** Translate the postconditions of the current function contract in the
    environment.

    The function contract previously built by {!pre_funspec} is popped
    from the environment. Some care should be taken to call this function at
    the right time to pop the right contract. *)

val pre_code_annotation:
  kernel_function -> stmt -> Env.t -> code_annotation -> Env.t
(** Translate the preconditions of the given code annotation in the
    environment.

    If available, the statement contract is pushed in the environment, some care
    should be taken to call {!post_code_annotation} at the right time
    to pop the right contract. *)

val post_code_annotation:
  kernel_function -> stmt -> Env.t -> code_annotation -> Env.t
(** Translate the postconditions of the current code annotation in the
    environment.

    If necessary, the statement contract previously built by
    {!pre_code_annotation} is popped from the environment. Some care
    should be taken to call this function at the right time to pop the right
    contract. *)
