(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Contract_types

(** Translate a given ACSL contract (function or statement) into the
    corresponding C statement for runtime assertion checking. *)

type t = contract

val create: loc:location -> spec -> t
(** Create a contract from a [spec] object (either function spec or statement
    spec) *)

val translate_preconditions: kernel_function -> Env.t -> t -> Env.t
(** Translate the preconditions of the given contract into the environment *)

val translate_postconditions: kernel_function -> Env.t -> Env.t
(** Translate the postconditions of the given contract into the environment *)
