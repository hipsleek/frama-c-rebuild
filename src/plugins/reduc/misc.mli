(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

exception Not_implemented of string
val not_implemented: what:string -> unit

(* [emitter] of the reduc plugin. *)
val emitter: Emitter.t

(* ******************************************************)
(*      Annotations and function contracts helpers      *)
(* ******************************************************)
val assert_and_validate: kf:Kernel_function.t -> stmt -> predicate -> unit
