(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Computations of the statements that read a given memory zone. *)

type t =
  | Direct of Cil_types.stmt
  (** Direct read by a statement. *)
  | Indirect of Cil_types.stmt
  (** Indirect read through a function call. *)

val compute: Memory_zone.t -> t list
(** [compute z] finds all the statements that read [z]. The [effects]
    information indicates whether the read occur on the given statement,
    or through an inner call for [Call] instructions. *)
