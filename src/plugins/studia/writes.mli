(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Computations of the statements that write a given memory zone. *)

type t =
  | Assign of Cil_types.stmt
  (** Direct assignment. *)
  | CallDirect of Cil_types.stmt
  (** Modification by a called leaf function. *)
  | CallIndirect of Cil_types.stmt
  (** Modification inside the body of a called function. *)
  | GlobalInit of Cil_types.varinfo * Cil_types.initinfo
  (** Initialization of a global variable. *)
  | FormalInit of
      Cil_types.varinfo *
      (Cil_types.kernel_function * Cil_types.stmt list) list
  (** Initialization of a formal parameter, with a list of callsites. *)

val compare: t -> t -> int

val compute: Memory_zone.t -> t list
(** [compute z] finds all the statements that modifies [z], and for each
    statement, indicates whether the modification is direct or indirect. *)
