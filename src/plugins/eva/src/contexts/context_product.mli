(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Cartesian product of two context abstractions. *)

module Make (L : Abstract_context.S) (R : Abstract_context.S)
  : Abstract_context.S with type t = L.t * R.t
