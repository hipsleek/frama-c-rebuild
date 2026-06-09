(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Generation of possible assigns from the C prototype of a function. *)

val from_prototype: Kernel_function.t -> Cil_types.from list

(** Same as {!from_prototype} but given a varinfo instead of a kernel
    function.
    @since 33.0-Arsenic
*)
val from_prototype_vi: Cil_types.varinfo -> Cil_types.from list
