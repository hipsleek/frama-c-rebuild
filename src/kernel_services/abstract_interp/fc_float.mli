(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Implementation of floating-point values of different precision,
    using the standard ocaml floating-point numbers in double precision.
    Long_Double and Real are inexact. *)

include Float_sig.S with type t = float
