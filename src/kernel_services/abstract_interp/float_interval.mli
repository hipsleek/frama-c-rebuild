(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Builds a semantics of floating-point intervals for different precisions,
    from a module providing the floating-point numbers used for the bounds
    of the intervals.
    Supports NaN and infinite values. *)
module Make (Float: Float_sig.S) :
  Float_interval_sig.S with type float := Float.t
