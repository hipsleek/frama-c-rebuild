(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Internal printer for Cabs2cil.

    Like the standard [Cil_printer], but instead of temporary variable
    names it prints the description that was provided when the temp was
    created.  This is usually better for messages that are printed for end
    users, although you may want the temporary names for debugging.  *)

open Cil_types
val pp_exp: Format.formatter -> exp -> unit
val pp_lval: Format.formatter -> lval -> unit
val pp_lhost: Format.formatter -> lhost -> unit
