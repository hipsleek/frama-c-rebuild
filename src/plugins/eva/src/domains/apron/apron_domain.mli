(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Experimental binding for the numerical abstract domains provided by
    the APRON library: http://apron.cri.ensmp.fr/library
    For now, this binding only processes scalar integer variables. *)

val octagon: Abstractions.Domain.registered
val box: Abstractions.Domain.registered
val polka_loose: Abstractions.Domain.registered
val polka_strict: Abstractions.Domain.registered
val polka_equality: Abstractions.Domain.registered
