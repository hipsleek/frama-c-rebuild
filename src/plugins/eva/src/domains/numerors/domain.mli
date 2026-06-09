(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Numerors' abstract domain, which computes a sound overapproximation of the
    floating-point semantic through the whole program. The domain's memory
    model is for now based on the <Simple_memory> functor provided by Eva.
    A reduced product with the Cvalue domain is performed at each step.
    For more details, one can look at M. Jacquemin's thesis. *)

val registered : Abstractions.Domain.registered
