(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

val valid_size: ?loc:location -> typ -> term -> identified_predicate

val is_allocable: ?loc:location -> term -> identified_predicate
val isnt_allocable: ?loc:location -> term -> identified_predicate

val assigns_result: ?loc:location -> typ -> term list -> from
val assigns_heap: term list -> from

val allocates_nothing: unit -> allocation
val allocates_result: ?loc:location -> typ -> allocation

val fresh_result: ?loc:location -> typ -> term -> identified_predicate
val null_result: ?loc:location -> typ -> identified_predicate
val aligned_result: ?loc:location -> typ -> identified_predicate
