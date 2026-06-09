(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Debug Memory Model                                                 --- *)
(* -------------------------------------------------------------------------- *)

val pp_sequence : 'a Pretty_utils.formatter -> Format.formatter ->
  'a Memory.sequence -> unit
val pp_equation : Format.formatter -> Memory.equation -> unit
val pp_acs : Format.formatter -> Memory.acs -> unit
val pp_value : 'a Pretty_utils.formatter -> Format.formatter ->
  'a Memory.value -> unit
val pp_rloc : 'a Pretty_utils.formatter -> Format.formatter ->
  'a Memory.rloc -> unit
val pp_sloc : 'a Pretty_utils.formatter -> Format.formatter ->
  'a Memory.sloc -> unit

module Make(_ : Memory.Model) : Memory.Model
