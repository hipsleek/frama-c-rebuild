(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type t = {
  over_inputs: Memory_zone.t;
  over_inputs_if_termination: Memory_zone.t;
  over_logic_inputs: Memory_zone.t;
  under_outputs_if_termination: Memory_zone.t;
  over_outputs: Memory_zone.t;
  over_outputs_if_termination: Memory_zone.t;
}

include Datatype.S with type t := t

val pretty_operational_inputs: t Pretty_utils.formatter
(** Pretty-print the fields [over_inputs_if_termination], [over_inputs] and
    [under_outputs_if_termination] *)

val pretty_outputs: t Pretty_utils.formatter
(** Pretty-print the fields [over_outputs] and [over_outputs_if_termination]. *)

val map: (Memory_zone.t -> Memory_zone.t) -> t -> t

val bottom: t
val join: t -> t -> t
