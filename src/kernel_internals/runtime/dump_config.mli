(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val dump_to_json : unit -> Yojson.Basic.t
(** Builds a Json object describing the Frama-C configuration. *)

val dump_to_stdout : unit -> unit
(** Dumps a Json object describing the Frama-C configuration to stdout. *)
