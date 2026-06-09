(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Taint status: untainted, direct taint, indirect taint or error. *)
module TaintStatus : Server.Data.S

(** Registers a hook applied each time some taint status may change. *)
val register_hook: (unit -> unit) -> unit

(** Taint status of a logic property. *)
val is_tainted_property: Property.identified_property -> TaintStatus.t
