(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** This modules stores the alarms and properties for which a red status has
    been emitted. *)

(* Remembers that a red status has been emitted for an alarm or a property at
   the given kinstr. *)
val add_red_alarm:    kinstr -> Alarms.t -> unit
val add_red_property: kinstr -> Property.t -> unit

type alarm_or_property = Alarm of Alarms.t | Prop of Property.t

(* Whether a red status has been emitted for a property in any callstack. *)
val is_red: Property.t -> bool

(* If option -eva-report-red-statuses has been set, reports red statuses in
   a csv file. *)
val report: unit -> unit

(* Register a hook that is called each time a red status is set *)
val register_hook: (alarm_or_property -> unit) -> unit
