(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

type t
val is_active: t -> behavior -> Alarmset.status
val is_active_from_name: t -> string -> Alarmset.status
val active_behaviors: t -> behavior list
val create: (predicate -> Alarmset.status) -> spec -> t
