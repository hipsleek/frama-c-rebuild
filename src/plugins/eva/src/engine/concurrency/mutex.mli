(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Datatype.S_with_collections

val id : t -> int
val label : t -> string
val find : int -> t option
val create : Position.local -> Concurrency.Name.t option -> t
val reset_state : unit -> unit
