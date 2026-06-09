(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Mt_utils

type return = { standard : Value.t }

module Domain : sig
  include Datatype.S_with_collections
  val threads : t -> Mt_register.Thread.t
  val mutexes : t -> Mt_register.Mutex.t
  val return  : t -> return
  val key : t Structure.Key_Domain.key
  val empty : unit -> t
end

val domain : Abstractions.Domain.registered
