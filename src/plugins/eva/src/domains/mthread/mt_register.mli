(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type value = Cvalue.V.t
type 'v result = 'v Mt_utils.Result.t

(* ----- Threads ------------------------------------------------------------ *)

type thread_status =
  { running : Mt_utils.trilean ; canceled : Mt_utils.trilean }

module Thread : sig
  include Datatype.S_with_collections
  val id : t -> int
  val empty : t
  val top : t
  val is_included : t -> t -> bool
  val join : t -> t -> t
  val narrow : t -> t -> t
  val find : Thread.t -> t -> thread_status option

  val register : Thread.t list -> t -> (t * value) result
  val start    : value -> t -> (t * value) result
  val suspend  : value -> t -> (t * value) result
  val cancel   : value -> t -> (t * value) result
end

(* ----- Mutex -------------------------------------------------------------- *)

type mutex_status =
  | Locked (* Surely locked *)
  | Unlocked (* Maybe unlocked *)

module Mutex : sig
  include Datatype.S_with_collections
  val id : t -> int
  val empty : t
  val top : t
  val is_included : t -> t -> bool
  val join : t -> t -> t
  val narrow : t -> t -> t

  val register : Mutex.t list -> t -> (t * value) result
  val lock     : value -> t -> (t * value) result
  val unlock   : value -> t -> (t * value) result

  val locked_mutexes : t -> Mutex.Set.t
end
