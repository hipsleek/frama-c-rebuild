(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Memory
open Lang
open Lang.F

type env
type label

type value =
  | Term
  | Addr of s_lval
  | Lval of s_lval * label
  | Init of s_lval * label
  | Chunk of Sigma.chunk * label

val create : unit -> env
val register : Conditions.sequence -> env

val at : env -> id:int -> label
val find : env -> F.term -> value
val updates : env -> label Memory.sequence -> Vars.t -> Memory.update Bag.t
val visible : label -> bool
val subterms : env -> (F.term -> unit) -> F.term -> bool
val prev : label -> label list
val next : label -> label list
val iter : (Memory.mval -> term -> unit) -> label -> unit
val branching : label -> bool

class virtual engine :
  object
    method virtual pp_atom : Format.formatter -> term -> unit
    method virtual pp_flow : Format.formatter -> term -> unit

    method is_atomic_lv : s_lval -> bool

    method pp_ofs : Format.formatter -> s_offset -> unit
    method pp_offset : Format.formatter -> s_offset list -> unit

    method pp_host : Format.formatter -> s_host -> unit (** current state *)

    method pp_lval : Format.formatter -> s_lval -> unit (** current state *)

    method pp_init : Format.formatter -> s_lval -> unit (** current state *)

    method pp_addr : Format.formatter -> s_lval -> unit

    method pp_label : Format.formatter -> label -> unit (** label name *)

    method pp_chunk : Format.formatter -> Sigma.chunk -> unit (** chunk name *)
  end
