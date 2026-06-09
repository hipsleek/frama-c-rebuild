(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A bijective hash map implementation based on a pair of hash tables *)

module Make (H : Hashtbl.S) : sig
  val clear : unit -> unit

  val add : H.key -> H.key -> unit

  val tails : H.key -> H.key list
  val tail : H.key -> H.key
  val tail_opt : H.key -> H.key option

  val heads : H.key -> H.key list
  val head : H.key -> H.key
  val head_opt : H.key -> H.key option

  val tail_or_self : H.key -> H.key
  val head_or_self : H.key -> H.key
end
