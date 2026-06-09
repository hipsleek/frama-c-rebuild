(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types


val nul_exp : exp

(* exception raised when the program tries to access a memory location directly. *)
exception Explicit_pointer_address of location

module LvalOrRef : sig
  type t = Lval of lval | Ref of lval
  val pretty : Format.formatter -> t -> unit

  (* result stored in cache. May raise Explicit_pointer_address *)
  val from_exp : exp -> t option
end

module Lval : sig
  (* result stored in cache. May raise Explicit_pointer_address *)
  val simplify : lval -> lval
end

(** clear the two caches *)
val clear_cache : unit -> unit
