(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- String Constants                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Lang.F

type cst =
  | C_str of string (** String Literal *)
  | W_str of int64 list (** Wide String Literal *)

val pretty : Format.formatter -> cst -> unit

val str_len : cst -> term -> pred
(** Property defining the size of the string in bytes,
    with [\0] terminator included. *)

val str_val : cst -> term
(** The array containing the [char] of the constant *)

val str_id : cst -> int
(** Non-zero integer, unique for each different string literal *)

val char_at : cst -> term -> term

val cluster : unit -> Definitions.cluster
(** The cluster where all strings are defined. *)
