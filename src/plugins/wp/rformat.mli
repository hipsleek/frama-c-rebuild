(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Format

val epsilon : float

val get_time : float array -> float -> int
(** [get_time T t] returns [k] such that [T[k-1] <= t <= T[k]],
    [T] is extended with [T[-1]=0] and [T[N]=+oo]. *)

val pp_time : formatter -> float -> unit
(** Pretty print time in hour, minutes, seconds, or milliseconds, as appropriate *)

val pp_time_range : float array -> formatter -> float -> unit

type command =
  | CMD of string
  | ARG of string * string
  | TEXT

val command : string -> command

val pretty : (formatter -> string -> string -> unit) -> formatter -> string -> unit
