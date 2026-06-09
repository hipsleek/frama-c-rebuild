(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cvalue

val get_retres_vi: kernel_function -> varinfo option
(** Fake varinfo used by Value to store the result of functions. Returns
    [None] if the function has a void type. *)

val returned_value: kernel_function -> V.t

val warn_unsupported_spec : string -> unit
(** Warns on functions from the frama-c libc with unsupported specification. *)
