(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Per-function computation of widening hints. *)

open Cil_types

(** [getWidenHints kf s] retrieves the set of widening hints related to
    function [kf] and statement [s]. *)
val getWidenHints: kernel_function -> stmt ->
  Base.Set.t * (Base.t -> Addresses.Bytes.widen_hint)

(** Parses all widening hints defined via the widen_hint syntax extension.
    The result is memoized for subsequent calls. *)
val precompute_widen_hints: unit -> unit
