(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Filtering Categories of Alarms *)
(* -------------------------------------------------------------------------- *)

(** Flags for controlling the low-level API. Each flag control whether
    a category of alarms will be visited or not. *)
type t = {
  remove_trivial: bool;
  initialized: Kernel_function.Set.t ;
  mem_access: bool;
  div_mod: bool;
  shift: bool;
  left_shift_negative: bool;
  right_shift_negative: bool;
  signed_overflow: bool;
  unsigned_overflow: bool;
  signed_downcast: bool;
  unsigned_downcast: bool;
  pointer_downcast: bool;
  float_to_int: bool;
  finite_float: bool;
  pointer_call: bool;
  pointer_alignment: bool;
  pointer_value: bool;
  bool_value: bool;
}

(** Defaults flags are taken from the Kernel and RTE plug-in options. *)
val default :
  ?remove_trivial:bool ->
  ?initialized:Kernel_function.Set.t ->
  ?mem_access:bool ->
  ?div_mod:bool ->
  ?shift:bool ->
  ?left_shift_negative:bool ->
  ?right_shift_negative:bool ->
  ?signed_overflow:bool ->
  ?unsigned_overflow:bool ->
  ?signed_downcast:bool ->
  ?unsigned_downcast:bool ->
  ?pointer_downcast:bool ->
  ?float_to_int:bool ->
  ?finite_float:bool ->
  ?pointer_call:bool ->
  ?pointer_alignment:bool ->
  ?pointer_value:bool ->
  ?bool_value:bool ->
  unit -> t

(** All flags set to [true], "@all" for initialized *)
val all : unit -> t

(** All flags set to [false], empty for initialized *)
val none : t

(* -------------------------------------------------------------------------- *)
