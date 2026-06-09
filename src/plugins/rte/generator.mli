(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type status_accessor =
  string (* name *)
  * (Cil_types.kernel_function -> bool -> unit) (* for each kf and each kind of
                                                   annotation, set/unset the
                                                   fact that there has been
                                                   generated *)
  * (Cil_types.kernel_function -> bool) (* is this kind of annotation generated
                                           in kf? *)
module type S = sig
  val is_computed: Kernel_function.t -> bool
  val set: Kernel_function.t -> bool -> unit
  val accessor: status_accessor
end

(* No module for Trivial: dependency added for generators below *)

module Initialized: S
module Mem_access: S
module Pointer_alignment: S
module Pointer_value: S
module Pointer_call: S
module Div_mod: S
module Shift: S
module Left_shift_negative: S
module Right_shift_negative: S
module Signed_overflow: S
module Signed_downcast: S
module Unsigned_overflow: S
module Unsigned_downcast: S
module Pointer_downcast: S
module Float_to_int: S
module Finite_float: S
module Bool_value: S

val all_statuses: status_accessor list

(** The Emitter for Annotations registered by RTE *)
val emitter: Emitter.t

open Cil_types

(** Returns all annotations actually {i registered} by RTE so far *)
val get_registered_annotations: stmt -> code_annotation list

val self: State.t
