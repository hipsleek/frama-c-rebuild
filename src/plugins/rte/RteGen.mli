(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Consult internal plug-in documentation for more details *)

(** Flags for filtering Alarms *)
module Flags : module type of Flags

(** RTE Generator Status & Emitters *)
module Generator : module type of Generator

(** Visitors to iterate over Alarms and/or generate Code-Annotations *)
module Visit : sig
  open Cil_types

  val annotate: ?flags:Flags.t -> kernel_function -> unit

  val get_annotations_kf:
    ?flags:Flags.t -> kernel_function -> code_annotation list

  val get_annotations_stmt:
    ?flags:Flags.t -> kernel_function -> stmt -> code_annotation list

  val get_annotations_exp:
    ?flags:Flags.t -> kernel_function -> stmt -> exp -> code_annotation list

  val get_annotations_lval:
    ?flags:Flags.t -> kernel_function -> stmt -> lval -> code_annotation list

  type on_alarm =
    kernel_function -> stmt -> invalid:bool -> Alarms.alarm -> unit
  type 'a iterator =
    ?flags:Flags.t -> on_alarm -> Kernel_function.t -> stmt -> 'a -> unit

  val iter_lval : lval iterator
  val iter_exp : exp iterator
  val iter_instr : instr iterator
  val iter_stmt : stmt iterator

  val register :
    Emitter.t -> kernel_function -> stmt -> invalid:bool -> Alarms.alarm ->
    code_annotation * bool
end

(** Same result as having [-rte] on the command line *)
val compute : unit -> unit

module Options : sig

  module DoShift : Parameter_sig.Bool
  module DoDivMod : Parameter_sig.Bool
  module DoFloatToInt : Parameter_sig.Bool
  module DoInitialized : Parameter_sig.Kernel_function_set
  module DoMemAccess : Parameter_sig.Bool
  module DoPointerCall : Parameter_sig.Bool
  module Trivial : Parameter_sig.Bool

end
