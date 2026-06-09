(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

val emitter: Emitter.emitter

val aux_import: File.code_transformation_category
val main_import: File.code_transformation_category

(** {1 Messages and warning categories} *)

(* Messages leading also to a call to [annot_error] in raising an exception.
   So, they can always be considered an informative message.
   Kernel category: "annot-error"
   @raise the given exception *)
val annot_warning: ?source:Filepos.t -> raising:(unit -> 'b)
  -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(* Kernel category: "annot-error" *)
val annot_error: ?source:Filepos.t
  -> ('a,Format.formatter,unit) format -> 'a

val wkey_integer_cast: warn_category

(** {1 Options} *)

val find_ulevel_spec: string -> int -> string -> bool * int
val is_importation_on: unit -> bool
val set_importation_off: unit -> unit

val continue_after_parsing: unit -> bool
val continue_after_typing: unit -> bool

val is_unroll_loop_pragma_on: unit -> bool
val is_unroll_loop_condition_on: unit -> bool

module AddonEnsuresAndExits : Parameter_sig.Bool
module Idirs : Parameter_sig.String_list
module AddonIntegerCast: Parameter_sig.Bool
module Run : Parameter_sig.Bool
module Import : Parameter_sig.String_list
