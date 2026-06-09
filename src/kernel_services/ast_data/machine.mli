(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module handle the machine configuration. Previous Frama-C
    versions handled this in {!Cil}.

    @since 30.0-Zinc
*)

open Cil_types

(* ***********************************************************************)
(** {2 State}                                                            *)
(* ***********************************************************************)

val self: State.t
(** Internal state of the machine. *)

val is_computed: ?project:Project.t -> unit -> bool
(** Whether current project has set its machine description. *)

(* ***********************************************************************)
(** {2 Names getters}                                                    *)
(* ***********************************************************************)

val size_t: unit -> string
val ssize_t: unit -> string
val wchar_t: unit -> string
val ptrdiff_t: unit -> string
val intptr_t: unit -> string
val uintptr_t: unit -> string
val int_fast8_t: unit -> string
val int_fast16_t: unit -> string
val int_fast32_t: unit -> string
val int_fast64_t: unit -> string
val uint_fast8_t: unit -> string
val uint_fast16_t: unit -> string
val uint_fast32_t: unit -> string
val uint_fast64_t: unit -> string
val wint_t: unit -> string
val sig_atomic_t: unit -> string
val time_t: unit -> string

(* ***********************************************************************)
(** {2 Types}                                                            *)
(* ***********************************************************************)

(** @since 32.0-Germanium *)
module type SizeofInfo = sig
  val short: unit -> int
  val int: unit -> int
  val long: unit -> int
  val longlong: unit -> int
  val ptr: unit -> int
  val float: unit -> int
  val double: unit -> int
  val longdouble: unit -> int
  val void: unit -> int (** might be -1 if unsupported in current machdep *)
  val func: unit -> int (** might be -1 if unsupported in current machdep *)
end

(* ***********************************************************************)
(** {2 [sizeof] getters}                                                 *)
(* ***********************************************************************)

(** @since 32.0-Germanium
    @before 32.0-Germanium These functions were at top-level and named sizeof_<type>
*)
module Sizeof : SizeofInfo

(* ***********************************************************************)
(** {2 [_Alignof] and GCC [__alignof__] getters}                         *)
(* ***********************************************************************)

(** @since 32.0-Germanium *)
module type AlignofInfo = sig
  include SizeofInfo
  val aligned: unit -> int (** might be -1 if unsupported in current machdep *)
  val max: unit -> int
  (** alignment for max_align_t. Note that:
      - it might not be the maximal alignment supported by the machine.
        For this, use {!max_extended_alignment}.
      - if the machdep does not define it, the call warns (once) and it is
        computed as the maximum of the known alignment values.
  *)
end

(** @since 32.0-Germanium
    @before 32.0-Germanium These functions were at top-level and named alignof_<type>
*)
module Alignof : AlignofInfo

(** @since 32.0-Germanium *)
module GCCAlignof : AlignofInfo

(* ***********************************************************************)
(** {2 Typ/kind getters}                                                 *)
(* ***********************************************************************)

val ptrdiff_kind: unit -> ikind
val ptrdiff_type: unit -> typ

val sizeof_kind: unit -> ikind
val sizeof_type: unit -> typ

val wchar_kind: unit -> ikind
val wchar_type: unit -> typ

val uintptr_kind: unit -> ikind
val uintptr_type: unit -> typ

val string_literal_type: unit -> typ

(* ***********************************************************************)
(** {2 Expansions getters}                                               *)
(* ***********************************************************************)

val weof: unit -> string
val wordsize: unit -> string
val posix_c_source: unit -> string
val bufsiz: unit -> string
val eof: unit -> string
val fopen_max: unit -> string
val filename_max: unit -> string
val host_name_max: unit -> string
val tty_name_max: unit -> string
val l_tmpnam: unit -> string
val path_max: unit -> string
val tmp_max: unit -> string
val rand_max: unit -> string
val mb_cur_max: unit -> string
val nsig: unit -> string

(* ***********************************************************************)
(** {2 Other getters}                                                    *)
(* ***********************************************************************)

val version: unit -> string

val compiler: unit -> string

val machdep_name: unit -> string

val get_machdep: unit -> Machdep.mach

val char_is_unsigned: unit -> bool

val little_endian: unit -> bool

val has_builtin_va_list: unit -> bool

val cpp_arch_flags: unit -> string list

val errno: unit -> (string * string) list

val custom_defs: unit -> (string * string) list

val use_logical_operators: unit -> bool

val lower_constants: unit -> bool
[@@deprecated "Use Kernel.Constfold.get instead."]
[@@migrate { repl = Kernel.Constfold.get }]

val insert_implicit_casts: unit -> bool

val max_extended_alignment: unit -> int
(** -1 if the platform does not support extended alignments
    @since 32.0-Germanium
*)

(* ***********************************************************************)
(** {2 Compiler }                                                        *)
(* ***********************************************************************)

val msvcMode: unit -> bool
(** Short for [Machdep.msvcMode (get_machdep ())]
    @since 30.0-Zinc  *)

val gccMode: unit -> bool
(** Short for [Machdep.gccMode (get_machdep ())]
    @since 30.0-Zinc  *)

val acceptEmptyCompinfo: unit -> bool
(** whether we accept empty struct. Implied by {!msvcMode} and {!gccMode}, and
    can be forced by {!set_acceptEmptyCompinfo} otherwise.
    @since 30.0-Zinc
*)

val set_acceptEmptyCompinfo: unit -> unit
(** After a call to this function, empty compinfos are allowed by the kernel,
    this must be used as a configuration step equivalent to a machdep, except
    that it is not a user configuration.

    Note that if the selected machdep is GCC or MSVC, this call has no effect
    as these modes already allow empty compinfos.

    @since 30.0-Zinc
*)

(* ***********************************************************************)
(** {2 Initializer }                                                     *)
(* ***********************************************************************)

(** Call this function to perform some initialization, and only after you have
    set {!msvcMode}. {!initLogicBuiltins} is the function to call to init
    logic builtins. The [Machdep] argument is a description of the hardware
    platform and of the compiler used. *)
val init: initLogicBuiltins:(unit -> unit) -> Machdep.mach -> unit

(* ***********************************************************************)
(** {2 Forward references}                                               *)
(* ***********************************************************************)

(** Unless your name is {!Cil_builtins}, you should not call this. *)
val init_builtins_ref: (unit -> unit) ref
[@@alert machine_init_builtins_ref
    "This function can only be called by Cil_builtins"]
