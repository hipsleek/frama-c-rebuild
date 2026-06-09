(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang

(** VList Theory Builtins

    Empty API, the module only registers builtins. *)

val check_tau : tau -> bool
val check_term : F.term -> bool
val alist : tau -> tau (* element -> list of element *)
val elist : tau -> tau option (* list -> element *)

class type engine =
  object
    method callstyle : Qed.Engine.callstyle
    method pp_atom : Format.formatter -> F.term -> unit
    method pp_flow : Format.formatter -> F.term -> unit
  end

val f_nil : Fun.t
val f_elt : Fun.t
val f_nth : Fun.t
val f_cons : Fun.t
val f_concat : Fun.t
val f_repeat : Fun.t

val list : F.term list -> F.term
val concat : F.term list -> F.term
val repeat : F.term -> F.term -> F.term

val export : #engine -> Format.formatter -> F.term list -> unit
val pretty : #engine -> Format.formatter -> F.term list -> unit
val elements : #engine -> Format.formatter -> F.term list -> unit
val pprepeat : #engine -> Format.formatter -> F.term list -> unit
val shareable : F.term -> bool

val specialize_eq_list: Lang.For_export.specific_equality
