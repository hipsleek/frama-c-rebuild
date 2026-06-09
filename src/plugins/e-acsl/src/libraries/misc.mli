(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Utilities for E-ACSL. *)

open Cil_types

(* ************************************************************************** *)
(** {2 Handling \result} *)
(* ************************************************************************** *)

val result_lhost: kernel_function -> lhost
(** @return the lhost corresponding to \result in the given function *)

val result_vi: kernel_function -> varinfo
(** @return the varinfo corresponding to \result in the given function *)

(* ************************************************************************** *)
(** {2 Other stuff} *)
(* ************************************************************************** *)

val is_fc_or_compiler_builtin: varinfo -> bool

val is_fc_stdlib_generated: varinfo -> bool
(** @return true if the [varinfo] is a generated stdlib function. (For instance
    generated function by the Variadic module. *)

val cty: logic_type -> typ
(** Assume that the logic type is indeed a C type. Just return it. *)

val ptr_base: loc:location -> exp -> exp
(** Takes an expression [e] and return [base] where [base] is the address [p]
    if [e] is of the form [p + i] and [e] otherwise. *)

val ptr_base_and_base_addr: loc:location -> exp -> exp * exp
(* Takes an expression [e] and return a tuple [(base, base_addr)] where [base]
   is the address [p] if [e] is of the form [p + i] and [e] otherwise, and
   [base_addr] is the address [&p] if [e] is of the form [p + i] and 0
   otherwise. *)

val term_of_li: logic_info -> term
(** [term_of_li li] assumes that [li.l_body] matches [LBterm t]
    and returns [t]. *)

val is_set_of_ptr_or_array: logic_type -> bool
(** Checks whether the given logic type is a set of pointers. *)

val is_range_free: term -> bool
(** @return true iff the given term does not contain any range. *)

val is_bitfield_pointers: logic_type -> bool
(** @return true iff the given logic type is a bitfield pointer or a
    set of bitfield pointers. *)

val term_has_lv_from_vi: term -> bool
(** @return true iff the given term contains a variables that originates from
    a C varinfo, that is a non-purely logic variable. *)

val name_of_binop: binop -> string
(** @return the name of the given binop as a string. *)

val make_binop: loc:location -> binop -> exp -> exp -> exp
(** Calls {!Cil.mkBinOp_exn} with [constfold] set to [true].
    @since 33.0-Arsenic *)

val finite_min_and_max: Ival.t -> Z.t * Z.t
(** [finite_min_and_max i] takes the finite ival [i] and returns its bounds. *)

module Id_term : sig
  include Datatype.S_with_hashtbl with type t = term

  val deep_copy : t -> t
  (** @return a copy of the given term with all sub-terms being copied as well.
      If a term already in the AST is added another time somewhere else in the
      AST, it has to be unshared in this way, so as to preserve the invariant:
      two term nodes in the AST may not be physically identical.
  *)

  val deep_copy_predicate : predicate -> predicate
  (** @return a predicate with all sub-terms occurring within being unshared. *)
end
(** Datatype for terms that relies on physical equality.
    Note that of its collections only [Hashtbl] can be used.
    Using [Map] and [Set] raises a fatal error as they require a comparison
    function, which cannot be defined in a sound way for physical equality. *)

val extract_uncoerced_lval: exp -> exp option
(** Unroll the [CastE] part of the expression until an [Lval] is found, and
    return it.

    If at some point the expression is neither a [CastE] nor an [Lval], then
    return [None]. *)

val labels_are_all_here : logic_label list -> bool
(** @return [true] if all labels are the builtin label Here (or list is empty). *)

val unghost_type : typ -> typ
(** remove all occurrences (also deep ones) of the "ghost" attribute. *)
