(******************************************************************************)
(*                                                                            *)
(*  SPDX-License-Identifier LGPL-2.1                                          *)
(*  Copyright (C)                                                             *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)      *)
(*  INRIA (Institut National de Recherche en Informatique et en Automatique)  *)
(*                                                                            *)
(******************************************************************************)

(** Smart constructors for logic annotations.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)

open Cil_types

(* ************************************************************************** *)
(** {2 Nodes with a unique ID} *)
(* ************************************************************************** *)

(** creates a code annotation with a fresh id. *)
val new_code_annotation : code_annotation_node -> code_annotation

(** @return a fresh id for a code annotation. *)
val fresh_code_annotation: unit -> int

(** set a fresh id to an existing code annotation*)
val refresh_code_annotation: code_annotation -> code_annotation

(** set fresh id to properties of an existing funspec
    @since Sodium-20150201
*)
val refresh_spec: funspec -> funspec

(** creates a new toplevel predicate.
    [predicate_kind] is [Assert] by default. It can be set to:
    - [Check] for a predicate that should only be used to check a property,
      without adding it as hypothesis for the rest of the verification.
    - [Admit] for a predicate that is an hypothesis for the rest of the
      verification and should not be checked by Frama-C.

    See {!Cil_types.toplevel_predicate} for more information.
    @since 22.0-Titanium
*)
val toplevel_predicate: ?kind:predicate_kind -> predicate -> toplevel_predicate

(** creates a new identified predicate with a fresh id.
    @before 22.0-Titanium no [only_check] parameter.
    @before 23.0-Vanadium [kind] parameter was named [only_check].
*)
val new_predicate: ?kind:predicate_kind -> predicate -> identified_predicate

(** creates a new acsl_extension with a fresh id.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf>
    @since Chlorine-20180501
    @before the function took one less argument, [~plugin] which is now
    used to set the [ext_plugin] field.
*)
val new_acsl_extension: plugin:string -> string -> location -> bool ->
  acsl_extension_kind -> acsl_extension

(** Gives a new id to an existing predicate.
    @since Oxygen-20120901
*)
val refresh_predicate: identified_predicate -> identified_predicate

(** @return a fresh id for predicates *)
val fresh_predicate_id: unit -> int

(** extract a named predicate for an identified predicate. *)
val pred_of_id_pred: identified_predicate -> predicate

(** creates a new identified term with a fresh id*)
val new_identified_term: term -> identified_term

(** Gives a new id to an existing term.
    @since Oxygen-20120901 *)
val refresh_identified_term: identified_term -> identified_term

(** @return a fresh id from an identified term*)
val fresh_term_id: unit -> int

(* ************************************************************************** *)
(** {2 Logic labels} *)
(* ************************************************************************** *)

val pre_label: logic_label
val post_label: logic_label
val here_label: logic_label
val old_label: logic_label
val loop_current_label: logic_label
val loop_entry_label: logic_label

(** @since Sodium-20150201 *)
val init_label: logic_label

(* ************************************************************************** *)
(** {2 Predicates} *)
(* ************************************************************************** *)

(** makes a predicate with the given names and location. Default name is empty
    and default location is unknown.
    @since 33.0-Arsenic *)
val pred: ?loc:location -> ?names:string list -> predicate_node -> predicate

(** makes a predicate with no name. Default location is unknown.*)
val unnamed: ?loc:location -> predicate_node -> predicate

(** prepend the given [names] to the the predicate's names.
    @since 33.0-Arsenic *)
val prepend_names: names:string list -> predicate -> predicate

(** \true *)
val ptrue: predicate

(** \false *)
val pfalse: predicate

(** \old *)
val pold: ?loc:location -> ?names:string list -> predicate -> predicate

(** application of predicate*)
val papp:
  ?loc:location -> ?names:string list ->
  logic_info * logic_label list * term list ->
  predicate

(** && *)
val pand:
  ?loc:location -> ?names:string list -> predicate * predicate -> predicate

(** || *)
val por:
  ?loc:location -> ?names:string list -> predicate * predicate -> predicate

(** ^^ *)
val pxor:
  ?loc:location -> ?names:string list -> predicate * predicate -> predicate

(** ! *)
val pnot: ?loc:location -> ?names:string list -> predicate -> predicate

(** Folds && over a list of predicates. *)
val pands: ?names:string list -> predicate list -> predicate

(** Folds || over a list of predicates. *)
val pors: ?names:string list -> predicate list -> predicate

(** local binding *)
val plet:
  ?loc:location -> ?names:string list -> logic_info -> predicate -> predicate

(** ==> *)
val pimplies :
  ?loc:location -> ?names:string list -> predicate * predicate -> predicate

(** ? : *)
val pif:
  ?loc:location -> ?names:string list ->
  predicate * predicate * predicate -> predicate

(** <==> *)
val piff:
  ?loc:location -> ?names:string list -> predicate * predicate -> predicate

(** Binary relation.
    @see <https://frama-c.com/download/frama-c-plugin-development-guide.pdf> *)
val prel:
  ?loc:location -> ?names:string list -> relation * term * term -> predicate

(** \forall *)
val pforall:
  ?loc:location -> ?names:string list -> quantifiers * predicate -> predicate

(** \exists *)
val pexists:
  ?loc:location -> ?names:string list -> quantifiers * predicate -> predicate

(** \fresh(pt,size) *)
val pfresh:
  ?loc:location -> ?names:string list ->
  logic_label * logic_label * term * term -> predicate

(** \allocable *)
val pallocable:
  ?loc:location -> ?names:string list -> logic_label * term -> predicate

(** \freeable *)
val pfreeable:
  ?loc:location -> ?names:string list -> logic_label * term -> predicate

(** \valid_read *)
val pvalid_read:
  ?loc:location -> ?names:string list -> logic_label * term -> predicate

(** \valid *)
val pvalid:
  ?loc:location -> ?names:string list -> logic_label * term -> predicate

(** \object_pointer *)
val pobject_pointer:
  ?loc:location -> ?names:string list -> logic_label * term -> predicate

(** \valid_function *)
val pvalid_function: ?loc:location -> ?names:string list -> term -> predicate

(** \initialized *)
val pinitialized:
  ?loc:location -> ?names:string list -> logic_label * term -> predicate

(** \dangling *)
val pdangling:
  ?loc:location -> ?names:string list -> logic_label * term -> predicate

(** \aligned *)
val paligned: ?loc:location -> ?names:string list -> term * term -> predicate

(** \at *)
val pat:
  ?loc:location -> ?names:string list -> predicate * logic_label -> predicate

(** \valid_index: requires index having integer type or set of integers *)
val pvalid_index:
  ?loc:location -> ?names:string list -> logic_label * term * term -> predicate

(** \valid_range: requires bounds having integer type *)
val pvalid_range:
  ?loc:location -> ?names:string list ->
  logic_label * term * term * term -> predicate

(** \separated *)
val pseparated: ?loc:location -> ?names:string list -> term list -> predicate

(* ************************************************************************** *)
(** {2 Logic types} *)
(* ************************************************************************** *)

(** instantiate type variables in a logic type.
    @since 18.0-Argon moved from Logic_utils *)
val instantiate :
  (string * logic_type) list ->
  logic_type -> logic_type

(** @return [true] if the logic type definition can be expanded.
    @since 18.0-Argon *)
val is_unrollable_ltdef : logic_type_info -> bool

(** expands logic type definitions only.
    To expands both logic part and C part, uses {!Ast_types.unroll_logic}.
    @since 18.0-Argon *)
val unroll_ltdef : logic_type -> logic_type

(** [isLogicType test typ] is [false] for pure logic types and the result
    of test for C types. *)
val isLogicCType : (typ -> bool) -> logic_type -> bool

(** returns [true] if the type is a list<t>.
    @since Aluminium-20160501 *)
val is_list_type: logic_type -> bool

(** [make_type_list_of t] returns the type list<[t]>.
    @since Aluminium-20160501 *)
val make_type_list_of: logic_type -> logic_type

(** returns the type of elements of a list type.
    @raise Failure if the input type is not a list type.
    @since Aluminium-20160501 *)
val type_of_list_elem: logic_type -> logic_type

(** returns [true] if the type is a set<t>.
    @since Neon-20140301 *)
val is_set_type: logic_type -> bool

(** [set_conversion ty1 ty2] returns a set type as soon as [ty1] and/or [ty2]
    is a set. Elements have type [ty1], or the type of the elements of [ty1] if
    it is itself a set-type (i.e. we do not build set of sets that way).
*)
val set_conversion: logic_type -> logic_type -> logic_type

(** converts a type into the corresponding set type if needed. Does nothing
    if the argument is already a set type. *)
val make_set_type: logic_type -> logic_type

(** returns the type of elements of a set type.
    @raise Failure if the input type is not a set type. *)
val type_of_element: logic_type -> logic_type

(** [plain_or_set f t] applies [f] to [t] or to the type of elements of [t]
    if it is a set type. *)
val plain_or_set: (logic_type -> 'a) -> logic_type -> 'a

(** [transform_element f t] is the same as
    [set_conversion (plain_or_set f t) t]
    @since Nitrogen-20111001
*)
val transform_element: (logic_type -> logic_type) -> logic_type -> logic_type

(** [true] if the argument is not a set type. *)
val is_plain_type: logic_type -> bool

(** [make_arrow_type args rt] returns a [rt] if [args] is empty or the
    corresponding [Larrow] type.

    @since 25.0-Manganese
*)
val make_arrow_type: logic_var list -> logic_type -> logic_type

(** @return true if the argument is the boolean type. *)
val is_boolean_type: logic_type -> bool

(* ************************************************************************** *)
(** {1 Logic Terms} *)
(* ************************************************************************** *)

(** returns a anonymous term of the given type. *)
val term : ?loc:Fileloc.t -> term_node -> logic_type -> term

(** [..] of integers *)
val trange: ?loc:Fileloc.t -> term option * term option -> term

(** boolean constant
    @since 30.0-Zinc
*)
val tboolean: ?loc:Fileloc.t -> bool -> term

(** integer constant *)
val tinteger: ?loc:Fileloc.t -> int -> term

(** integer constant *)
val tinteger_s64: ?loc:Fileloc.t -> int64 -> term

(** integer constant
    @since Oxygen-20120901 *)
val tint: ?loc:Fileloc.t -> Z.t -> term

(** real constant *)
val treal: ?loc:Fileloc.t -> float -> term

(** real zero *)
val treal_zero: ?loc:Fileloc.t -> ?ltyp:logic_type -> unit -> term

(** string constant *)
val tstring: ?loc:Fileloc.t -> string -> term

(** \at *)
val tat: ?loc:Fileloc.t -> term * logic_label -> term

(** \old
    @since Nitrogen-20111001
*)
val told: ?loc:Fileloc.t -> term -> term

(** variable *)
val tvar: ?loc:Fileloc.t -> logic_var -> term

(** \result *)
val tresult: ?loc:Fileloc.t -> typ -> term

(** cast to the given C type *)
val tcast: ?loc:Fileloc.t -> term -> typ -> term

(** coercion to the given logic type *)
val tlogic_coerce: ?loc:Fileloc.t -> term -> logic_type -> term

(** [alignof(t)] *)
val talignof: ?loc:Fileloc.t -> typ -> term

(** [true] if the term is \result (potentially enclosed in \at)*)
val is_result: term -> bool

(** [true] if the term is \exit_status (potentially enclosed in \at)
    @since Nitrogen-20111001
*)
val is_exit_status: term -> bool

(* ************************************************************************** *)
(** {1 Logic Offsets} *)
(* ************************************************************************** *)

(** Equivalent to [lastOffset] for terms.
        @since Oxygen-20120901 *)
val lastTermOffset: term_offset -> term_offset

(** Equivalent to [addOffset] for terms.
        @since Oxygen-20120901 *)
val addTermOffset:     term_offset -> term_offset -> term_offset

(** Equivalent to [addOffsetLval] for terms.
        @since Oxygen-20120901 *)
val addTermOffsetLval: term_offset -> term_lval -> term_lval
