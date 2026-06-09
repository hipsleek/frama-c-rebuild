(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** AST manipulation utilities. *)

open Cil_types

(* ************************************************************************** *)
(** {2 Expressions} *)
(* ************************************************************************** *)

(** [true] iff the constant is an integer constant
    (i.e. neither a float nor a string). Enum tags and chars
    are integer constants.
*)
val is_integral_const: constant -> bool

(** returns the value of the corresponding integer literal or [None]
    if the constant is not an integer (i.e. {!is_integral_const} returns false).
*)
val possible_value_of_integral_const: constant -> Z.t option

(** returns the value of the corresponding integer constant expression, or
    [None] if the expression is not a constant expression or does not
    evaluate to an integer constant.

    @before 26.0-Iron the function only returned [Some v] when the
    expression was an integer literal (i.e. [Const c]).
*)
val possible_value_of_integral_expr: exp -> Z.t option

(** returns the value of the corresponding integer literal. It is
    the responsibility of the caller to ensure the constant is indeed
    an integer constant. If unsure, use {!possible_value_of_integral_const}.
*)
val value_of_integral_const: constant -> Z.t

(** returns the value of the corresponding integer constant expression. It
    is the responsibility of the caller to ensure that the argument is indeed
    an integer constant expression. If unsure, use
    {!possible_value_of_integral_expr}.

    @before 26.0-Iron the function would fail if the expression was not an
    integer literal (see {!possible_value_of_integral_expr}).
*)
val value_of_integral_expr: exp -> Z.t

(** [true] iff the expression is a constant expression that evaluates to
    a null pointer, i.e. 0 or a cast to 0.

    @before 26.0-Iron the function would return [false] as soon as the
    expression was not an integer literal (possibly casted).
*)
val is_null_expr: exp -> bool

(** [true] iff the expression is a constant expression that evaluates to
    a non-null pointer.

    {b Warning:} note that for the purpose of this function [&x] is {i not} a
    constant expression, hence the function will return [false] in this case.

    @before 26.0-Iron the function would return [false] as soon as
    the expression was not an integer literal (possibly casted).
*)
val is_non_null_expr: exp -> bool

(** [true] if the varinfo is a global variable representing a
    string literal.
    @since 32.0-Germanium
*)
val is_string_literal: varinfo -> bool

(* ************************************************************************** *)
(** {2 Logical terms} *)
(* ************************************************************************** *)

val is_integral_logic_const: logic_constant -> bool
(** @return [true] if the constant has integral type [(integer, char,
    enum)]. [false] otherwise.
    @since Oxygen-20120901 *)

val possible_value_of_integral_logic_const: logic_constant -> Z.t option
(** @return [Some n] if the constant has integral type [(integer, char,
    enum)]. [None] otherwise.
    @since Oxygen-20120901 *)

val value_of_integral_logic_const: logic_constant -> Z.t
(** @return the value of the constant.
    Assume the argument is an integral constant.
    @since Oxygen-20120901 *)

val possible_value_of_integral_term: term -> Z.t option
(** @return [Some n] if the term has integral type [(integer, char,
    enum)]. [None] Otherwise.
    @since Oxygen-20120901 *)

val term_lvals_of_term: term -> term_lval list
(** @return the list of all the term lvals of a given term.
    Purely syntactic function. *)

val precondition : goal:bool -> funspec -> predicate
(** Builds the precondition from [b_assumes] and [b_requires] clauses.
    With [~goal:true], only returns assert and check predicates.
    With [~goal:false], only returns assert and admit predicates.
    @since Carbon-20101201
    @before 23.0-Vanadium no [goal] flag.
*)

val behavior_assumes : funbehavior -> predicate
(** Builds the conjunction of the [b_assumes].
    @since Nitrogen-20111001 *)

val behavior_precondition : goal:bool -> funbehavior -> predicate
(** Builds the precondition from [b_assumes] and [b_requires] clauses.
    For flag [~goal] see {!Ast_info.precondition} above.
    @since Carbon-20101201
    @before 23.0-Vanadium no [goal] flag.
*)

val behavior_postcondition :
  goal:bool -> funbehavior -> termination_kind -> predicate
(** Builds the postcondition from [b_assumes] and [b_post_cond] clauses.
    For flag [~goal] see {!Ast_info.precondition} above.
    @before 23.0-Vanadium no [goal] flag.
*)

val disjoint_behaviors : funspec -> string list -> predicate
(** Builds the [disjoint_behaviors] property for the behavior names.
    @since Nitrogen-20111001 *)

val complete_behaviors : funspec -> string list -> predicate
(** Builds the [disjoint_behaviors] property for the behavior names.
    @since Nitrogen-20111001 *)

val merge_assigns_from_complete_bhvs:
  ?warn:bool -> ?unguarded:bool -> funbehavior list -> string list list -> assigns
(** @return the assigns of an unguarded behavior (when [unguarded]=true)
    or a set of complete behaviors.
    - the funbehaviors can come from either a statement contract or a function
      contract.
    - the list of sets of behavior names can come from the contract of the
      related function.

    Optional [warn] argument can be used to force emitting or cancelation of
    warnings.
    @since Oxygen-20120901 *)

val merge_assigns_from_spec: ?warn:bool -> funspec -> assigns
(** It is a shortcut for [merge_assigns_from_complete_bhvs
    spec.spec_complete_behaviors spec.spec_behavior].  Optional [warn] argument
    can be used to force emitting or cancelation of warnings
    @return the assigns of an unguarded behavior or a set of complete behaviors.
    @since Oxygen-20120901 *)

val merge_assigns: ?warn:bool -> funbehavior list -> assigns
(** Returns the assigns of an unguarded behavior.
    Optional [warn] argument can be used to force emitting or cancelation of
    warnings. *)

val variable_term: location -> logic_var -> term
val constant_term: location -> Z.t -> term
val is_null_term: term -> bool


(* ************************************************************************** *)
(** {2 Statements} *)
(* ************************************************************************** *)

val is_loop_statement: stmt -> bool
val get_sid: kinstr -> int

val mkassign: lval -> exp -> location -> instr
val mkassign_statement: lval -> exp -> location -> stmt

(** determines if a var is local to a block. *)
val is_block_local: varinfo -> block -> bool

val block_of_local: fundec -> varinfo -> block
(** [local_block f vi] returns the block of [f] in which [vi] is declared.
    [vi] must be a variable of [f]. *)

(* ************************************************************************** *)
(** {2 Types} *)
(* ************************************************************************** *)

val direct_array_size: typ -> Z.t
val array_size: typ -> Z.t

(* ************************************************************************** *)
(** {2 Functions} *)
(* ************************************************************************** *)

(** Operations on cil function. *)
module Function: sig
  val formal_args: varinfo -> (string * typ * attributes) list
  (** Returns the list of the named formal arguments of a function.
      Never call on a variable of non functional type.*)

  val is_formal: varinfo -> fundec -> bool
  val is_local: varinfo -> fundec -> bool
  val is_formal_or_local: varinfo -> fundec -> bool
  val is_formal_of_prototype:
    varinfo (* to check *) -> varinfo (* of the prototype *) -> bool
  (** [is_formal_of_prototype v f] returns [true] iff [f] is a prototype and
      [v] is one of its formal parameters. *)

  val is_definition: cil_function -> bool
  val get_vi: cil_function -> varinfo
  val get_name: cil_function -> string
  val get_id: cil_function -> int

  val get_statics: fundec -> varinfo list
  (** @since 29.0-Copper *)

end

(* ************************************************************************** *)
(** {2 Predefined} *)
(* ************************************************************************** *)

val start_with_frama_c: string -> bool
(** @return true if the given string starts with ["Frama_C_"].
    @since 29.0-Copper
*)

val is_show_each_builtin: string -> bool
(** @return true if the given string starts with ["Frama_C_show_each"].
    @since 29.0-Copper
*)

val is_domain_show_each_builtin: string -> bool
(** @return true if the given string starts with ["Frama_C_domain_show_each"].
    @since 29.0-Copper
*)

val is_dump_each_builtin: string -> bool
(** @return true if the given string starts with ["Frama_C_dump_each"].
    @since 29.0-Copper
*)

val is_dump_file_builtin: string -> bool
(** @return true if the given string starts with ["Frama_C_dump_each_file"].
    @since 29.0-Copper
*)

val is_split_builtin: string -> bool
(** @return true if the given string starts with ["Frama_C_builtin_split"].
    @since 29.0-Copper
*)

val start_with_frama_c_builtin: string -> bool
(** @return true if the given string starts with ["Frama_C_"].
    @since 29.0-Copper
*)

val is_frama_c_builtin: varinfo -> bool
(** @return true if {!start_with_frama_c_builtin} or
    {!Cil_builtins.has_fc_builtin_attr} are true.
    @before 29.0-Copper Behave like {!start_with_frama_c_builtin}.
*)
