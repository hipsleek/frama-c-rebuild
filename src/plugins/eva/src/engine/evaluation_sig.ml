(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

(** Generic evaluation and reduction of expressions and left values. *)

module type S = sig

  (** State of abstract domain. *)
  type state

  (** Context *)
  type context

  (** Numeric values to which the expressions are evaluated. *)
  type value

  (** Origin of values. *)
  type origin

  (** Location of an lvalue. *)
  type loc

  (** Results of an evaluation: the results of all intermediate calculation (the
      value of each expression and the location of each lvalue) are cached here.
      See {!Eval} for more details. *)
  module Valuation : Valuation with type value = value
                                and type origin = origin
                                and type loc = loc

  (** Evaluation functions store the results of an evaluation into [Valuation.t]
      maps. Abstract domains read these results from [Abstract_domain.valuation]
      records. This function converts the former into the latter. *)
  val to_domain_valuation:
    Valuation.t -> (value, loc, origin) Abstract_domain.valuation

  (** [evaluate ~valuation state expr] evaluates the expression [expr] in the
      state [state], and returns the pair [result, alarms], where:
      - [alarms] are the set of alarms ensuring the soundness of the evaluation;
      - [result] is either `Bottom if the evaluation leads to an error,
        or `Value (valuation, value), where [value] is the numeric value computed
        for the expression [expr], and [valuation] contains all the intermediate
        results of the evaluation.

      Optional arguments are:
      - [valuation] is a cache of already computed expressions; empty by default.
      - [reduction] allows the deactivation of the backward reduction performed
        after the forward evaluation; true by default.
      - [subdivnb] is the maximum number of subdivisions performed on non-linear
        sub-expressions of [expr]. If a lvalue occurs several times in [expr],
        its value can be split up to [subdivnb] times to gain more precision.
        Set to the value of the option -eva-subdivide-non-linear by default. *)
  val evaluate :
    ?valuation:Valuation.t -> ?reduction:bool -> ?subdivnb:int ->
    state -> exp -> (Valuation.t * value) evaluated

  (** Computes the value of a lvalue, with possible indeterminateness: the
      returned value may be uninitialized, or contain escaping addresses.
      Also returns the alarms resulting of the evaluation of the lvalue location,
      and a valuation containing all the intermediate results of the evaluation.
      The [valuation] argument is a cache of already computed expressions.
      It is empty by default.
      [subdivnb] is the maximum number of subdivisions performed on non-linear
      expressions. *)
  val copy_lvalue :
    ?valuation:Valuation.t -> ?subdivnb:int ->
    state -> lval -> (Valuation.t * value flagged_value) evaluated

  (** [lvaluate ~valuation ~for_writing state lval] evaluates the left value
      [lval] in the state [state]. Same general behavior as [evaluate] above
      but evaluates the lvalue into a location and its type.
      The boolean [for_writing] indicates whether the lvalue is evaluated to be
      read or written. It is useful for the emission of the alarms, and for the
      reduction of the location.
      [subdivnb] is the maximum number of subdivisions performed on non-linear
      expressions (including the possible pointer and offset of the lvalue). *)
  val lvaluate :
    ?valuation:Valuation.t -> ?subdivnb:int -> for_writing:bool ->
    state -> lval -> (Valuation.t * loc) evaluated

  (** [reduce ~valuation state expr positive] evaluates the expression [expr]
      in the state [state], and then reduces the [valuation] such that
      the expression [expr] evaluates to a zero or a non-zero value, according
      to [positive]. By default, the empty valuation is used. *)
  val reduce:
    ?valuation:Valuation.t ->
    state -> exp -> bool -> Valuation.t evaluated

  (** [assume ~valuation state expr value] assumes in the [valuation] that
      the expression [expr] has the value [value] in the state [state],
      and backward propagates this information to the subterm of [expr].
      If [expr] has not been already evaluated in the [valuation], its forward
      evaluation takes place first, but the alarms are dismissed. By default,
      the empty valuation is used.
      The function returns the updated valuation, or bottom if it discovers
      a contradiction. *)
  val assume:
    ?valuation:Valuation.t ->
    state -> exp -> value -> Valuation.t or_bottom

  val eval_function:
    ?subdivnb:int -> lhost -> ?args:exp list -> state ->
    (Kernel_function.t * Valuation.t) list or_top_bottom * Alarmset.t
  (** Evaluation of the function argument of a [Cil_types.Call] constructor.
      In case of a function pointer, returns:
      - the alarms raised by the evaluation of the pointer, plus an alarm
        [Alarms.Function_pointer] if it may not evaluate to a valid function
        for arguments [args] (if provided).
      - `Top if the pointer cannot be evaluated (if cvalue is disabled);
      - `Bottom if the pointer evaluates to bottom.
      - or the list of possible functions. Each function [f] is paired with
        a valuation obtained by assuming the pointer evaluates exactly to [f]. *)

  val interpret_truth:
    alarm:(unit -> Alarms.t) -> 'a -> 'a Abstract_value.truth -> 'a evaluated
end
