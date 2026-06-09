(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Generate C implementations of E-ACSL predicates. *)

val generalized_untyped_to_exp:
  adata:Assert.t ->
  ?name:string ->
  kernel_function ->
  ?rte:bool ->
  Env.t ->
  predicate ->
  exp * Assert.t * Env.t
(** Convert an untyped ACSL predicate into a corresponding C expression. *)

val do_it:
  kernel_function ->
  Env.t ->
  toplevel_predicate ->
  Env.t
(** Translate the given predicate to a runtime check in the given environment.
    If [pred_to_print] is set, then the runtime check will use this predicate as
    message. *)

val to_exp :
  adata:Assert.t ->
  ?inplace:bool ->
  ?name:string ->
  kernel_function ->
  ?rte:bool ->
  Env.t ->
  predicate ->
  exp * Assert.t * Env.t
(** [to_exp ~adata ?inplace ?name kf ?rte env p] translates an ACSL predicate into a C expression.
    - [adata]: assertion context
    - [inplace]: if the root predicate has a label, indicates if it should be
      immediately translated or if [Translate_ats] should be used to retrieve the translation
    - [name]: name to use for generated variables
    - [kf]: the enclosing function.
    - [rte]: if true, generate and translate RTE before translating the predicate
    - [env]: the current environment
    - [p]: the predicate to translate *)

exception No_simple_translation of predicate
(** Exception raised if [untyped_to_exp] would generate new statements in the
    environment *)

val untyped_to_exp: predicate -> exp
(** Convert an untyped ACSL predicate into a corresponding C expression. This
    expression is valid only in certain contexts and shouldn't be used. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

module Translate_rtes : sig
  val translate_rte_annots_ref:
    ((Format.formatter -> code_annotation -> unit) ->
     code_annotation ->
     kernel_function ->
     Env.t ->
     code_annotation list ->
     Env.t) ref

  val translate_rte_exp_ref:
    (?filter:(code_annotation -> bool) ->
     kernel_function ->
     Env.t ->
     exp ->
     Env.t) ref
end
