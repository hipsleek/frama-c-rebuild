(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Generate C implementations of E-ACSL terms. *)

val to_exp:
  adata:Assert.t ->
  ?inplace:bool ->
  kernel_function ->
  Env.t ->
  term ->
  exp * Assert.t * Env.t
(** [to_exp ~adata ?inplace kf env t] converts an ACSL term into a
    corresponding C expression.
    - [adata]: assertion context.
    - [inplace]: if the root term has a label, indicates if it should be
      immediately translated or if [Translate_ats] should be used to retrieve
      the translation.
    - [kf]: The enclosing function.
    - [env]: The current environment.
    - [t]: The term to translate. *)

val to_exp_il : ?inplace:bool -> term -> Interlang.exp Interlang_gen.m
(** a version of [to_exp] that translates ACSL terms to the intermediate
    language instead to Cil. *)

exception No_simple_translation of term
(** Exception raised if [untyped_to_exp] would generate new statements in
    the environment *)

val untyped_to_exp: typ option -> term -> exp
(** Convert an untyped ACSL term into a corresponding C expression. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

val translate_rte_exp_ref:
  (?filter:(code_annotation -> bool) ->
   kernel_function ->
   Env.t ->
   exp ->
   Env.t) ref

module Translate_predicates : sig
  val to_exp_ref :
    (adata:Assert.t ->
     ?inplace:bool ->
     ?name:string ->
     kernel_function ->
     ?rte:bool ->
     Env.t ->
     predicate ->
     exp * Assert.t * Env.t)
      ref

  val rte_guards_to_exp_old_ref:
    ( loc:location ->
      kf:kernel_function ->
      term ->
      Env.t ->
      Env.t
    ) ref

  val rte_guards_to_exp_il_ref:
    (term ->  Interlang.rte list Interlang_gen.M.t) ref
end
