(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Analyses_types

(** Utility functions for generating C implementations. *)

val must_translate: Property.t -> bool
(** @return true if the given property must be translated (ie. if the valid
    properties must be translated or if its status is not [True]), false
    otherwise. *)

val must_translate_opt: Property.t option -> bool
(** Same than [must_translate] but for [Property.t option]. Return false if the
    option is [None]. *)

val gmp_to_sizet:
  adata:Assert.t ->
  loc:location ->
  name:string ->
  ?check_lower_bound:bool ->
  ?pp:term ->
  kernel_function ->
  Env.t ->
  term ->
  exp * Assert.t * Env.t
(** Translate the given GMP integer to an expression of type [size_t]. RTE
    checks are generated to ensure that the GMP value holds in this type.
    The parameter [name] is used to generate relevant predicate names.
    If [check_lower_bound] is set to [false], then the GMP value is assumed to
    be positive.
    If [pp] is provided, this term is used in the messages of the RTE checks. *)

val comparison_to_exp :
  loc:location ->
  kernel_function ->
  Env.t ->
  number_ty ->
  binop -> exp -> exp -> ?name:string -> term option -> exp * Env.t
(** [comparison_to_exp ~loc kf env ity ?name bop e1 e2 topt] generates
    the C code equivalent to [e1 bop e2] in the given environment.
    [ity] is the number type of the comparison when comparing scalar numbers.
    [name] is used to generate temporary variable names.
    [topt] is the term holding the result of the comparison. *)

val conditional_to_exp:
  ?name:string ->
  loc:location ->
  kernel_function ->
  term option ->
  exp ->
  exp * Env.t ->
  exp * Env.t ->
  exp * Env.t
(** [conditional_to_exp ?name ~loc kf t_opt e1 (e2, env2) (e3, env3)] generates
    the C code equivalent to [e1 ? e2 : e3] in the given  environment.
    [env2] and [env3] are the environment respectively for [e2] and [e3].
    [t_opt] is the term holding the result of the conditional. *)

val env_of_li:
  adata:Assert.t ->
  loc:location ->
  kernel_function ->
  Env.t ->
  logic_info ->
  Assert.t * Env.t
(** [env_of_li ~adata ~loc kf env li] translates the logic info [li] in the
    given environment with the given assertion context. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

module Translate_terms : sig
  val to_exp_ref:
    (adata:Assert.t ->
     ?inplace:bool ->
     kernel_function ->
     Env.t ->
     term ->
     exp * Assert.t * Env.t) ref
end

val predicate_to_exp_ref:
  (adata:Assert.t ->
   ?name:string ->
   kernel_function ->
   ?rte:bool ->
   Env.t ->
   predicate ->
   exp * Assert.t * Env.t) ref
