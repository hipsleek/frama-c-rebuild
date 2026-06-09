(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* Create calls to a few memory built-ins.
   Partial support for ranges is provided. *)

val call:
  loc:location ->
  kernel_function ->
  string ->
  typ ->
  Env.t ->
  exp list ->
  exp * Env.t
(* [call ~loc kf name ctx env ts] creates a call to the E-ACSL memory built-in
   identified by [name] with the given arguments [ts].
   The supported built-ins are:
   [base_addr], [block_length], [offset], [freeable] and [aligned]. *)

val call_with_size:
  adata:Assert.t ->
  loc:location ->
  kernel_function ->
  typ ->
  Env.t ->
  predicate ->
  exp * Assert.t * Env.t
(* [call_with_size ~loc kf ctx env p] creates a call to the E-ACSL
   memory built-in identified by [p] which requires two arguments per term,
   namely the pointer under study and a size in bytes.
   The supported built-ins are: [initialized] and [separated]. *)

val call_valid:
  adata:Assert.t ->
  loc:location ->
  kernel_function ->
  typ ->
  Env.t ->
  predicate ->
  exp * Assert.t * Env.t
(* [call_valid ~loc kf name ctx env p] creates a call to the E-ACSL memory
   built-in [valid], [valid_read], or [object_pointer] according to [p]. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

val predicate_to_exp_ref:
  (adata:Assert.t ->
   kernel_function ->
   Env.t ->
   predicate ->
   exp * Assert.t * Env.t) ref

module Translate_terms : sig
  val to_exp_ref:
    (adata:Assert.t ->
     ?inplace:bool ->
     kernel_function ->
     Env.t ->
     term ->
     exp * Assert.t * Env.t) ref
end

val gmp_to_sizet_ref:
  (adata:Assert.t ->
   loc:location ->
   name:string ->
   ?check_lower_bound:bool ->
   ?pp:term ->
   kernel_function ->
   Env.t ->
   term ->
   exp * Assert.t * Env.t) ref
