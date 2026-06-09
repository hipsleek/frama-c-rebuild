(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Generate C implementations of user-defined logic functions.
    A logic function can have multiple C implementations depending on
    the types computed for its arguments.
    Eg: Consider the following definition: [integer g(integer x) = x]
      with the following calls: [g(5)] and [g(10*INT_MAX)]
      They will respectively generate the C prototypes [int g_1(int)]
      and [long g_2(long)] *)

(**************************************************************************)
(************** Logic functions without labels ****************************)
(**************************************************************************)

val reset: unit -> unit

val app_to_exp:
  adata:Assert.t ->
  loc:location ->
  ?tapp:term ->
  kernel_function ->
  Env.t ->
  ?eargs:exp list ->
  logic_info ->
  term list ->
  exp * Assert.t * Env.t
(** Translate a Tapp term or a Papp predicate to an expression. If the optional
    argument [eargs] is provided, then these expressions are used as arguments
    of the function. The optional argument [tapp] is the term corresponding to
    the call, in case we are translating a term *)

val add_generated_functions_to_file: file -> unit
(** Insert into the globals of the given file the generated kernel functions
    (their declaration and their definition). Also registers these functions
    using {!Globals.Functions.register}. *)

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
