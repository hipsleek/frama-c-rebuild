(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Loop specific actions. *)

open Cil_types
open Analyses_types

(**************************************************************************)
(************************* Loop annotations *******************************)
(**************************************************************************)

val handle_annotations:
  Env.t -> Kernel_function.t -> stmt -> stmt * Env.t
(** Modify the given stmt loop to insert the code which verifies the loop
    annotations, ie. preserves its loop invariants and checks the loop variant.
    Also return the modified environment. *)

(**************************************************************************)
(**************************** Nested loops ********************************)
(**************************************************************************)

val mk_nested_loops:
  loc:location -> (Env.t -> stmt list * Env.t) -> kernel_function -> Env.t ->
  lscope_var list -> stmt list * Env.t
(** [mk_nested_loops ~loc mk_innermost_block kf env lvars] creates nested
    loops (with the proper statements for initializing the loop counters)
    from the list of logic variables [lvars]. Quantified variables create
    loops while let-bindings simply create new variables.
    The [mk_innermost_block] closure creates the statements of the innermost
    block. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

val translate_predicate_ref:
  (kernel_function -> Env.t -> toplevel_predicate -> Env.t) ref

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
