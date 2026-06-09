(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Convert quantifiers. *)

open Cil_types

val quantif_to_exp:
  kernel_function -> Env.t -> predicate -> exp * Env.t
(** The given predicate must be a quantification. *)

(* ***********************************************************************)
(** {2 Forward references} *)
(* ***********************************************************************)

val predicate_to_exp_ref:
  (adata:Assert.t ->
   kernel_function ->
   Env.t ->
   predicate ->
   exp * Assert.t * Env.t) ref
