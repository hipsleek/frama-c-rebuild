(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Module with the context to hold the data contributing to an assertion and
    general functions to create assertion statements. *)

open Cil_types
open Analyses_types

type t
(** Type to hold the data contributing to an assertion. *)

val empty: loc:location -> kernel_function -> Env.t -> t * Env.t
(** Empty assertion context. *)

val no_data: t
(** No data assertion context.

    Contrary to an empty assertion context, a "no data" assertion context will
    always have no data in it, even when calling [register] on it. The goal is
    to have a context to pass to functions that need one even if we do not need
    it afterwards. For instance there is no following assertion statement. *)


val with_data_from: loc:location -> kernel_function -> Env.t -> t -> t * Env.t
(** [with_data_from ~loc kf env from] creates a new assertion context with the
    same data than the [from] assertion context.
    If [from] is a "no data" assertion context, then the new context is also a
    "no data" assertion context. *)

val merge_right: loc:location -> Env.t -> t -> t -> t * Env.t
(** [merge_right ~loc env adata1 adata2] merges the assertion data of [adata1]
    into [adata2] if [adata2] is not a "no data" assertion context. *)

val clean: loc:location -> Env.t -> t -> Env.t
(** [clean ~loc env adata] generates a call to the C cleanup function for the
    assertion context. This function *must* be used if the assertion context is
    not given to [runtime_check] or [runtime_check_with_msg], otherwise the
    memory allocated in the C structure will not be freed. *)

val push_pending_register_data: unit -> unit
(** [push_pending_register_data] adds a data registration to a stack of pending
    data registration to be generated later *)

val do_pending_register_data:
  Env.t -> Env.t
(** [do_pending_register_data] performs all the pending restrations*)

val register:
  loc:location ->
  ?force:bool ->
  string ->
  exp ->
  t ->
  t
(** [register ~loc env ?force name e adata] registers the data [e] corresponding
    to the name [name] to the assertion context [adata].
    If [force] is false (default), the data is not registered if the expression
    is a constant. If [force] is true, the data is registered even if the
    expression is a constant. *)

val register_term:
  loc:location ->
  ?force:bool ->
  term ->
  exp ->
  t ->
  t
(** [register_term ~loc env ?force t e adata] registers the data [e]
    corresponding to the term [t] to the assertion context [adata]. The
    parameter [force] has the same signification than for the function
    [register]. *)

val register_pred:
  loc:location ->
  Env.t ->
  ?force:bool ->
  predicate ->
  exp ->
  t ->
  t
(** [register_pred ~loc env ?force p e adata] registers the data [e]
    corresponding to the predicate [p] to the assertion context [adata]. The
    parameter [force] has the same signification than for the function
    [register]. *)

val register_pred_or_term:
  loc:location ->
  Env.t ->
  ?force:bool ->
  pred_or_term ->
  exp ->
  t ->
  t
(** [register_pred_or_term ~loc kf env ?force pot e adata] registers the data
    [e] corresponding to the predicate or term [pot] to the assertion context
    [adata]. The parameter [force] has the same signification than for the
    function [register]. *)

val runtime_check:
  adata:t ->
  pred_kind:predicate_kind ->
  annotation_kind ->
  kernel_function ->
  Env.t ->
  exp ->
  predicate ->
  stmt * Env.t
(** [runtime_check ~adata ~pred_kind kind kf env e p] generates a runtime check
    for predicate [p] by building a call to [__e_acsl_assert]. [e] (or [!e] if
    [reverse] is set to [true]) is the C translation of [p].
    [adata] is the assertion context holding the data contributing to the
    assertion.
    [pred_kind] indicates if the assert should be blocking or not.
    [kind] is the annotation kind of [p].
    [kf] is the current kernel_function.
    [env] is the current environment. *)

val runtime_check_with_msg:
  adata:t ->
  loc:location ->
  ?name:string ->
  string ->
  pred_kind:predicate_kind ->
  annotation_kind ->
  kernel_function ->
  Env.t ->
  exp ->
  stmt * Env.t
(** [runtime_check_with_msg ~adata ~loc ?name msg ~pred_kind kind kf env e]
    generates a runtime check for [e] (or [!e] if [reverse] is [true]) by
    building a call to [__e_acsl_assert].
    [adata] is the assertion context holding the data contributing to the
    assertion.
    [loc] is the location printed in the message if the runtime check fails.
    [name] is the name of the predicate printed if the runtime check fails.
    [msg] is the message printed if the runtime check fails.
    [pred_kind] indicates if the assert should be blocking or not.
    [kind] is the annotation kind of [p].
    [kf] is the current kernel_function.
    [env] is the current environment. *)
