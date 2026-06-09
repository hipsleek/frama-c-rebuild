(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(* ********************************************************************** *)
(* Helper functions to build statements *)
(* ********************************************************************** *)

val stmt: stmtkind -> stmt
(** Create a statement from a statement kind. *)

val block: stmt -> block -> stmt
(** Create a block statement from a block to replace a given statement.
    Requires that (1) the block is not empty, or (2) the statement is a skip. *)

val block_stmt: block -> stmt
(** Create a block statement from a block *)

val block_from_stmts: stmt list -> stmt
(** Create a block statement from a statement list. *)

val assigns: loc:location -> result:lval -> exp -> stmt
(** [assigns ~loc ~result value] creates a statement to assign the [value]
    expression to the [result] lval. *)

val assigns_field: loc:location -> varinfo -> string -> exp -> stmt
(** [assigns_field ~loc vi field value] creates a statement to assign the
    [value] expression to the [field] of the structure in the variable [vi]. *)

val if_stmt:
  loc:location -> cond:exp -> ?else_blk:block -> block -> stmt
(** [if ~loc ~cond ~then_blk ~else_blk] creates an if statement with [cond]
    as condition and [then_blk] and [else_blk] as respectively "then" block and
    "else" block. *)

val break: loc:location -> stmt
(** Create a break statement *)

val struct_local_init: loc:location -> varinfo -> (string * exp) list -> stmt
(** [struct_local_init ~loc vi fields] creates a local initialization for the
    structure variable [vi]. [fields] is a list of couple [(name, e)] where
    [name] is the name of a field in the structure and [e] is the expression to
    initialize that field. *)

(* ********************************************************************** *)
(* E-ACSL specific code: build calls to its RTL API *)
(* ********************************************************************** *)

val call: loc:location -> ?result:lval -> string -> exp list -> stmt
(** Construct a call to a function with the given name.
    @raise Not_found if the given string does not represent a function in the
    AST, for instance if the function does not exist. *)

val rtl_call:
  loc:location -> ?result:lval -> ?prefix:string -> string -> exp list -> stmt
(** Construct a call to a library function with the given name.

    [prefix] defaults to the E-ACSL RTL API prefix and can be explicitly
    provided to call functions without this prefix.

    @raise Rtl.Symbols.Unregistered if the given string does not represent
    such a function or if library functions were never registered (only possible
    when using E-ACSL through its API). *)

val store_stmt: ?str_size:exp -> varinfo -> stmt
(** Construct a call to [__e_acsl_store_block] that observes the allocation of
    the given varinfo. See [share/e-acsl/e_acsl.h] for details about this
    function. *)

val duplicate_store_stmt: ?str_size:exp -> varinfo -> stmt
(** Same as [store_stmt] for [__e_acsl_duplicate_store_block] that first
    checks for a previous allocation of the given varinfo. *)

val delete_stmt: ?is_addr:bool -> varinfo -> stmt
(** Same as [store_stmt] for [__e_acsl_delete_block] that observes the
    de-allocation of the given varinfo.
    If [is_addr] is false (default), take the address of varinfo. *)

val full_init_stmt: varinfo -> stmt
(** Same as [store_stmt] for [__e_acsl_full_init] that observes the
    initialization of the given varinfo. The varinfo is the address to fully
    initialize, no [addrOf] is taken. *)

val initialize: loc:location -> lval -> stmt
(** Same as [store_stmt] for [__e_acsl_initialize] that observes the
    initialization of the given left-value. *)

val mark_readonly : loc:location -> exp -> stmt
(** Same as [store_stmt] for [__e_acsl_markreadonly] that observes the
    read-onlyness of the given expression. *)

val set_unsound_verdict : loc:location -> stmt
(** @return a statement that indicates to the user that from here on all
    verdicts are unsound. *)
