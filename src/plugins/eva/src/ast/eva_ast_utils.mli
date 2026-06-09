(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Utility functions on the Eva AST of lvalues and expressions. *)

open Eva_ast_types

(** Conversion to Cil *)

val to_cil_exp : exp -> Cil_types.exp
val to_cil_lval : lval -> Cil_types.lval


(** Queries *)

(** Cf {!Cil.is_mutable_or_initialized}. *)
val is_mutable : lval -> bool
val is_initialized : lval -> bool


(** Expressions/Lvalue heights *)

(** Computes the height of an expression, that is the maximum number of nested
    operations in this expression. *)
val height_exp : exp -> int

(** Computes the height of an lvalue. *)
val height_lval : lval -> int


(** Specialized visitors *)

(** [exp_contains_volatile e] (resp. [lval_contains_volatile lv] is true
    whenever one l-value contained inside the expression [e] (resp. the lvalue
    [lv]) has volatile qualifier. Relational analyses should not learn
    anything on such values. *)
val exp_contains_volatile : exp -> bool
val lval_contains_volatile : lval -> bool

(** Returns the set of variables that syntactically appear in an expression or
    lvalue. *)
val vars_in_exp : exp -> Cil_datatype.Varinfo.Set.t
val vars_in_lval : lval -> Cil_datatype.Varinfo.Set.t


(** Constant conversion and folding. *)

val const_fold: exp -> exp
val fold_to_integer: exp -> Z.t option

val is_zero_ptr : exp -> bool

(** Offsets *)

(** Returns the last offset in the chain. *)
val last_offset: offset -> offset

(** Is an lvalue a bitfield? *)
val is_bitfield: lval -> bool
