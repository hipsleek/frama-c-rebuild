(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Floating Arithmetic Model *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Lang
open Lang.F

val f32 : adt
val f64 : adt

val t32 : tau
val t64 : tau

val fq32 : lfun
val fq64 : lfun

type model = Real | Float
val configure : model -> WpContext.rollback

val ftau : c_float -> tau
(** model independent *)

val tau_of_float : c_float -> tau
(** with respect to model *)

type op =
  | LT
  | EQ
  | LE
  | NE
  | NEG
  | ADD
  | SUB
  | MUL
  | DIV
  | REAL
  | ROUND
  | EXACT (** same as round, but argument is exact representation *)

val find : lfun -> op * c_float

val code_lit : c_float -> float -> string option -> term
val acsl_lit : Cil_types.logic_real -> term
val float_lit : c_float -> Q.t -> string
(** Returns a string literal in decimal notation (without suffix)
    that reparses to the same value (when added suffix). *)

val float_of_int : c_float -> unop
val float_of_real : c_float -> unop
val real_of_float : c_float -> unop

val fopp : c_float -> unop
val fadd : c_float -> binop
val fsub : c_float -> binop
val fmul : c_float -> binop
val fdiv : c_float -> binop

val flt : c_float -> cmp
val fle : c_float -> cmp
val feq : c_float -> cmp
val fneq : c_float -> cmp

val f_model : c_float -> lfun
val f_delta : c_float -> lfun
val f_epsilon : c_float -> lfun

val flt_of_real : c_float -> lfun
val real_of_flt : c_float -> lfun

val flt_add : c_float -> lfun
val flt_mul : c_float -> lfun
val flt_div : c_float -> lfun
val flt_neg : c_float -> lfun
