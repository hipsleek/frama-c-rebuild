(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Type system which computes the smallest C type that may contain all the
    possible values of a given integer term or predicate. Also compute the
    required casts. It is based on interval inference of module {!Interval}.

    It implement Figure 4 of J. Signoles' JFLA'15 paper "Rester statique pour
    devenir plus rapide, plus précis et plus mince".

    Example: consider a variable [x] of type [int] and a variable [y] of type
    char on a (strange) architecture in which values of type [int] belongs to
    the interval [[-128;127]] and values of type [char] belongs to the interval
    [[-32;31]], while there are no other integral types. Then here are some
    information computed from the term [1+(x+1)/(y-64)] by the type system:
    1. [x+1] must be a GMP (because of the potential overflow)
    2. consequently [x], which is an [int], must be coerced into a GMP and the
    same for the number 1 in this addition.
    3. [y-64] can be computed in an [int] (because the result belongs to the
    interval [[-96;-33]]).
    4. [(x+1)/(y-64)] must be a GMP operation because the numerator is a
    GMP (see 1.). Consequently [y-64] must be coerced into a GMP too. However,
    the result belongs to the interval [[-3;3]] and thus can be safely coerced
    to an [int].
    5. Consequently the addition of the toplevel term [1+(x+1)/(y-64)] can
    safely be computed in [int]: its result belongs to [[-2;4]]. *)

open Cil_types
open Analyses_types
open Analyses_datatype

(******************************************************************************)
(** {2 Datatypes} *)
(******************************************************************************)

(** {3 Smart constructors} *)

val c_int: number_ty
val ikind: ikind -> number_ty
val fkind: fkind -> number_ty
val gmpz: number_ty
val rational: number_ty
val nan: number_ty

(** {3 Useful operations over [number_ty]} *)

exception Not_a_number
val typ_of_number_ty: number_ty -> typ
(** @return the C type corresponding to an {!number_ty}. That is [Gmp.z_t ()]
    for [Gmpz], [Real.t ()] for [Real] and [TInt(ik, [[]])] for [Ctype ik].
    @raise Not_a_number in case of [Nan]. *)

val number_ty_of_typ: post:bool -> typ -> number_ty
(** Reverse of [typ_of_number_ty]
    [number_ty_of_typ ~post ty] return the {!number_ty} corresponding to a
    C type. [post] indicates if the type is before or after the typing phase.
    The GMP types will be recognized only in a post-typing phase.
*)

val join: number_ty -> number_ty -> number_ty
(** {!number_ty} is a join-semi-lattice if you do not consider [Other]. If
    there is no [Other] in argument, this function computes the join of this
    semi-lattice. If one of the argument is {!Other}, the function assumes that
    the other argument is also {!Other}. In this case, the result is [Other]. *)

val number_ty_bound_variable:
  profile:Profile.t -> term * logic_var * term -> number_ty
(** return the type of a quantified logic variable *)

(******************************************************************************)
(** {2 Typing} *)
(******************************************************************************)
val clear: unit -> unit
(** Remove all the previously computed types. *)

(** {3 Getters}

    Below, the functions assume that either {!type_term} or
    {!type_named_predicate} has been previously computed for the given term or
    predicate. *)

val get_number_ty: logic_env:Logic_env.t -> term -> number_ty
(** @return the inferred type for the given term. *)

val get_effective_ty: logic_env:Logic_env.t -> term -> number_ty
(** @return the necessary cast inferred by the type system if any, or the type
    inferred for the given term otherwise *)

val get_typ: logic_env:Logic_env.t -> term -> typ
(** Get the type which the given term must be generated to. *)

val get_effective_typ: logic_env:Logic_env.t -> term -> typ
(** Get the type which the given term must be converted to if any, and the
    translation type otherwise *)

val get_cast: logic_env:Logic_env.t -> term -> typ option
(** Get the type which the given term must be converted to (if any). *)

val unsafe_set:
  term ->
  ?ctx:number_ty ->
  logic_env:Logic_env.t ->
  number_ty ->
  unit
(** Register that the given term has the given type in the given context (if
    any). No verification is done. *)

(*****************************************************************************)
(** {2 Typing/types-related utils} *)
(*****************************************************************************)

val typ_of_lty: logic_type -> typ
(** @return the C type that corresponds to the given logic type. *)

(*****************************************************************************)
(** {2 Typing processing} *)
(*****************************************************************************)

val type_program : file -> unit
(** compute and store the type of all the terms that will be translated
    in a program *)

val preprocess_predicate :
  logic_env:Logic_env.t ->
  predicate ->
  unit
(** compute and store the types of all the terms in a given predicate  *)

val preprocess_rte :
  logic_env:Logic_env.t ->
  code_annotation ->
  unit
(** compute and store the type of all the terms in a code annotation *)

val preprocess_term:
  use_gmp_opt:bool ->
  ?ctx:number_ty ->
  logic_env:Logic_env.t ->
  term ->
  unit
(** Compute the type of each subterm of the given term in the given context. If
    [use_gmp_opt] is false, then the conversion to the given context is done
    even if -e-acsl-gmp-only is set. *)
