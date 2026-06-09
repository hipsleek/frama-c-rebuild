(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Calls to the GMP's API. *)

open Cil_types

val init: loc:location -> exp -> stmt
(** build stmt [mpz_init(v)] or [mpq_init(v)] depending on typ of [v] *)

val init_set: loc:location -> lval -> exp -> exp -> stmt
(** [init_set x_as_lv x_as_exp e] builds stmt [x = e] or [mpz_init_set*(v, e)]
    or [mpq_init_set*(v, e)] with the good function 'set'
    according to the type of [e] *)

val clear: location -> exp -> stmt
(** build stmt [mpz_clear(v)] or [mpq_clear(v)] depending on typ of [v] *)

val assign: loc:location -> lval -> exp -> exp -> stmt
(** [assign x_as_lv x_as_exp e] builds stmt [x = e] or [mpz_set*(e)]
    or [mpq_set*(e)] with the good function 'set'
    according to the type of [e] *)

module Z : sig

  val name_arith_bop: binop -> string
  (** [name_of_mpz_arith_bop bop] returns the name of the GMP integer function
      corresponding to the [bop] arithmetic operation. *)

  val new_var:
    loc:location -> ?scope:Varname.scope -> ?name:string ->
    Env.t -> kernel_function -> term option ->
    (varinfo -> exp (* the var as exp *) -> stmt list) ->
    exp * Env.t
  (** Same as [Env.new_var], but dedicated to mpz_t variables initialized by
      {!Mpz.init}. *)

  val create:
    loc:location -> ?name:string -> term option ->  Env.t -> kernel_function ->
    exp -> exp * Env.t
  (** Create an integer number. *)

  val add_cast:
    loc:location -> ?name:string -> Env.t -> kernel_function -> typ -> exp ->
    exp * Env.t
  (** Assumes that the given exp is of integer type and casts it into
      the given typ *)

  val binop:
    loc:location -> term option -> binop -> Env.t -> kernel_function ->
    exp -> exp -> exp * Env.t
  (** Applies [binop] to the given expressions. The optional term
      indicates whether the comparison has a correspondence in the logic. *)

  val cmp:
    loc:location -> string -> term option -> binop ->  Env.t ->
    kernel_function -> exp -> exp -> exp * Env.t
    (** Compares two expressions according to the given [binop]. The optional
        term indicates whether the comparison has a correspondence in the
        logic. *)

end

module Q : sig

  val name_arith_bop: binop -> string
  (** [name_of_mpz_arith_bop bop] returns the name of the GMP rational function
      corresponding to the [bop] arithmetic operation. *)

  val normalize_str: string -> string
  (** Normalize the string so that it fits the representation used by the
      underlying real library. For example, "0.1" is a real number in ACSL
      whereas it is considered as a double by [libgmp] because it is written in
      decimal expansion. In order to make [libgmp] consider it to be a rational,
      it must be converted into "1/10". *)

  val create:
    loc:location -> ?name:string -> term option ->  Env.t -> kernel_function ->
    exp -> exp * Env.t
  (** Create a rational number. *)

  val cast_to_z: loc:location -> ?name:string -> Env.t -> exp -> exp * Env.t
  (** Assumes that the given exp is of real type and casts it into Z *)

  val add_cast:
    loc:location -> ?name:string -> Env.t -> kernel_function -> typ -> exp ->
    exp * Env.t
  (** Assumes that the given exp is of real type and casts it into
      the given typ *)

  val binop:
    loc:location -> term option -> binop -> Env.t -> kernel_function ->
    exp -> exp -> exp * Env.t
  (** Applies [binop] to the given expressions. The optional term
      indicates whether the comparison has a correspondence in the logic. *)

  val cmp:
    loc:location -> string -> term option -> binop ->  Env.t ->
    kernel_function -> exp -> exp -> exp * Env.t
    (** Compares two expressions according to the given [binop]. The optional
        term indicates whether the comparison has a correspondence in the
        logic. *)

end
