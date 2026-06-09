(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang.F

(** Term manipulation for Tacticals *)

val s_bool:term -> term list

val s_cnf_ite: term -> term -> term -> term list
val s_dnf_ite: term -> term -> term -> term list
val s_cnf_iff: term -> term -> term list
val s_dnf_iff: term -> term -> term list
val s_cnf_xor: term -> term -> term list
val s_dnf_xor: term -> term -> term list

(* Is the term into a Conjunctive Normal Form *)
val is_cnf: term -> bool

(* returns the Conjunctive Normal Form of a term *)
val e_cnf: ?depth:int -> term -> term

(*Is the term into a Conjunctive Normal Form *)
val is_dnf: term -> bool

(* returns the Disjunctive Normal Form of a term *)
val e_dnf: ?depth:int -> term -> term
