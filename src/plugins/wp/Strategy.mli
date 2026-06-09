(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** {2 Term & Predicate Selection} *)

open Lang.F
open Conditions
open Tactical

val occurs_x : var -> term -> bool
val occurs_y : var -> pred -> bool
val occurs_e : term -> term -> bool
val occurs_p : term -> pred -> bool
val occurs_q : pred -> pred -> bool

(** Lookup the first occurrence of term in the sequent and returns
    the associated selection. Returns [Empty] is not found.
    Goal is lookup first. *)
val select_e : sequent -> term -> selection

(** Same as [select_e] but for a predicate. *)
val select_p : sequent -> pred -> selection

(** {2 Strategy} *)

type argument = ARG: 'a field * 'a -> argument

type strategy = {
  priority : float ;
  tactical : tactical ;
  selection : selection ;
  arguments : argument list ;
}

class pool :
  object
    method add : strategy -> unit
    method sort : strategy array
  end

class type heuristic =
  object
    method id : string
    method title : string
    method descr : string
    method search : (strategy -> unit) -> sequent -> unit
  end

val register : #heuristic -> unit
val export : #heuristic -> heuristic
val lookup : id:string -> heuristic
val iter : (heuristic -> unit) -> unit

(** {2 Factory} *)

type t = strategy
val arg : 'a field -> 'a -> argument
val make : tactical ->
  ?priority:float -> ?arguments:argument list -> selection -> strategy

(**/**)

(* To be used only when applying the tactical *)

val set_arg : tactical -> argument -> unit
val set_args : tactical -> argument list -> unit
