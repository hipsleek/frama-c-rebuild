(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Interval inference for terms.

    Compute the smallest interval that contains all the possible values of a
    given integer term. The interval of C variables is directly inferred from
    their C type. The interval of logic variables must be registered from
    outside before computing the interval of a term containing such variables
    (see module {!Interval.Env}).

    It implements Figure 3 of J. Signoles' JFLA'15 paper "Rester statique pour
    devenir plus rapide, plus précis et plus mince".
    Also implements a partial support for real numbers.

    Example: consider a variable [x] of type [int] on a (strange) architecture
    in which values of type [int] belongs to the interval \[-128;127\] and a
    logic variable [y] which was registered in the environment with an interval
    \[-32;31\]. Then here are the intervals computed from the term
    [1+(x+1)/(y-64)]:
    1. x in \[128;127\];
    2. x+1 in \[129;128\];
    3. y in \[-32;31\];
    4. y-64 in \[-96;-33\];
    5. (x+1)/(y-64) in \[-3;3\];
    6. 1+(x+1)/(y-64) in \[-2;4\]

    Note: this is a partial wrapper on top of [Ival.t], to which most
    functions are delegated. *)

open Cil_types
open Analyses_types
open Analyses_datatype

(* ************************************************************************** *)
(** {3 Useful operations on intervals} *)
(* ************************************************************************** *)
type t = ival

val ty_of_interv: ?ctx:number_ty -> ?use_gmp_opt:bool -> t -> number_ty

val is_included_in_typ: ival -> typ -> bool

(* ************************************************************************** *)
(** {3 Inference system} *)
(* ************************************************************************** *)
(* The inference phase infers the smallest possible integer interval which the
   values of the term can fit in. *)

val get_from_profile: profile:Profile.t -> term -> t
(** @return the value computed by the interval inference phase
    @raise Is_a_real if the term is either a float or a real.
    @raise Not_a_number if the term does not represent any
    number.*)

val get: logic_env:Logic_env.t -> term -> t
(** @return the value computed by the interval inference phase, same as
      [get_from_profile] but with a full-fledged logic environment instead of a
      function profile *)

val joins_from_profile: profile:Profile.t -> term list -> t
val joins: logic_env:Logic_env.t -> term list -> t
val join_plus_one: profile:Profile.t -> term -> term -> t

val get_ival: logic_env:Logic_env.t -> term -> Ival.t option


(*****************************************************************************)
(** {2 Interval processing} *)
(*****************************************************************************)

val infer_program : file -> unit
(** compute and store the type of all the terms that will be translated
    in a program *)

val preprocess_predicate :
  logic_env:Logic_env.t -> predicate -> unit
(** compute and store the type of all the terms in a code annotation *)

val preprocess_code_annot :
  logic_env:Logic_env.t -> code_annotation -> unit
(** compute and store the type of all the terms in a code annotation *)

val preprocess_term :
  logic_env:Logic_env.t -> term -> unit

val clear : unit -> unit
