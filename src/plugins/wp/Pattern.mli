(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Logic_ptree

(* -------------------------------------------------------------------------- *)
(* --- Pattern Engine                                                     --- *)
(* -------------------------------------------------------------------------- *)

type context
type pattern
type value

exception TypeError of location * string

val pattern_loc: pattern -> location

(** Creates an empty environment.
    @before 33.0-Arsenic the typing context was mandatory.
*)
val context : ?tc:Logic_typing.typing_context -> unit -> context

(** Raise a typing error related to patterns.
    Either the typing context has been built with a logic typing context and it
    uses it or it raises an exception.
    @raise TypeError when the context does not have a typing_context
    @since 33.0-Arsenic
*)
val error: context -> location -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Parse a pattern and enrich the environment with pattern variables
    @raise TypeError in case of error when context does not have typing_context
    @before 33.0-Arsenic it used to always use the typing_context for errors
*)
val pa_pattern : context -> lexpr -> pattern

(** Parse value according to the environment
    @raise TypeError in case of error when context does not have typing_context
    @before 33.0-Arsenic it used to always use the typing_context for errors
*)
val pa_value : context -> lexpr -> value

(** Return a value that equals the pattern *)
val self : pattern -> pattern * value

(** Force pattern naming, for debugging purposes *)
val named : string -> pattern -> pattern

(** Pattern printer *)
val pp_pattern : Format.formatter -> pattern -> unit

(** Value printer *)
val pp_value : Format.formatter -> value -> unit

(** Matching lookup *)
type lookup = {
  head: bool ;
  goal: bool ;
  hyps: bool ;
  split: bool ;
  pattern: pattern ;
}

(** Matching result *)
type sigma

(** Sigma printer *)
val pp_sigma : Format.formatter -> sigma -> unit

val iter_sigma : (string -> Tactical.selection -> unit) -> sigma -> unit

(** Empty results *)
val empty : sigma

(** Matching sequent *)
val psequent : lookup -> sigma -> Conditions.sequent -> sigma option

(** Composing values from matching results *)
val select : sigma -> value -> Tactical.selection

(** Composing a boolean *)
val bool : value -> bool

(** Composing a string *)
val string : value -> string

(** Typechecking *)

type env

(** [raise] defaults to false *)
val env : ?raise:bool -> unit -> env

(** Raise a typing error related to patterns.
    Either the environment has been built with [raise] set to [false] and it
    logs an error, or it was set to [true] and it raises an exception.
    @raise TypeError when the environment has [raise] set to [true]
    @since 33.0-Arsenic
*)
val typecheck_error : env -> location -> ('a, Format.formatter, unit, unit) format4 -> 'a

val typecheck_value : env -> ?tau:Lang.F.tau -> value -> unit
val typecheck_pattern : env -> ?tau:Lang.F.tau -> pattern -> unit
val typecheck_lookup : env -> lookup -> unit

(* -------------------------------------------------------------------------- *)
