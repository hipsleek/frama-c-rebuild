(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types


module Types : sig

  (** Pointers are the address of a variable, with a potential offset,
      and are used to refer in a simple way to an address in memory *)
  type pointer = Cil_types.varinfo * int

  module Pointer: Datatype.S_with_collections with type t = pointer

  type state = Cvalue.Model.t
  type value = Cvalue.V.t
  type zone = Memory_zone.t
  type slice = Cvalue.V_Offsetmap.t

  type functions_states = state Cil_datatype.Stmt.Hashtbl.t
  type map_functions_states = state Cil_datatype.Stmt.Map.t

  type state_accesser =
    | Global
    | Local of functions_states

  val map_functions_states_to_get_state: map_functions_states -> (stmt -> state)

  val iter_requests:
    state_accesser -> stmt -> (Results.request -> unit) -> unit

  val merge_map_non_map_functions_states:
    map_functions_states -> functions_states -> map_functions_states
  val merge_map_functions_states:
    map_functions_states -> map_functions_states -> map_functions_states

end
open Types


(** {1 Union of state, values and list of values} *)

(**. We also return a boolean indicating whether an update has taken
   place, ie. if the result of the union is different (thus greater)
   from the first argument. Notice that this means that those
   functions are not symmetrical! *)
val join_state : state -> state -> state * bool
val join_value : value -> value -> value * bool

val join_params : value list -> value list -> value list * bool

(** Remove all the values that are not global variables from the state *)
val clear_non_globals : state -> state


(** {1 Reading and writing in memory} *)

(** [read_slice ~p ~sbytes st] reads [sbytes] starting
    from [p] in [state]. *)
val read_slice: p:value -> sbytes:int -> state -> slice

(** Return the value pointed by the given int pointer *)
val read_int_pointer: pointer -> state -> value


(** [write_int_pointer p v state] write the int [v] at the location
    pointed [p] in state [state]. *)
val write_int_pointer : pointer -> int -> state -> state

(** [replace_value_at_int_pointer p ~before ~after state] replaces [before]
    by [after] in the abstract value bound at location [p] in [state]. *)
val replace_value_at_int_pointer:
  pointer -> before:int -> after:int -> state -> state

(** [write_at_pointer ~p ~sbytes ~slice st] alters [state] by
    writing at the [sbytes] bytes starting at [p] the slice [v]. *)
val write_slice:
  p:pointer -> sbytes:int -> slice:slice -> exact:bool -> state -> state


(** {1 Conversion to and from Mthread world to the value analysis} *)

(*** All conversion functions below return an error message in case of failure. *)
type 'a conversion = ('a, string) Result.t

val extract_fun : value -> kernel_function conversion
val extract_pointer : value -> pointer conversion
val extract_int : value -> int conversion
val extract_int_possibly_zero : value -> (int * [`Exact | `WithZero]) conversion
val extract_constant_string : value -> string conversion

(** Fails if [value] represents more than [cardinal] integers. *)
val extract_int_list : cardinal:int -> value -> int list conversion

val int_to_value: int -> value

val pretty_slice: slice Pretty_utils.formatter
