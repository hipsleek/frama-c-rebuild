(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module is used to generate missing specifications. Options
    {!Kernel.GeneratedDefaultSpec}, {!Kernel.GeneratedSpecMode} and
    {!Kernel.GeneratedSpecCustom} can be used to choose precisely which clause
    to generate in which case.
    @since 28.0-Nickel
*)

open Cil_types

(** Different types of clauses which can be generated via
    {!populate_funspec}. *)
type clause = [
  | `Exits
  | `Assigns
  | `Requires
  | `Allocates
  | `Terminates
]

(** Represents exits clause in the sense of
    {!Cil_types.behavior.b_post_cond}. *)
type t_exits = (termination_kind * identified_predicate) list

(** Assigns clause *)
type t_assigns = Cil_types.assigns

(** Allocation clause *)
type t_allocates = Cil_types.allocation

(** Represents requires clause in the sense of
    {!Cil_types.behavior.b_requires}. *)
type t_requires = identified_predicate list

(** Represents terminates clause in the sense of
    {!Cil_types.spec.spec_terminates}. *)
type t_terminates = identified_predicate option

(** Type of a function that, given a {!Kernel_function.t} and a
    {!type:Cil_types.spec}, returns a clause. Accepted clause types include
    {!t_exits}, {!t_assigns}, {!t_requires}, {!t_allocates} and {!t_terminates}.
*)
type 'a gen = (kernel_function -> spec -> 'a)

(** Alias for brevity, status emitted for properties. *)
type status = Property_status.emitted_status

(** [register ?gen_exits ?gen_requires ?status_allocates ... name] registers a
    new mode called [name] which can then be used for specification generation
    (see {!Kernel.GeneratedSpecMode} and {!Kernel.GeneratedSpecCustom}). All
    parameters except [name] are optional, meaning default action (mode
    Frama_C) will be performed if left unspecified (triggers a warning).
*)
val register :
  ?gen_exits:t_exits gen -> ?status_exits:status ->
  ?gen_assigns:t_assigns gen -> ?status_assigns:status ->
  ?gen_requires:t_requires gen -> ?gen_allocates:t_allocates gen ->
  ?status_allocates:status -> ?gen_terminates:t_terminates gen ->
  ?status_terminates:status ->
  string -> unit

(** [populate_funspec ~loc ~do_body kf clauses] generates missing specifications
    for [kf] according to selected [clauses].
    [loc] is set to [Kernel_function.get_location kf] by default, and is used
    to specify warnings locations.
    By default [do_body] is false, meaning only specification of prototypes will
    be generated.
*)
val populate_funspec : ?loc:Cil_types.location -> ?do_body:bool ->
  kernel_function -> clause list ->unit
