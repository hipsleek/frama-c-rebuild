(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** State of the slicing.
    @since Phosphorus-20170501-beta1 *)

val get: unit -> SlicingTypes.sl_project
(** Get the state of the slicing project.
    Assume it has already been initialized through {!Slicing.Api.reset_slicing}.
*)

val may: (unit -> unit) -> unit
(** apply the given closure if the slicing project has been initialized through
    {!Slicing.Api.reset_slicing}. *)

val may_map: none:'a -> (unit -> 'a) -> 'a
(** apply the given closure if the slicing project has been initialized through
    {!Slicing.Api.reset_slicing}, or else return the default value.*)

val self: State.t
(** Internal state of the slicing tool from project viewpoints.
    @since Sulfur-20171101 *)

val reset_slicing: unit -> unit
(** Function that can be used for:
    - initializing the slicing tool before starting a slicing project;
    - removing all computed slices and all internal pending requests
      of the current slicing project. *)
