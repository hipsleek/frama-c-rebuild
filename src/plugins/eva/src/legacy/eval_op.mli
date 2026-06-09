(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Numeric evaluation. Factored with evaluation in the logic. *)

open Cil_types
open Cvalue

(** Transformation a value into an offsetmap of size [sizeof(typ)] bytes. *)
val offsetmap_of_v: typ:Cil_types.typ -> V.t -> V_Offsetmap.t

(** Returns the offsetmap at a precise_location from a state. *)
val offsetmap_of_loc:
  Precise_locs.precise_location -> Model.t -> V_Offsetmap.t Eval.or_bottom

val backward_comp_left_from_type:
  logic_type ->
  (bool -> Abstract_interp.Comp.t -> Cvalue.V.t -> Cvalue.V.t -> Cvalue.V.t)
(** Reduction of a {!Cvalue.V.t} by [==], [!=], [>=], [>], [<=] and [<].
    [backward_comp_left_from_type positive op l r] reduces [l]
    so that the relation [l op r] holds. [typ] is the type of [l]. *)

val reduce_by_initialized_defined :
  (V_Or_Uninitialized.t -> V_Or_Uninitialized.t) ->
  Locations.t -> Model.t -> Model.t

val apply_on_all_locs:
  (Locations.t -> 'a -> 'a) -> Locations.t -> 'a -> 'a
(** [apply_all_locs f loc state] folds [f] on all the atomic locations
    in [loc], provided there are less than [plevel]. Useful mainly
    when [loc] is exact or an over-approximation. *)

val reduce_by_valid_loc:
  positive:bool -> Locations.access -> Locations.t -> typ -> Model.t -> Model.t
(* [reduce_by_valid_loc positive ~for_writing loc typ state] reduces
   [state] so that [loc] contains a pointer [p] such that [(typ* )p] is
   valid if [positive] holds (or invalid otherwise). *)

val make_loc_contiguous: Locations.t -> Locations.t
(** 'Simplify' the location if it represents a contiguous zone: instead
    of multiple offsets with a small size, change it into a single offset
    with a size that covers the entire range. *)

val pretty_stitched_offsetmap: Format.formatter -> typ -> V_Offsetmap.t -> unit
val pretty_offsetmap: typ -> Format.formatter -> V_Offsetmap.t -> unit

(* Given an under-approximation of a location, finds an under-approximation
   of the value at this location in the given state.
   Returns None if no under-approximation can be computed. *)
val find_under_approximation:
  Cvalue.Model.t -> Locations.t -> Cvalue.V_Or_Uninitialized.t option
