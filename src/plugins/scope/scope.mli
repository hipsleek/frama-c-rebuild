(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil_datatype

(** Scope analysis. *)

(** Interface for the Scope plugin. *)
module Defs : sig
  val get_defs :
    Kernel_function.t -> stmt -> lval ->
    (Stmt.Hptset.t * Memory_zone.t option) option
  (** @return the set of statements that define [lval] before [stmt] in [kf].
      Also returns the zone that is possibly not defined.
      Can return [None] when the information is not available (Pdg missing). *)

  val get_defs_with_type :
    Kernel_function.t -> stmt -> lval ->
    ((bool * bool) Stmt.Map.t * Memory_zone.t option) option
  (** @return a map from the statements that define [lval] before [stmt] in
          [kf]. The first boolean indicates the possibility of a direct
          modification at this statement, ie. [lval = ...] or [lval = f()].
          The second boolean indicates a possible indirect modification through
          a call.
          Also returns the zone that is possibly not defined.
          Can return [None] when the information is not available (Pdg missing).
  *)

  val compute_with_def_type_zone:
    Cil_types.kernel_function -> Cil_types.stmt -> Memory_zone.t ->
    ((bool * bool) Cil_datatype.Stmt.Map.t * Memory_zone.t option) option
    (** internal use *)
end

module Datascope : sig
  val get_data_scope_at_stmt :
    Kernel_function.t -> stmt -> lval ->
    Stmt.Hptset.t * (Stmt.Hptset.t * Stmt.Hptset.t)
  (**  @raise Kernel_function.No_Definition if [kf] has no definition.
       @return 3 statement sets related to the value of [lval] before [stmt] :
       - the forward selection,
       - the both way selection,
       - the backward selection. *)

  val get_prop_scope_at_stmt :
    kernel_function -> stmt -> code_annotation ->
    Stmt.Hptset.t * code_annotation list
  (** compute the set of statements where the given annotation has the same
      value as before the given stmt. Also returns the eventual code annotations
      that are implied by the one given as argument. *)

  val check_asserts : unit -> code_annotation list
  (** Print how many assertions could be removed based on the previous
      analysis ([get_prop_scope_at_stmt]) and return the annotations
      that can be removed. *)

  val rm_asserts : unit -> unit
  (** Same analysis than [check_asserts] but mark the assertions as proven. *)

  (** for internal use *)
  module R: Plugin.General_services
end

(** {3 Zones} *)

module Zones : sig
  type t_zones = Memory_zone.t Stmt.Hashtbl.t
  val build_zones :
    kernel_function -> stmt -> lval -> Stmt.Hptset.t * t_zones
  val pretty_zones : Format.formatter -> t_zones -> unit
  val get_zones : t_zones ->  Cil_types.stmt -> Memory_zone.t
end
