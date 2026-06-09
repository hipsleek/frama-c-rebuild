(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Auxiliary functions to mark invalid (more precisely 'escaping') the
    references to a variable whose scope ends. *)

(** Set of bases that might contain a reference to a local or formal
    variable. Those references must be marked as dangling once we leave
    the scope of those local or formals. *)
type clobbered_set = {
  mutable clob: Base.SetLattice.t
}

val packed_descr: Structural_descr.pack

val bottom: unit -> clobbered_set
val top: unit -> clobbered_set

val remember_bases_with_locals: clobbered_set -> Base.SetLattice.t -> unit
(** Add the given set of bases to an existing clobbered set *)

val remember_if_locals_in_value:
  clobbered_set -> Locations.t -> Cvalue.V.t -> unit
(** [remember_locals_in_value clob loc v] adds all bases pointed to by [loc]
    to [clob] if [v] contains the address of a local or formal *)

val offsetmap_contains_local: Cvalue.V_Offsetmap.t -> bool


val make_escaping:
  exact:bool ->
  escaping:Base.Hptset.t ->
  on_escaping:(b:Base.t -> itv:Z.t * Z.t -> v:Cvalue.V.t -> unit) ->
  within:Base.SetLattice.t ->
  Cvalue.Model.t -> Cvalue.Model.t
(** [make_escaping ~exact ~escaping ~on_escaping ~within state] changes all
    references to the variables in [escaping] to "escaping address".
    All such references must be in the offsetmaps bound to [within].
    [on_escaping b itv v] is called when a reference is found: [v] is the value
    that refers to [escaping], [b] is the base in which [v] appears
    (included in [within]) and [itv] is the offset at which [v] appears.
    If [exact] holds, a strong update is performed. Otherwise, only
    a week update is executed. *)

val make_escaping_fundec:
  Cil_types.fundec -> clobbered_set -> Cil_types.varinfo list ->
  Cvalue.Model.t -> Cvalue.Model.t
(** [make_escaping_fundec fdec clob l state] changes all references to the
    local or formal variables in [l] to "escaping". All pointers to [l] should
    be in the offsetmap bound to the variables contained in [clob].
    [fdec] is used to detect whether we are deallocating the outer scope of a
    function, in which case a different warning is emitted. *)

val substitute:
  Base.substitution -> clobbered_set -> Cvalue.Model.t -> Cvalue.Model.t
(** [substitute substitution clob state] applies [substitution] to all pointer
    values in the offsetmaps bound to variables in [clob] in [state]. *)
