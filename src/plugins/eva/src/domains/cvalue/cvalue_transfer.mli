(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Transfer functions for the main domain of the Value analysis. *)

type value = Main_values.CVal.t
type origin = value
type location = Main_locations.PLoc.location

include Abstract_domain.Transfer
  with type state := Cvalue.Model.t
   and type value := value
   and type location := location
   and type origin := origin

(** [warn_imprecise_write lval loc v] emits a warning about the assignment of
    value [v] into location [loc] if one of them is overly imprecise. [lval] is
    the assigned lvalue, and [prefix] is an optional prefix to the warning. *)
val warn_imprecise_write:
  ?prefix:string -> Eva_ast.lval -> Locations.t -> Cvalue.V.t -> unit

(** [warn_imprecise_offsm_write lval offsm] emits a warning about the assignment
    of offsetmap [offsm] if it contains an overly imprecise value. [lval] is the
    assigned lvalue, and [prefix] is an optional prefix to the warning. *)
val warn_imprecise_offsm_write:
  ?prefix:string -> Eva_ast.lval -> Cvalue.V_Offsetmap.t -> unit
