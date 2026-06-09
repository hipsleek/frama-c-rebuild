(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Letifications                                                      --- *)
(* -------------------------------------------------------------------------- *)

open Lang.F

module Ground :
sig

  type env
  val top : unit -> env
  val copy : env -> env

  val e_apply : env -> term -> term
  val p_apply : env -> pred -> pred

  val forward : env -> pred -> pred
  val backward : env -> pred -> pred
  val branch : env -> pred -> pred * env * env

end

module Subst :
sig
  type t
  val equal : t -> t -> bool
  val pretty : string -> Format.formatter -> t -> unit
  val e_apply : t -> term -> term
  val p_apply : t -> pred -> pred
  val empty : t
  val add : var -> term -> t -> t
  val assume : t -> pred -> t
  val find : var -> t -> term
  val iter : (var -> term -> unit) -> t -> unit
  val domain : t -> Vars.t
  val codomain : t -> Vars.t
end

module Defs :
sig
  type t
  val empty : t
  val merge : t -> t -> t
  val extract : pred -> t
  val add : t ref -> pred -> unit
  val domain : t -> Vars.t
end

val bind : Subst.t -> Defs.t -> Vars.t -> Subst.t
(** [bind sigma defs xs] select definitions in [defs]
    targeting variables [xs]. The result is a new substitution that
    potentially augment [sigma] with definitions for [xs] (and others). *)

val add_definitions : Subst.t -> Defs.t -> Vars.t -> pred list -> pred list
(** [add_definitions sigma defs xs ps] keep all
    definitions of variables [xs] from [sigma] that comes from [defs].
    They are added to [ps]. *)

(** Pruning strategy ; selects most occurring literals to split cases. *)
module Split :
sig

  type occur

  val create : unit -> occur
  val add : occur -> pred -> unit
  val select : occur -> (pred * int) list

end
