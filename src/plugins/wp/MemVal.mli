(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Lang.F

module type State =
sig
  (** abstract state **)
  type t

  val bottom : t
  val join : t -> t -> t

  val of_kinstr : Cil_types.kinstr -> t
  (** [of_stmt stmt] get the abstract state of [stmt]. **)

  val of_stmt : Cil_types.stmt -> t
  (** [of_kf kf] get the join state of all [kf]'s statements states **)

  val of_kf : Cil_types.kernel_function -> t

  val pretty : Format.formatter -> t -> unit
end

module type Value =
sig
  val configure : unit -> WpContext.rollback
  val datatype : string

  module State : State

  (** abstract value **)
  type t
  type state = State.t

  val null : t

  (** [cvar x] returns the abstract value corresponding to &[x]. **)
  val cvar : Cil_types.varinfo -> t

  (** [field v fd] returns the value obtained by access to field [fd]
      from [v]. **)
  val field : t -> Cil_types.fieldinfo -> t

  (** [shift v obj k] returns the value obtained by access at an index [k]
      with type [obj] from [v]. **)
  val shift : t -> Ctypes.c_object -> term -> t

  (** [base_addr v] returns the value corresponding to the base address
      of [v]. **)
  val base_addr : t -> t

  (** [load s v obj] returns the value at the location given by [v] with type
      [obj] within the state [s]. **)
  val load : state -> t -> Ctypes.c_object -> t

  (** [domain v] returns a list of all possible concrete bases of [v]. **)
  val domain : t -> Base.t list

  (** [offset v] returns a function which when applied with a term returns
      a predicate over [v]'s offset. *)
  val offset : t -> (term -> pred)

  val pretty : Format.formatter -> t -> unit
end

module Make(_ : Value) : Memory.Model

(** The glue between WP and EVA. **)
module Eva : Value
