(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eval

type control_point =
  | Initial
  | Start of Cil_types.kernel_function
  | Before of Cil_types.stmt
  | After of Cil_types.stmt

module ControlPoint : Datatype.S_with_collections with type t = control_point

module type InputDomain = sig
  include Datatype.S

  (** The domain name, used to enable it via -eva-domains. *)
  val name: string

  (** The state representing all possible concrete states. *)
  val top: t
end

(** Automatic storage of the states computed during the analysis. *)
module type S = sig
  type t

  (** Registers the state computed at a control point:
      - for the given [callstack] if provided;
      - for any callstack otherwise. *)
  val set_state: ?callstack:Callstack.t -> control_point -> t -> unit

  (** Returns:
      - [`Top] if no analysis has started or if states are not stored.
      - or the state set by the last call to [set_state] with the same arguments.
      - or [`Bottom] if no such call has been made. *)
  val get_state: ?callstack:Callstack.t -> control_point -> t or_top_bottom

  (** Returns all callstacks from previous calls to [set_state] for the
      given control point. *)
  val callstacks: control_point -> Callstack.t list or_top

  (** Are states of this domain saved? *)
  val is_computed: unit -> bool
end

module Make (Domain : InputDomain) : S with type t := Domain.t
