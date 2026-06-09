(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Initial abstract state at the beginning of a call. From most precise to
    less precise. *)
type call_init_state =
  | ISCaller (** information from the caller is propagated in the callee. May be
                 more precise, but problematic w.r.t Memexec because it increases
                 cache miss dramatically. *)
  | ISFormals (** empty state, except for the equalities between a formal and
                  the corresponding actual. Lesser impact on Memexec. *)
  | ISEmpty (** completely empty state, without impact on Memexec. *)


type t
val key: t Abstract_domain.key
val project: t -> Equality.Set.t

module type Context = Abstract.Context.External
module type Value = Abstract.Value.External

module Make (Context : Context) (Value : Value with type context = Context.t) :
  Abstract_domain.S with type context = Context.t
                     and type value = Value.t
                     and type location = Precise_locs.precise_location
                     and type state = t

val registered : Abstractions.Domain.registered
