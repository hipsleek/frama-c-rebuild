(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Logic expressions *)

module Make
    ( ADT : Logic.Data )
    ( Field : Logic.Field )
    ( Fun : Logic.Function )
  :
  sig
    (** Logic API *)
    include Logic.Term with module ADT = ADT
                        and module Field = Field
                        and module Fun = Fun

    (** Prints term in debug mode. *)
    val debug : Format.formatter -> term -> unit

    (** {2 Global State}
        One given [term] has valid meaning only for one particular state. *)

    type state
    (** Hash-consing, cache, rewriting rules, etc. *)

    val create : unit -> state
    (** Create a new fresh state. Local state is not modified. *)

    val get_state : unit -> state
    (** Return local state. *)

    val set_state : state -> unit
    (** Update local state. *)

    val clr_state : state -> unit
    (** Clear local state. *)

    val in_state : state -> ('a -> 'b) -> 'a -> 'b
    (** execute in a particular state. *)

    val rebuild_in_state : state -> ?cache:term Tmap.t -> term -> term * term Tmap.t
    (** rebuild a term in the given state *)

    (** Register a constant in the global state. *)
    val constant : term -> term

    (** {2 Context Release} *)

    val release : unit -> unit
    (** Clear caches and checks. Global builtins are kept. *)

  end
