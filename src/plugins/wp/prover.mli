(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Provers configuration and information *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(** {2 Prover} *)
(* -------------------------------------------------------------------------- *)

type t =
  | Why3 of Why3Provers.t (** Prover via Why3 *)
  | Qed                   (** Qed Solver *)
  | Tactical              (** Interactive Prover *)
  | CFG                   (** Used for properties proved only using CFG.
                              It cannot be disabled/manually enabled. *)

module Pset : Set.S with type elt = t
module Pmap : Map.S with type key = t

val equal : t -> t -> bool
val compare : t -> t -> int
val pretty : Format.formatter -> t -> unit

val ident : t -> string
(** Identifier of the Prover for WP, typically "CVC5:1.2.1" *)

val name : t -> string
(** Name of the prover, typically CVC5 *)

val shortcut : t -> string
(** Shortcut name (typically lowercase name) *)

val version : t -> string
(** Frama-C version for TIP and Qed *)

val title : ?version:bool -> t -> string

val parse : string -> t option

val is_auto : t -> bool
val is_tactical : t -> bool
val is_extern : t -> bool
val has_counter_examples : t -> bool

val filename_for : t -> string
val of_name : ?fallback:bool -> string -> t option

(* -------------------------------------------------------------------------- *)
(** {2 Prover list} *)
(* -------------------------------------------------------------------------- *)

val provers : ?filter :(t -> bool) -> unit -> t list
(** Returns *all* provers that satisfy [filter] (which defaults _ -> true)
    E.g. if you need only enabled solvers, it should be called with the
    [enabled] function.

    @since 33.0-Arsenic
*)

val add_reload_hook : (unit -> unit) -> unit

val enabled : t -> bool
val set_prover : t -> state:bool -> unit
val add_prover_update_hook : (t -> unit) -> unit

val use_scripts : unit -> bool
val use_strategies : unit -> bool

val set_use_scripts : bool -> unit
(** Note: if false, also disables strategies *)

val set_use_strategies : bool -> unit
(** Note: if true, also enables scripts *)

val add_scripts_update_hook : (unit -> unit) -> unit

(* -------------------------------------------------------------------------- *)
(** {2 Interactive provers configuration} *)
(* -------------------------------------------------------------------------- *)

module InteractiveMode : sig
  type t =
    | Batch     (** Only check scripts *)
    | Update    (** Check and update scripts *)
    | Edit      (** Edit then check scripts *)
    | Fix       (** Try check script, then edit script on non-success *)
    | FixUpdate (** Update & Fix *)

  val title : t -> string
  val parse : string -> t
  val pretty : Format.formatter -> t -> unit

  val get : unit -> t
  val set : t -> unit

  val add_hook_on_update : (unit -> unit) -> unit
end

(* -------------------------------------------------------------------------- *)
(** {2 TIP configuration} *)
(* -------------------------------------------------------------------------- *)

module TipMode : sig
  type t =
    | Batch
    | Update
    | Dry
    | Init

  val is_scratch: unit -> bool
  val is_saving: unit -> bool

  val get : unit -> t
  val set : t -> unit

  val add_hook_on_update : (unit -> unit) -> unit
end

val dkey_shell : Wp_parameters.category
