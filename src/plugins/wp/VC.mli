(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Verification Conditions Interface                                  --- *)
(* -------------------------------------------------------------------------- *)

open VCS

(** {2 Proof Obligations} *)

type t (** elementary proof obligation *)

val get_id : t -> string
val get_model : t -> WpContext.model
val get_scope : t -> WpContext.scope
val get_context : t -> WpContext.context
val get_description : t -> string
val get_property : t -> Property.t
val get_result : t -> Prover.t -> result
val get_results : t -> (Prover.t * result) list
val get_sequent : t -> Conditions.sequent
val get_formula: t -> Lang.F.pred
val is_trivial : t -> bool

(** One prover at least returns a valid verdict. *)
val is_valid : t -> bool

(** At least one non-valid verdict. *)
val has_unknown : t -> bool

(** Same as [is_valid] for non-smoke tests. For smoke-tests,
    same as [is_unknown]. *)
val is_passed : t -> bool

(** {2 Database}
    Notice that a property or a function have no proof obligation until you
    explicitly generate them {i via} the [generate_xxx] functions below.
*)

val clear : unit -> unit
val proof : Property.t -> t list
(** List of proof obligations computed for a given property. Might be empty if you
    don't have used one of the generators below. *)

val remove : Property.t -> unit
val iter_ip : (t -> unit) -> Property.t -> unit
val iter_kf : (t -> unit) -> ?bhv:string list -> Kernel_function.t -> unit

(** {2 Generators}
    The generated VCs are also added to the database, so they can be
    accessed later. The default value for [model] is what has been
    given on the command line ([-wp-model] option)
*)

val generate_ip : ?model:string -> Property.t -> t Bag.t
val generate_kf : ?model:string -> ?bhv:string list -> ?prop:string list ->
  Kernel_function.t -> t Bag.t
val generate_call : ?model:string -> Cil_types.stmt -> t Bag.t
val generate_all : ?model:string -> ?bhv:string list -> ?prop:string list ->
  unit -> t Bag.t
(** @since 33.0-Arsenic *)

(** {2 Prover Interface} *)

val prove : t ->
  ?config:config ->
  ?mode:Prover.InteractiveMode.t ->
  ?start:(t -> unit) ->
  ?progress:(t -> string -> unit) ->
  ?result:(t -> Prover.t -> result -> unit) ->
  Prover.t -> bool Task.task
(** Returns a ready-to-schedule task. *)

val spawn : t ->
  ?config:config ->
  ?start:(t -> unit) ->
  ?progress:(t -> string -> unit) ->
  ?result:(t -> Prover.t -> result -> unit) ->
  ?success:(t -> Prover.t option -> unit) ->
  (Prover.InteractiveMode.t * Prover.t) list -> unit
(** Same as [prove] but schedule the tasks into the global server returned
    by [server] function below.

    The first succeeding prover cancels the other ones. *)

val server : ?procs:int -> unit -> Task.server
(** Default number of parallel tasks is given by [-wp-par] command-line option.
    The returned server is global to Frama-C, but the number of parallel task
    allowed will be updated to fit the [~procs] or command-line options. *)

val command :
  ?provers:Why3.Whyconf.prover list ->
  ?interactive_mode:Prover.InteractiveMode.t ->
  ?scripts:bool ->
  ?strategies:bool ->
  t Bag.t -> unit
(** Run proofs on the provided bag of WPOs.
    The defaults for the different optional variables are obtained from the
    current configuration status. That is, the command line when in CLI mode,
    or what has been configured so far by the user in the GUI mode.

    @before 33.0-Arsenic only provers and tip were configurable and the default
            were computed from the CLI.
*)
(* -------------------------------------------------------------------------- *)
