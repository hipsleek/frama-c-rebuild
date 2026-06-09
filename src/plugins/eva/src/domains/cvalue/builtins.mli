(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Eva analysis builtins for the cvalue domain, more efficient than their
    equivalent in C. *)

(** Interface exported in Eva.ml. *)
include Builtins_sig.API

(** Prepares the builtins to be used for an analysis. Must be called at the
    beginning of each Eva analysis. Warns about builtins of incompatible types,
    builtins without an available specification and builtins overriding function
    definitions. *)
val prepare_builtins: unit -> unit

(** Is a given function replaced by a builtin? *)
val is_builtin_overridden: Cil_types.kernel_function -> bool

(** [clobbered_set_from_ret state ret] can be used for functions that return
    a pointer to where they have written some data. It returns all the bases
    of [ret] whose contents may contain local variables. *)
val clobbered_set_from_ret: Cvalue.Model.t -> Cvalue.V.t -> Base.SetLattice.t

type call = (Precise_locs.precise_location, Cvalue.V.t) Eval.call
type result = Cvalue.Model.t * Locals_scoping.clobbered_set

(** Returns the cvalue builtin for a function, if any. Also returns the name of
    the builtin and the specification of the function; the preconditions must be
    evaluated along with the builtin.
    [prepare_builtins] should have been called before using this function. *)
val find_builtin_override:
  Cil_types.kernel_function -> (string * builtin * Cil_types.funspec) option

(* Applies a cvalue builtin for the given call, in the given cvalue state. *)
val apply_builtin:
  builtin -> call -> pre:Cvalue.Model.t -> post:Cvalue.Model.t ->
  result list * cacheable
