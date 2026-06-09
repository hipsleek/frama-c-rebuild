(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module computes the set of kernel functions that are considered by
    the command line options transmitted to WP. That is:

    - all functions on which a verification must be tried,
    - all functions that are called by the previous ones,
    - including those parameterized via the 'calls' clause.

    It takes in account the options -wp-bhv and -wp-props so that if all
    functions are initially selected but in fact some of them are filtered out
    by these options, they are not considered.
*)

val compute: WpContext.model ->
  ?fct:Wp_parameters.functions ->
  ?bhv:string list ->
  ?prop:string list ->
  unit ->
  unit
(** Compute the entire set, populating specification related to:
    - exits
    - terminates
    - assigns (for functions without body)
*)

val compute_kf: WpContext.model -> Kernel_function.t -> unit
(** Compute the target properties associated to the given kernel function. It
    also populates exits, terminates and assigns for the function and its
    callees, as well as RTE assertions if they are asked on command  line.

    @since 28.0-Nickel
*)

val iter: (Kernel_function.t -> unit) -> unit


val with_callees: Kernel_function.t -> Kernel_function.Set.t
(** @returns the set composed of the given kernel_function together with its
    callees. If this function does not have a definition, the empty set is
    returned.
*)
