(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val iter_in_order: (Kernel_function.t -> unit) -> unit
(** Iterate over all the functions, in the callgraph order, i.e. from callers
    to callees. In case of cycles (mutual recursive functions), the order is
    unspecified. *)

val iter_in_rev_order: (Kernel_function.t -> unit) -> unit
(** Iterate over all the functions, in the callgraph reverse order, i.e. from
    callees to callers. In case of cycles (mutual recursive functions), the
    order is unspecified. *)

val iter_on_callers : (Kernel_function.t -> unit) -> Kernel_function.t -> unit
(** Iterate over all the callers of a given function in a (reverse) depth-first
    way. Do nothing if the function is not in the callgraph. *)

val iter_on_callees : (Kernel_function.t -> unit) -> Kernel_function.t -> unit
(** Iterate over all the callees of a given function in a (reverse) depth-first
    way. Do nothing if the function is not in the callgraph. *)

val nb_calls: unit -> int
(** @return the number of function calls in the whole callgraph. It is not
    (necessarily) equal to the number of graph edges (depending on the
    underlying graph datastructure) *)
