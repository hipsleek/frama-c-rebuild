(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Dynamic allocation related builtins.
    Most functionality is exported as builtins. *)

val fold_dynamic_bases: (Base.t -> Callstack.t -> 'a -> 'a) -> 'a -> 'a
(** [fold_dynamic_bases f init] folds [f] to each dynamically allocated base,
    with initial accumulator [init].
    Note that this also includes bases created by [alloca] and [VLAs]. *)

val alloc_size_ok: Cvalue.V.t -> Alarmset.status
(* [alloc_size_ok size] checks that [size] represents a valid allocation
   size w.r.t. the total address space. [True] means that the requested size is
   small enough, [False] that the allocation is guaranteed to fail (because
   the size is always greater than SIZE_MAX). *)

val free_automatic_bases: Callstack.t -> Cvalue.Model.t -> Cvalue.Model.t
(** Performs the equivalent of [free] for each location that was allocated via
    [alloca()] in the current function (as per [Eva_utils.call_stack ()]).
    This function must be called during finalization of a function call. *)

val freeable: Cvalue.V.t -> Abstract_interp.truth
(** Evaluates the ACSL predicate \freeable(value): holds if and only if the
    value points to an allocated memory block that can be safely released using
    the C function free. Note that \freeable(\null) does not hold, despite NULL
    being a valid argument to the C function free. *)
