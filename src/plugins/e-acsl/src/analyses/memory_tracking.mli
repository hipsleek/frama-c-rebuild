(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** Compute a sound over-approximation of what left-values must be tracked by
    the memory model library *)

module SpecialPointers : sig
  type spec = {pointer : bool; freeable : bool; writeable : bool; initialized : bool}
  val initialize : unit -> unit

  (* is this one of stdin/stdout/stderr/&errno? If so, return spec *)
  val pointer_of_term : term -> spec option
end

val reset: unit -> unit
(** Must be called to redo the analysis *)

val use_monitoring: unit -> bool
(** Is one variable monitored (at least)? *)

val must_monitor_vi: ?kf:kernel_function -> ?stmt:stmt -> varinfo -> bool
(** [must_monitor_vi ?kf ?stmt vi] returns [true] if the varinfo [vi] at the given
    [stmt] in the given function [kf] must be tracked by the memory model
    library. *)

val must_monitor_lval: ?kf:kernel_function -> ?stmt:stmt -> lval -> bool
(** Same as {!must_monitor_vi}, for left-values *)

val must_monitor_exp: ?kf:kernel_function -> ?stmt:stmt -> exp -> bool
(** Same as {!must_monitor_vi}, for expressions *)

val found_concurrent_function: loc:location -> varinfo -> unit
(** [found_concurrent_function ~loc fvi] signals to the memory tracking
    sub-system that a concurrent function [fvi] has been found at [loc] while
    parsing the sources. This function needs only to be called if the
    concurrency support of E-ACSL is deactivated.

    If the memory monitoring is in use when a concurrent function is found,
    abort the parsing: the user needs to activate the concurrency support.

    Otherwise store the information of the first concurrent function found.
    Later if the memory monitoring is used because of memory properties to
    verify, then abort the parsing: the user needs to activate the concurrency
    support.

    In summary, an analyzed source code can be concurrent with the concurrency
    support of E-ACSL deactivated only if no memory properties are to be
    verified. *)
