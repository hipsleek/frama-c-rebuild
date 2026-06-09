(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** This module is used to check the assigns specification of a given function
    so that if it is not precise enough to enable precise memory models
    hypotheses computation, the assigns specification is considered incomplete.

    All these functions are memoized.
*)

val compute: Kernel_function.t -> unit

val is_complete: Kernel_function.t -> bool

val warn: Kernel_function.t -> unit
(** Displays a warning if the given kernel function has incomplete assigns.
    Note that the warning is configured with [~once] set to [true]. *)

val wkey_pedantic: Wp_parameters.warn_category
