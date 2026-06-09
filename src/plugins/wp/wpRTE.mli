(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Invoke RTE to generate missing annotations
    for the given function and model. *)
val generate : WpContext.model -> Kernel_function.t -> unit

(** Invoke RTE on all selected functions *)
val generate_all : WpContext.model -> unit

(** Returns [true] if RTE annotations should be generated for
    the given function and model (and are not generated yet). *)
val missing_guards : WpContext.model -> Kernel_function.t -> bool
