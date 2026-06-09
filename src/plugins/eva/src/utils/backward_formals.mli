(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Functions related to the backward propagation of the value of formals
    at the end of a call. When possible, this value is propagated to
    the actual parameter. *)


val written_formals: Cil_types.kernel_function -> Cil_datatype.Varinfo.Set.t
(** [written_formals kf] is an over-approximation of the formals of [kf]
    which may be internally overwritten by [kf] during its call. *)


val safe_argument: Eva_ast.exp -> bool
(** [safe_argument e] returns [true] if [e] (which is supposed to be
    an actual parameter) is guaranteed to evaluate in the same way before and
    after the call. *)
