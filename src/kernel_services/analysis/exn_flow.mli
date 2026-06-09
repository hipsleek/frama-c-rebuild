(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Manages information related to possible exceptions thrown by each
    function in the AST. *)

(** returns the set of exceptions that a given kernel function might throw. *)
val get_kf_exn: Kernel_function.t -> Cil_datatype.Typ.Set.t

(** computes the information if not already done. *)
val compute: unit -> unit

(**/**)
(** internal state of the module. *)
val self_fun: State.t
val self_stmt: State.t
(**/**)

(** transforms functions that may throw into functions returning a union type
    composed of the normal return or one of the exceptions. *)
val remove_exn: Cil_types.file -> unit

(** category of the code transformation above. *)
val transform_category: File.code_transformation_category
