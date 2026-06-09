(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Checks if the given name is the name of a Frama-C builtin *)
val is_frama_c_builtin : Cil_types.varinfo -> bool

(** Checks if the given name is the name of one of the variadic va_* builtins *)
val is_va_builtin : string -> bool

(** Checks if a varinfo is a variadic function *)
val is_variadic_function : Cil_types.varinfo -> bool
[@@deprecated "Use Ast_types.is_variadic instead"]
[@@migrate { repl = (fun vi -> Ast_types.is_variadic vi.vtype)}]

(** Build a variadic function record for the given [varinfo] according to its
    classification. Returns [None] if the function is not variadic. *)
val classify : Environment.t -> Cil_types.varinfo ->
  Va_types.variadic_function option
