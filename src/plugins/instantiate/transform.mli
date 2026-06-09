(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Module for AST transformation *)

(** Registers a new [Instantiator] to the visitor from the [Generator_sig]
    module. Each new instantiator generator should call this globally.
*)
val register: (module Instantiator_builder.Generator_sig) -> unit

(** In all selected functions of the given file, for all function call, if there
    exists a instantiator module for this function, and the call is well-typed,
    replaces it with a call to the generated override function and inserted the
    generated function in the AST.
*)
val transform: Cil_types.file -> unit
