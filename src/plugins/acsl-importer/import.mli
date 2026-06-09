(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Importing module. *)

val import:
  iDir:string list -> pfile:string -> init_typenames:bool ->
  Cil_types.file -> unit
(** Imports an external property file [pfile] into the given Cil AST.
    The file and included files can be searched into [~iDir] directories.
    When [init_typenames] is true, that starts by adding typenames of the
    current ast file into the [Logic_env] of the parser (this can be done
    only once). *)
