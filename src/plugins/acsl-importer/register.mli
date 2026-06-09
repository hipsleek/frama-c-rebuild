(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)


val paste_global_annot :
  ?pfile:string -> ?pline:int -> ?cfile:Filepath.t ->
  string -> Cil_types.file -> unit

val paste_fun_spec : Kernel_function.t ->
  ?pfile:string -> ?pline:int -> ?cfile:Filepath.t ->
  string -> Cil_types.file -> unit

val paste_code_annot : Kernel_function.t -> Cil_types.stmt ->
  ?pfile:string -> ?pline:int -> ?cfile:Filepath.t ->
  string -> Cil_types.file -> unit
