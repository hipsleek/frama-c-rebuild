(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

val open_source : scan_references:bool -> Filepath.t -> (string, string) result

val set_workdir : Filepath.t -> Filepath.t -> unit

val get_workdir : Filepath.t -> Filepath.t option
