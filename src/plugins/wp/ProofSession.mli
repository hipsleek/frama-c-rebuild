(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type script =
  | NoScript
  | Script of Filepath.t
  | Deprecated of Filepath.t

val pp_file : Format.formatter -> Filepath.t -> unit
val pp_script_for : Format.formatter -> Wpo.t -> unit

val get : Wpo.t -> script
val exists : Wpo.t -> bool
val save : stdout:bool -> Wpo.t -> Json.t -> unit
val load : Wpo.t -> Json.t
val remove : Wpo.t -> unit

val filename : force:bool -> Wpo.t -> Filepath.t

val mark : Wpo.t -> unit
val reset_marks : unit -> unit
val remove_unmarked_files : dry:bool -> unit

(**************************************************************************)
