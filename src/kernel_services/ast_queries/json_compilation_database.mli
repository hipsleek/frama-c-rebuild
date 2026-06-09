(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** [get_flags f] returns the preprocessing flags associated to file [f]
    in the JSON compilation database (when enabled), or the empty string
    otherwise. If not empty, the flags always start with a space. *)
val get_flags : Filepath.t -> string list

(** [get_dir f] returns the preprocessing directory associated to file [f]
    in the JSON compilation database.
    @since 25.0-Manganese
*)
val get_dir : Filepath.t -> Filepath.t option

(** [has_entry f] returns true iff [f] has an entry in the JSON compilation
    database. Must only be called if a JCDB file has been specified.
    @since 22.0-Titanium
*)
val has_entry : Filepath.t -> bool
