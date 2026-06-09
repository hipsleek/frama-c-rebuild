(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Sparecode analysis. *)
(** Interface for the unused code detection. *)

module Register: sig
  val get: select_annot:bool -> select_slice_annot:bool -> Project.t
  (** Remove in each function what isn't used to compute its outputs,
      or its annotations when [select_annot] is true,
      or its slicing annotations when [select_slice_annot] is true.
      @return a new project where the sparecode has been removed.
  *)

  val rm_unused_globals : ?new_proj_name:string -> ?project:Project.t -> unit -> Project.t
  (** Remove  unused global types and variables from the given project
      (the current one if no project given).
      The source project is not modified.
      The result is in the returned new project.
  *)

end
